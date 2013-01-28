(* Be sure and first read the implementation overview in timing_wheel.mli.

   A timing wheel is represented as an array of "levels", where each level is an array of
   "slots".  Each slot represents a range of keys, and holds the elements associated with
   those keys.  Each level is determined by two parameters: [bits], the number of key bits
   that that level is responsible for distinguishing, and [bits_per_slot], the size of the
   range of keys that correspond to a single slot in the array.  Conceptually, each level
   breaks up all possible keys into ranges of size 2^bits_per_slot.  The length of a level
   array is [2^bits], and the array is used like a circular buffer to traverse the ranges
   as the timing wheel's [min_allowed_key] increases.  A key [k], if stored in the level,
   is stored at index [(k / 2^bits_per_slot) mod 2^bits].

   The settings of the [bits] values are configurable by user code, although there is a
   reasonable default setting.  Given the [bits] values, the [bits_per_slot] are chosen so
   that [bits_per_slot] at level [i] is the sum of the [bits] at all lower levels.
   Thus, a slot's range at level [i] is as large as the entire array's range at level
   [i - 1].

   Each level has a [min_allowed_key] and a [max_allowed_key] that determine the range of
   keys that it currently represents.  The crucial invariant of the timing wheel data
   structure is that the [min_allowed_key] at level [i] is no more than the
   [max_allowed_key + 1] of level [i + 1].  This ensures that the levels can represent all
   keys from the [min_allowed_key] of the lowest level to the [max_allowed_key] of the
   highest level.  The [shift_levels] function is responsible for restoring this invariant
   when the min_allowed_key for the timing wheel is increased.

   The idea of the implementation is similar to the hierarchical approach described in:

   | Hashed and Hierarchical Timing Wheels:
   | Efficient Data Structures for Implementing a Timer Facility
   |
   | Varghese & Lauck, 1996

   However, the code is completely new.
*)

open Core.Std

let debug = ref false
let check_invariant = ref false

let error = Or_error.error
let ok_exn = Or_error.ok_exn
let raise = Error.raise

let log msg a sexp_of_a =
  eprintf "%s\n%!" (Sexp.to_string_hum (Info.sexp_of_t (Info.create msg a sexp_of_a)));
;;

module Priority_queue = struct
  let max_representable_key = Int.max_value lsr 1

  let max_level_bits = Int.num_bits - 2

  module Elt_ = struct
    type ('a, 'timing_wheel) t =
      { key : int;
        (* [at] is used when the priority queue is used to implement a timing wheel.  If
           unused, it will be [Time.epoch]. *)
        at : Time.t;
        value : 'a;
        (* [level_index >= 0] iff the element is in a timing wheel. *)
        mutable level_index : int;
        (* The elements in a slot are circularly doubly-linked, in order to support
           constant-time removal. *)
        mutable prev : ('a, 'timing_wheel) t sexp_opaque;
        mutable next : ('a, 'timing_wheel) t sexp_opaque;
        (* Each element has a pointer to the timing wheel it is in, so that we can implement
           [contains] in constant time. *)
        timing_wheel : 'timing_wheel;
      }
    with fields, sexp_of

    let unlink t =
      t.prev.next <- t.next;
      t.next.prev <- t.prev;
    ;;

    let fold start ~init:ac ~f =
      let rec loop t ac =
        let t = t.next in
        let ac = f ac t in
        if phys_equal t start then ac else loop t ac
      in
      loop start ac
    ;;

    let iter t ~f = fold t ~init:() ~f:(fun () t -> f t)

    let invariant value_invariant timing_wheel_invariant t =
      iter t ~f:(fun t ->
        try
          let check f field = f (Field.get field t) in
          Fields.iter
            ~key:ignore
            ~at:ignore
            ~value:(check value_invariant)
            ~level_index:ignore
            ~prev:(check (fun prev -> assert (phys_equal t prev.next)))
            ~next:(check (fun next -> assert (phys_equal t next.prev)))
            ~timing_wheel:(check timing_wheel_invariant)
          ;
        with exn ->
          failwiths "elt_invariant failed" (exn, t)
            (<:sexp_of< exn * (_, _) t >>))
    ;;

    let length t =
      let n = fold t ~init:0 ~f:(fun n _ -> n + 1) in
      if !debug then log "Elt.length" (n, t) (<:sexp_of< int * (_, _) t >>);
      n;
    ;;

  end

  module Level = struct
    module Elt = Elt_

    type ('a, 'timing_wheel) t =
      { index : int;
        bits : int;
        mask : int;
        bits_per_slot : int;
        keys_per_slot : int;
        mutable length : int;
        mutable min_allowed_key : int;
        mutable max_allowed_key : int;
        dummy_elt : ('a, 'timing_wheel) Elt.t;
        slots : ('a, 'timing_wheel) Elt.t array sexp_opaque;
      }
    with fields, sexp_of

    let num_allowed_keys t = t.max_allowed_key - t.min_allowed_key + 1

    let num_slots t = Array.length t.slots

    let compute_max_allowed_key t = t.min_allowed_key + t.keys_per_slot * num_slots t - 1

    let is_dummy_elt t elt = phys_equal elt t.dummy_elt

    let invariant (type a) (type timing_wheel)
        (a_invariant : a Invariant.t)
        (timing_wheel_invariant : timing_wheel Invariant.t)
        (t : (a, timing_wheel) t)
        : unit =
      try
        let check f field = f (Field.get field t) in
        Fields.iter
          ~index:(check (fun index -> assert (index >= 0)))
          ~bits:(check (fun bits -> assert (bits > 0)))
          ~mask:(check (fun mask -> assert (mask = num_slots t - 1)))
          ~bits_per_slot:(check (fun bits_per_slot -> assert (bits_per_slot >= 0)))
          ~keys_per_slot:(check (fun keys_per_slot ->
            assert (keys_per_slot = 1 lsl t.bits_per_slot)))
          ~length:(check (fun length ->
            assert (length
                    = Array.fold t.slots ~init:0 ~f:(fun n elt ->
                      if is_dummy_elt t elt then
                        n
                      else
                        n + Elt.length elt))))
          ~min_allowed_key:(check (fun min_allowed_key ->
            assert (min_allowed_key >= 0);
            assert (Int.rem min_allowed_key t.keys_per_slot = 0)))
          ~max_allowed_key:(check (fun max_allowed_key ->
            assert (max_allowed_key = compute_max_allowed_key t)))
          ~dummy_elt:ignore
          ~slots:(check (fun slots ->
            Array.iter slots ~f:(fun elt ->
              if not (is_dummy_elt t elt) then begin
                Elt.invariant a_invariant timing_wheel_invariant elt;
                Elt.iter elt ~f:(fun elt ->
                  assert (elt.Elt.key >= t.min_allowed_key);
                  assert (elt.Elt.key <= t.max_allowed_key);
                  assert (elt.Elt.level_index = t.index))
              end)))
        ;
      with exn ->
        failwiths "Level.invariant failed" (exn, t)
          (<:sexp_of< exn * (_, _) t >>)
    ;;

    let create_exn ~index ~bits ~bits_per_slot ~dummy_elt =
      if bits <= 0 || bits_per_slot < 0 then
        failwiths "Level.create got invalid argument(s)"
          (bits, bits_per_slot) <:sexp_of< int * int >>;
      let num_slots = 1 lsl bits in
      let keys_per_slot = 1 lsl bits_per_slot in
      { index;
        bits;
        mask = num_slots - 1;
        bits_per_slot;
        keys_per_slot;
        length = 0;
        min_allowed_key = 0;
        max_allowed_key = keys_per_slot * num_slots - 1;
        dummy_elt;
        slots = Array.create ~len:num_slots dummy_elt;
      }
    ;;

    let slot t ~key = (key lsr t.bits_per_slot) land t.mask

    let iter_elt t ~f =
      Array.iter t.slots ~f:(fun elt ->
        if not (is_dummy_elt t elt) then Elt.iter elt ~f)
    ;;

    let inc_slot t slot = (slot + 1) land t.mask

    let add_elt t elt =
      let key = elt.Elt.key in
      assert (key >= t.min_allowed_key && key <= t.max_allowed_key);
      t.length <- t.length + 1;
      elt.Elt.level_index <- t.index;
      let slot = slot t ~key:elt.Elt.key in
      let first = t.slots.(slot) in
      if is_dummy_elt t first then begin
        t.slots.(slot) <- elt;
        elt.Elt.next <- elt;
        elt.Elt.prev <- elt;
      end else begin
        elt.Elt.prev <- first;
        elt.Elt.next <- first.Elt.next;
        first.Elt.next <- elt;
        elt.Elt.next.Elt.prev <- elt;
      end;
    ;;

    let remove_elt t elt =
      t.length <- t.length - 1;
      elt.Elt_.level_index <- -1;
      let slots = t.slots in
      let slot = slot t ~key:elt.Elt.key in
      let first = slots.(slot) in
      if not (phys_equal elt first) then
        Elt.unlink elt
      else begin
        if phys_equal elt elt.Elt.next then
          (* [elt] is the only element in the slot *)
          slots.(slot) <- t.dummy_elt
        else begin
          slots.(slot) <- elt.Elt.next;
          Elt.unlink elt;
        end;
      end;
    ;;

    (* [increase_min_allowed_key t ~upper_bound handle_removed] increases the min-allowed
       key in [t] to as large as a value as possible that is less than or equal to
       [upper_bound].  It applies [handle_removed] to each element that is removed. *)
    let increase_min_allowed_key t ~upper_bound ~handle_removed =
      if !debug then
        log "Level.increase_min_allowed_key" (upper_bound, t)
          (<:sexp_of< int * (_, _) t >>);
      let slot = ref (slot t ~key:t.min_allowed_key) in
      while t.min_allowed_key + t.keys_per_slot <= upper_bound do
        if t.length = 0 then
          (* If there are no elements at this level, we can just set min_allowed_key to the
             desired value. *)
          t.min_allowed_key <- t.keys_per_slot * (upper_bound / t.keys_per_slot)
        else begin
          let elt = t.slots.(!slot) in
          if not (is_dummy_elt t elt) then begin
            t.slots.(!slot) <- t.dummy_elt;
            let rec loop elt' =
              (* We extract [next] from [elt'] before unlinking it. *)
              let next = elt'.Elt_.next in
              t.length <- t.length - 1;
              elt'.Elt_.level_index <- -1;
              elt'.Elt_.prev <- t.dummy_elt;
              elt'.Elt_.next <- t.dummy_elt;
              handle_removed elt';
              if not (phys_equal next elt) then loop next;
            in
            loop elt
          end;
          slot := inc_slot t !slot;
          t.min_allowed_key <- t.min_allowed_key + t.keys_per_slot;
        end
      done;
      assert (t.min_allowed_key <= upper_bound);
      t.max_allowed_key <- compute_max_allowed_key t;
    ;;

    let min_elt_smaller_than t min_elt_already_found =
      if t.length = 0 then
        min_elt_already_found
      else begin
        let rec loop slot slot_min_allowed_key =
          let continue =
            match min_elt_already_found with
            | None -> true
            | Some elt -> slot_min_allowed_key < elt.Elt.key
          in
          if not continue then
            min_elt_already_found
          else begin
            let elt = t.slots.(slot) in
            if is_dummy_elt t elt then
              loop (inc_slot t slot) (slot_min_allowed_key + t.keys_per_slot)
            else begin
              Elt.fold elt ~init:min_elt_already_found ~f:(fun min_elt_already_found elt ->
                match min_elt_already_found with
                | None -> Some elt
                | Some elt' ->
                  if elt.Elt.key < elt'.Elt.key
                  then Some elt
                  else min_elt_already_found)
            end
          end
        in
        loop (slot t ~key:t.min_allowed_key) t.min_allowed_key
      end
    ;;

  end

  type 'a t =
    { mutable length : int;
      dummy_elt : 'a elt;
      levels : ('a, 'a t sexp_opaque) Level.t array;
    }
  and 'a elt = ('a, 'a t sexp_opaque) Elt_.t
    with fields, sexp_of

  type 'a timing_wheel = 'a t with sexp_of

  module Elt = struct
    type 'a t = 'a elt with sexp_of

    let key   = Elt_.key
    let at    = Elt_.at
    let value = Elt_.value

    let invariant value_invariant t = value_invariant (value t)
  end

  let is_empty t = length t = 0

  let num_levels t = Array.length t.levels

  let min_allowed_key t = t.levels.(0).Level.min_allowed_key

  let max_allowed_key t =
    min max_representable_key t.levels.(num_levels t - 1).Level.max_allowed_key
  ;;

  let invariant (type a) (a_invariant : a -> unit) (t : a t) : unit =
    try
      assert (min_allowed_key t <= max_representable_key);
      assert (max_allowed_key t <= max_representable_key);
      let check f field = f (Field.get field t) in
      Fields.iter
        ~length:(check (fun length -> assert (length >= 0)))
        ~dummy_elt:ignore
        ~levels:(check (fun (levels : (a, a t) Level.t array) ->
          assert (num_levels t > 0);
          Array.iteri levels ~f:(fun level_index (level : (a, a t) Level.t) ->
            Level.invariant a_invariant (fun t' -> assert (phys_equal t t')) level;
            if level_index > 0 then begin
              let prev_level = levels.(level_index - 1) in
              let module L = Level in
              assert (level.L.keys_per_slot = Level.num_allowed_keys prev_level);
              let bound = prev_level.L.max_allowed_key + 1 in
              assert (level.L.min_allowed_key <= bound);
              assert (not (level.L.min_allowed_key + level.L.keys_per_slot <= bound));
            end
          )))
      ;
    with exn ->
      failwiths "invariant failed" (exn, t) (<:sexp_of< exn * _ t >>)
  ;;

  let min_elt t =
    if length t = 0 then
      None
    else begin
      let rec loop level_index min_elt_already_found =
        if !debug then
          log "min_elt.loop" (level_index, min_elt_already_found)
            (<:sexp_of< int * _ Elt.t option >>);
        if level_index = num_levels t then
          min_elt_already_found
        else begin
          let level = t.levels.(level_index) in
          let continue =
            match min_elt_already_found with
            | None -> true
            | Some elt -> level.Level.min_allowed_key < elt.Elt_.key
          in
          if not continue then
            min_elt_already_found
          else
            loop (level_index + 1) (Level.min_elt_smaller_than level min_elt_already_found)
        end
      in
      loop 0 None
    end
  ;;

  let min_elt t =
    if !debug then log "min_elt" t <:sexp_of< _ t >>;
    if !check_invariant then invariant ignore t;
    let result = min_elt t in
    if !debug then log "min_elt result" result <:sexp_of< _ Elt.t option >>;
    result
  ;;

  let min_key t = Option.map (min_elt t) ~f:Elt.key

  let add_elt t elt =
    if !debug then log "add_elt" (elt, t) <:sexp_of< _ Elt.t * _ t >>;
    let key = elt.Elt_.key in
    assert (key >= min_allowed_key t);
    let n = num_levels t in
    let rec loop level_index =
      if level_index = n then
        failwiths "add_elt got key too large" (key, t)
          (<:sexp_of< int * _ t >>)
      else begin
        let level = t.levels.(level_index) in
        assert (key >= level.Level.min_allowed_key);
        if key <= Level.max_allowed_key level then
          Level.add_elt level elt
        else
          loop (level_index + 1)
      end
    in
    loop 0;
  ;;

  let internal_add t ~key ~at value =
    if !debug then log "add" (key, t) <:sexp_of< int * _ t >>;
    if !check_invariant then invariant ignore t;
    if key < min_allowed_key t then
      Ok `Key_too_small
    else if key > max_allowed_key t then
      error "add got invalid key" (key, t) (<:sexp_of< int * _ t >>)
    else begin
      let elt =
        { Elt_.
          key;
          at;
          value;
          level_index = -1;
          next = t.dummy_elt;
          prev = t.dummy_elt;
          timing_wheel = t;
        }
      in
      add_elt t elt;
      t.length <- t.length + 1;
      Ok (`Added elt)
    end
  ;;

  let add t ~key value = internal_add t ~key ~at:Time.epoch value

  let shift_levels t ~min_allowed_key ~force ~handle_removed =
    let handle_removed elt =
      if elt.Elt_.key >= min_allowed_key then
        add_elt t elt
      else begin
        t.length <- t.length - 1;
        handle_removed elt;
      end
    in
    let rec loop level_index upper_bound =
      if level_index < num_levels t then begin
        let level = t.levels.(level_index) in
        let prev_min_allowed_key = Level.min_allowed_key level in
        Level.increase_min_allowed_key level ~upper_bound ~handle_removed;
        if force || Level.min_allowed_key level > prev_min_allowed_key then
          loop (level_index + 1) (level.Level.max_allowed_key + 1);
      end
    in
    loop 0 min_allowed_key;
  ;;

  let increase_min_allowed_key t ~key ~handle_removed =
    if !debug then
      log "increase_min_allowed_key" (key, t) <:sexp_of< int * _ t >>;
    if !check_invariant then invariant ignore t;
    if key <= min_allowed_key t then
      Ok ()
    else if key > max_representable_key then
      error "increase_min_allowed_key got invalid key" (key, t) (<:sexp_of< int * _ t >>)
    else begin
      shift_levels t ~min_allowed_key:key ~force:false ~handle_removed;
      Ok ()
    end;
  ;;

  let create ?(level_bits = [11; 10; 10; 10; 10; 10]) ~dummy () =
    if List.is_empty level_bits
      || List.exists level_bits ~f:(fun bits -> bits <= 0)
    then
      error "create got invalid level_bits" level_bits (<:sexp_of< int list >>)
    else begin
      let rec dummy_elt =
        { Elt_.
          key = -1;
          at = Time.epoch;
          value = dummy;
          level_index = -1;
          next = dummy_elt;
          prev = dummy_elt;
          timing_wheel = { length = -1; dummy_elt; levels = [||] };
        }
      in
      let (bits, levels) =
        List.foldi level_bits ~init:(0, [])
          ~f:(fun index (bits_per_slot, levels) bits ->
            (bits + bits_per_slot,
             Level.create_exn ~index ~bits ~bits_per_slot ~dummy_elt :: levels))
      in
      if bits > max_level_bits then
        error "create ~level_bits has too many bits"
          (`got bits, `max max_level_bits, level_bits)
          (<:sexp_of< [`got of int] * [`max of int] * int list >>)
      else begin
        let t = { length = 0; dummy_elt; levels = Array.of_list_rev levels } in
        shift_levels t ~min_allowed_key:0 ~force:true
          ~handle_removed:(fun _ -> assert false);
        if !check_invariant then invariant ignore t;
        Ok t;
      end
    end
  ;;

  let contains t elt = elt.Elt_.level_index >= 0 && phys_equal t elt.Elt_.timing_wheel

  let remove t elt =
    if !debug then log "remove" (elt, t) <:sexp_of< _ Elt.t * _ t >>;
    if !check_invariant then invariant ignore t;
    if not (contains t elt) then
      error "remove of element that it doesn't contain" (elt, t)
        (<:sexp_of< _ Elt.t * _ t >>)
    else begin
      t.length <- t.length - 1;
      Level.remove_elt t.levels.(elt.Elt_.level_index) elt;
      Result.ok_unit;
    end
  ;;

  let iter_elt t ~f = Array.iter t.levels ~f:(fun level -> Level.iter_elt level ~f)

  let iter t ~f = iter_elt t ~f:(fun elt -> f (Elt.value elt))

  TEST_MODULE = struct
    let _ = check_invariant := true
    let _ = debug := false

    let increase_min_allowed_key t ~key =
      let r = ref [] in
      let handle_removed elt = r := elt :: !r in
      Result.map (increase_min_allowed_key t ~key ~handle_removed) ~f:(fun () -> !r)

    let create ~level_bits = create ~level_bits ~dummy:() ()

    (* [create] errors *)
    TEST_UNIT =
      assert (Result.is_error (create ~level_bits:[]));
      assert (Result.is_error (create ~level_bits:[-1]));
      assert (Result.is_error (create ~level_bits:[1; -1]));
    ;;

    let add_exn t ~key value =
      match ok_exn (add t ~key value) with
      | `Added e -> e
      | `Key_too_small -> failwiths "key too small" (key, t) (<:sexp_of< int * _ t >>)
    ;;

    (* [add], [contains], and [remove] *)
    TEST_UNIT =
      let t1 = ok_exn (create ~level_bits:[1]) in
      let t2 = ok_exn (create ~level_bits:[1]) in
      let e1 = add_exn t1 ~key:0 () in
      let e2 = add_exn t2 ~key:0 () in
      assert (contains t1 e1);
      assert (not (contains t1 e2));
      assert (not (contains t2 e1));
      assert (contains t2 e2);
      assert (Result.is_ok (remove t1 e1));
      assert (not (contains t1 e1));
    ;;

    (* [add] errors *)
    TEST_UNIT =
      let t = ok_exn (create ~level_bits:[1]) in
      for key = min_allowed_key t to max_allowed_key t do
        assert (Result.is_ok (add t ~key ()));
      done;
      assert (add t ~key:(min_allowed_key t - 1) () = Ok `Key_too_small);
      assert (Result.is_error (add t ~key:(max_allowed_key t + 1) ()));
    ;;

    (* [max_representable_key] *)
    TEST_UNIT =
      let t = ok_exn (create ~level_bits:[1]) in
      assert (Result.is_error
                (increase_min_allowed_key t ~key:(max_representable_key + 1)));
      assert (Result.is_ok (increase_min_allowed_key t ~key:max_representable_key));
      assert (min_allowed_key t = max_representable_key);
      assert (max_allowed_key t = max_representable_key);
      let e1 = add_exn t ~key:max_representable_key () in
      assert (contains t e1);
      assert (length t = 1);
    ;;

    (* [increase_min_allowed_key] *)
    TEST_UNIT =
      let all_sums n =
        let results = Array.create ~len:(n + 1) [] in
        results.(0) <- [[]];
        for i = 1 to n do
          results.(i) <-
            List.concat
            (List.init i ~f:(fun j ->
              let first = j + 1 in
              List.map results.(i - first) ~f:(fun rest -> first :: rest)));
        done;
        results.(n)
      in
      let test ~num_bits ~level_bits ~initial_min_allowed_key ~step =
        if false then
          log "test" (`num_bits num_bits, `level_bits level_bits,
                      `initial_min_allowed_key initial_min_allowed_key,
                      `step step)
            (<:sexp_of< ([`num_bits of int] * [`level_bits of int list]
                         * [`initial_min_allowed_key of int]
                         * [`step of int]) >>);
        let t = ok_exn (create ~level_bits) in
        assert (Result.is_ok (increase_min_allowed_key t ~key:initial_min_allowed_key));
        assert (min_allowed_key t = initial_min_allowed_key);
        assert (max_allowed_key t >= min_allowed_key t + 1 lsl num_bits - 1);
        let keys =
          List.range
            ~start:`inclusive (min_allowed_key t)
            ~stop: `inclusive (max_allowed_key t - 1)
        in
        let n = ref 0 in
        List.iter keys ~f:(fun key ->
          ignore (add_exn t ~key () : _ Elt.t);
          incr n;
          assert (length t = !n));
        let elts_removed = ref [] in
        while length t > 0 do
          match
            increase_min_allowed_key t ~key:(min max_representable_key
                                               (min_allowed_key t + step))
          with
          | Ok elts ->
            elts_removed := elts @ !elts_removed;
            List.iter elts ~f:(fun elt ->
              assert (Elt.key elt < min_allowed_key t))
          | _ -> assert false
        done;
        let elts_removed =
          List.sort !elts_removed ~cmp:(fun e e' -> Int.compare (Elt.key e) (Elt.key e'))
          |! List.map ~f:Elt.key
        in
        assert (elts_removed = keys);
      in
      let num_bits = 6 in
      let all_sums = all_sums num_bits in
      List.iter
        [ 0;
          max_representable_key - (1 lsl num_bits);
        ]
        ~f:(fun initial_min_allowed_key ->
          for step = 1 to 1 lsl num_bits do
            List.iter all_sums ~f:(fun level_bits ->
              test ~num_bits ~level_bits ~initial_min_allowed_key ~step)
          done);
    ;;

    (* [min_elt] *)
    TEST_UNIT =
      let t = ok_exn (create ~level_bits:[1; 1; 1; 1]) in
      assert (is_none (min_key t));
      ignore (add_exn t ~key:0 () : unit Elt.t);
      assert (min_key t = Some 0);
      let max_key = 10 in
      for key = 1 to max_key; do
        assert (Result.is_ok (add t ~key ()));
        assert (min_key t = Some 0);
      done;
      for key = 1 to max_key + 1; do
        begin match ok_exn (increase_min_allowed_key t ~key) with
        | [elt] -> assert (Elt.key elt = key - 1);
        | _ -> assert false
        end;
        assert (min_key t = if key <= max_key then Some key else None);
      done;
    ;;

    (* [min_elt] *)
    TEST_UNIT =
      let t = ok_exn (create ~level_bits:[1; 1; 1; 1]) in
      let max_key = 10 in
      let elts = List.init (max_key + 1) ~f:(fun key -> add_exn t ~key ()) in
      List.iter elts ~f:(fun elt ->
        ok_exn (remove t elt);
        let key = Elt.key elt in
        assert (min_key t = if key < max_key then Some (key + 1) else None));
    ;;

    (* [iter] *)
    TEST_UNIT =
      let t = ok_exn (create ~level_bits:[1; 1; 1; 1]) in
      let count () =
        let r = ref 0 in
        iter t ~f:(fun _ -> incr r);
        !r
      in
      assert (count () = 0);
      let num_elts = 10 in
      for key = 0 to num_elts - 1; do
        ignore (add_exn t ~key () : unit Elt.t);
      done;
      assert (count () = num_elts);
      assert (Result.is_ok (increase_min_allowed_key t ~key:1));
      assert (count () = num_elts - 1);
      assert (Result.is_ok (increase_min_allowed_key t ~key:num_elts));
      assert (count () = 0);
    ;;

  end

end

module Alarm = Priority_queue.Elt

type 'a t =
  { start : Time.t;
    mutable now : Time.t;
    alarm_precision : Time.Span.t;
    priority_queue : 'a Priority_queue.t;
  }
with fields, sexp_of

let length t = Priority_queue.length t.priority_queue

let is_empty t = length t = 0

let iter_alarms t ~f = Priority_queue.iter_elt t.priority_queue ~f

let key_time t key =
  Time.add t.start (Time.Span.scale t.alarm_precision (Float.of_int key))
;;

let max_alarm_at t = key_time t (Priority_queue.max_allowed_key t.priority_queue + 1)

let time_key t time =
  if Time.(<) time t.start then
    error "time before clock start" (time, t.start) (<:sexp_of< Time.t * Time.t >>)
  else begin
    let float = Time.Span.(//) (Time.diff time t.start) t.alarm_precision in
    match Float.iround ~dir:`Down float with
    | Some key -> Ok key
    | None ->
      error "time too far in the future" (time, float, t)
        (<:sexp_of< Time.t * float * _ t >>)
  end
;;

let interval_start t time = Result.map (time_key t time) ~f:(fun key -> key_time t key)

let invariant a_invariant t =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~start:ignore
      ~now:(check (fun now ->
        assert (Time.(>=) now t.start);
        begin match time_key t t.now with
        | Error e -> raise e
        | Ok now_key -> assert (now_key = Priority_queue.min_allowed_key t.priority_queue)
        end))
      ~alarm_precision:(check (fun alarm_precision ->
        assert (Time.Span.(>) alarm_precision Time.Span.zero)))
      ~priority_queue:(check (Priority_queue.invariant a_invariant));
    iter_alarms t ~f:(fun alarm ->
      assert (Alarm.key alarm = ok_exn (time_key t (Alarm.at alarm))));
  with exn ->
    failwiths "Timing_wheel.invariant failed" (exn, t) (<:sexp_of< exn * _ t >>)
;;

let create ?level_bits ~start ~alarm_precision ~dummy () =
  if Time.Span.(<=) alarm_precision Time.Span.zero then
    error "Timing_wheel.create got nonpositive alarm_precision" alarm_precision
      (<:sexp_of< Time.Span.t >>)
  else
    Or_error.map (Priority_queue.create ?level_bits ~dummy ()) ~f:(fun priority_queue ->
      let t = { start; now = start; alarm_precision; priority_queue } in
      if !check_invariant then invariant ignore t;
      t)
;;

let advance_clock t ~to_ ~handle_fired =
  if !debug then log "advance_clock" (to_, t) <:sexp_of< Time.t * _ t >>;
  let result =
    if Time.(<=) to_ (now t) then
      Ok ()
    else
      match time_key t to_ with
      | Error _ as e -> e
      | Ok key ->
        t.now <- to_;
        Priority_queue.increase_min_allowed_key t.priority_queue ~key
          ~handle_removed:(fun alarm -> handle_fired (Alarm.value alarm));
  in
  if !check_invariant then invariant ignore t;
  result
;;

let add_alarm t ~at value =
  let result =
    if Time.(<=) at (now t) then
      Ok `Not_in_the_future
    else
      match time_key t at with
      | Error _ as e -> e
      | Ok key ->
        match Priority_queue.internal_add t.priority_queue ~key ~at value with
        | Error _ | Ok (`Added _) as x -> x
        | Ok `Key_too_small ->
        (* This should be impossible, because we ensured [at > now t] above. *)
          assert false
  in
  if !check_invariant then invariant ignore t;
  result
;;

let add_alarm_exn t ~at value =
  match add_alarm t ~at value with
  | Ok (`Added a) -> a
  | Error e -> raise e
  | Ok `Not_in_the_future ->
    failwiths "Timing_wheel.add_alarm not in the future" (at, t)
      (<:sexp_of< Time.t * _ t >>)
;;

let remove_alarm t alarm =
  let result = Priority_queue.remove t.priority_queue alarm in
  if !check_invariant then invariant ignore t;
  result
;;

let next_alarm_at t =
  Option.map (Priority_queue.min_elt t.priority_queue) ~f:Alarm.at
;;

module Pretty_alarm = struct
  type 'a t =
    { at : Time.t;
      value : 'a;
    }
  with fields, sexp_of

  let create alarm = { at = Alarm.at alarm; value = Alarm.value alarm }

  let compare t1 t2 = Time.compare (at t1) (at t2)
end

module Pretty = struct
  type 'a t =
    { now : Time.t;
      alarm_precision : Time.Span.t;
      alarms : 'a Pretty_alarm.t list;
    }
  with sexp_of
end

let pretty t =
  let r = ref [] in
  iter_alarms t ~f:(fun a -> r := Pretty_alarm.create a :: !r);
  let alarms = List.sort !r ~cmp:Pretty_alarm.compare in
  { Pretty.
    now = now t;
    alarm_precision = alarm_precision t;
    alarms;
  }
;;

let sexp_of_t sexp_of_a t = <:sexp_of< a Pretty.t >> (pretty t)

TEST_MODULE = struct
  let () = debug := false
  let () = check_invariant := true

  (* No early alarms *)
  TEST_UNIT =
    let test ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
      log "test" (num_alarms, alarm_precision, alarm_separation, advance_by)
        (<:sexp_of< int * Time.Span.t * Time.Span.t * Time.Span.t >>);
      let t =
        ok_exn (create ~start:Time.epoch ~alarm_precision ~dummy:ignore ())
      in
      for i = 1 to num_alarms do
        let at = Time.add (now t) (Time.Span.scale alarm_separation (Float.of_int i)) in
        ignore (add_alarm_exn t ~at (fun () -> assert (Time.(<=) at (now t))))
      done;
      while not (is_empty t) do
        ok_exn (advance_clock t ~to_:(Time.add (now t) advance_by)
                  ~handle_fired:(fun f -> f ()))
      done;
    in
    List.iter [ 100 ] ~f:(fun num_alarms ->
      List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
        let alarm_precision = sec s in
        List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
          let alarm_separation = sec s in
          List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
            let advance_by = sec s in
            test ~num_alarms ~alarm_precision ~alarm_separation ~advance_by))));
  ;;

  TEST_UNIT =
    let t =
      ok_exn (create ~start:Time.epoch ~alarm_precision:(sec 1.) ~dummy:ignore
                ~level_bits:[10]
                ())
    in
    let num_alarms () =
      let r = ref 0 in
      iter_alarms t ~f:(fun _ -> incr r);
      !r
    in
    let add_alarm ~after f = add_alarm t ~at:(Time.add (now t) after) f in
    let add_alarm_ok ~after f =
      match add_alarm ~after f with
      | Ok (`Added _) -> true
      | _ -> false
    in
    let advance_clock by =
      ok_exn (advance_clock t ~to_:(Time.add (now t) by) ~handle_fired:(fun f -> f ()))
    in
    begin match add_alarm ~after:(sec (-1.)) ignore with
    | Ok `Not_in_the_future -> ()
    | _ -> assert false
    end;
    begin match add_alarm ~after:(sec 0.) ignore with
    | Ok `Not_in_the_future -> ()
    | _ -> assert false
    end;
    assert (max_alarm_at t = Time.add (now t) (sec 1024.));
    assert (num_alarms () = 0);
    assert (is_none (next_alarm_at t));
    let fired = ref false in
    assert (add_alarm_ok ~after:(sec 30.) (fun () -> fired := true));
    assert (next_alarm_at t = Some (Time.add (now t) (sec 30.)));
    assert (num_alarms () = 1);
    advance_clock (sec 30.);
    assert (not !fired);
    advance_clock (sec 1.);
    assert !fired;
  ;;
end
