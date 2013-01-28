open Core.Std

type completer = (left:string -> right:string -> string list)

let registered = ref false

let cleanupFun = ref None

(**
   Finally is always run, even if we press [ctrl + c]

   This is used because we HAVE to restore the terminal when we exit otherwise
   we will face the wrath of very angry users!!!

   This function cannot be used in nested calls.
*)
let unwind_protect ~f ~finally =
  if not !registered then begin
    at_exit (fun () -> Option.call ~f:!cleanupFun ());
    registered := true
  end;
  let finally () =
    cleanupFun := None;
    finally ()
  in
  cleanupFun := Some finally;
  let res=
    try
      f ()
    with e ->
      finally ();
      raise e
  in
  finally ();
  res

let with_readline f =
  let module T = Unix.Terminal_io in
  let attr_in = T.tcgetattr Unix.stdin
  and attr_out = T.tcgetattr Unix.stdout in
  unwind_protect
    ~f:(
      fun () ->
        let attr_in = {
          attr_in with T.
          c_echo = false;
          c_icanon = false;
          c_vmin = 1;
          c_ixon = false;
        }
        and attr_out = { attr_out with T.c_icanon = false; c_vmin = 1 }
        in
        T.tcsetattr attr_out ~mode:T.TCSAFLUSH Unix.stdout;
        T.tcsetattr attr_in ~mode:T.TCSADRAIN Unix.stdin;
        f ()
    )
    ~finally:(fun () ->
                T.tcsetattr attr_out ~mode:T.TCSAFLUSH Unix.stdout;
                T.tcsetattr attr_in ~mode:T.TCSADRAIN Unix.stdin;
             )

let interactive_readline ~prompt =
  print_string (prompt);
  let res =
    try
      Some (Shell.run_one "zenity" ["--entry";
                                    "--text="^prompt;
                                    "--title=\"Toploop readline!!\""])
    with _ -> None
  in
  Option.iter res ~f:print_endline;
  res

let mainloop ?text ~map_out ~completion ~prompt ~hist =
  let module IL = Readline__input_loop in
  let rec loop v =
    IL.print ~prompt ~map_out v;
    match Readline__input_char.get () with
    | `Newline ->
        print_newline ();
        IL.contents v;
    | #IL.input as in_v -> loop (IL.step ?completion v in_v)
  in
  try
    if Console.is_color_tty () then
      with_readline (fun () -> Some (loop (IL.create ?text hist)))
    else begin
      print_string prompt;
      Some (Pervasives.read_line ())
    end
  with End_of_file -> None

(**
   Handling the history.

   We do this in a very quick and dirty way:
   _We keep two lists: [current] and [pending] and append to both;
   when pending's length reaches [size] we rotate [pending] to [current] and place
   a new list in [pending].
*)
module History = struct
  type t = {
    size : int;
    mutable current:string list;
    mutable pending:string list
  }

  let create size = {
    size = size;
    current = [];
    pending = []
  }

  let flush h =
    h.current <- [];
    h.pending <- []

  let to_list h = List.take h.current h.size

  let of_list ?(size=50) l =
    let l = List.take l size in
    {
      size = size;
      current = l;
      pending = l;
    }

  let snapshot h =
    if h.size = 0 then
      []
    else
      h.current

  let add h v =
    h.current <- v::h.current;
    h.pending <- v::h.pending;
    if List.length h.pending > h.size then begin
      h.current <- h.pending;
      h.pending <- []
    end

  let null = create 0

  let default = create 50
end


let empty_completer ~left:_ ~right:_ = []

let input_line
    ?(history = History.default)
    ?(prompt="> ")
    ?text
    ?tab_completion
    ()
    =
  let res = mainloop
    ~map_out:ident
    ~hist:(History.snapshot history)
    ~prompt
    ?text
    ?completion:tab_completion
  in
  begin match res with
  | None | Some "" -> ()
  | Some v ->  History.add history v
  end;
  res

let input_line_eof
    ?history
    ?prompt
    ?text
    ?tab_completion
    () =
  match input_line
    ?history
    ?prompt
    ?text
    ?tab_completion
    ()
  with
  | None -> raise End_of_file
  | Some v -> v


let password ?(prompt="") () =
  mainloop
    ~map_out:(String.map ~f:(fun _ -> '*'))
    ~hist:[]
    ~prompt
    ~completion:None
    ?text:None

let confirm ?(prompt="") true_answer =
  let ans = mainloop
    ~map_out:ident
    ~hist:[]
    ~prompt
    ~completion:None
    ?text:None
  in
  Option.value_map ans
    ~f:(fun v -> String.lowercase v = String.lowercase true_answer)
    ~default:false

let choice choices =
  Option.iter (List.find_a_dup (List.map ~f:fst choices))
    ~f:(fun v -> failwithf "Readline.choice: two different choices for %s"
          v
          ());
  let make_sel_string s pos =
    sprintf "%s(%c)%s"
      (String.sub s ~pos:0 ~len:pos)
      s.[pos]
      (String.sub s ~pos:(pos+1) ~len:(String.length s -pos -1) )
  in

  let rec choose_id_char assigned s pos =
    if pos >= String.length s then
      None
    else
      let sel_string = String.of_char (s.[pos]) in
      if List.Assoc.mem assigned sel_string then
        choose_id_char assigned s (pos+1)
      else
        Some (sel_string,make_sel_string s pos)
  in

  let (strings,choices) =
    List.fold_left choices
      ~f:(fun (strings,choices) (s,v) ->
            match choose_id_char choices s 0 with
            | Some (idx,string) ->
                (string::strings),((idx,v)::choices)
            | None ->
                (** Failed to assign a short char *)
                (s::strings),choices)
      ~init:([],choices)
  in
  let prompt = String.concat ~sep:"," (List.rev strings) ^ "? " in
  let rec loop () =
    match
      mainloop ~prompt ~map_out:ident ~hist:[] ~completion:None ?text:None
    with
    | None -> None
    | Some x ->
      match List.Assoc.find choices (String.strip x) with
      | Some _ as s -> s
      | None -> loop ()
  in
  loop ()
