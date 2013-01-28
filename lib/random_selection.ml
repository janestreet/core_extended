(* reservoir sampling
   see http://www.nist.gov/dads/HTML/reservoirSampling.html
   see http://hnr.dnsalias.net/wordpress/?p=43
*)

open Core.Std

type 'a t = {
  rand : int -> int;
  desired_sample_size : int;
  selection : 'a option array;
  num_seen : int ref;
}

let desired_sample_size t = t.desired_sample_size

let to_list t = List.filter_opt (Array.to_list t.selection)

let create ?random_state desired_sample_size =
  let rand =
    let state =
      match random_state with
      | None -> Random.State.default
      | Some state -> state
    in
    fun n -> Random.State.int state n
  in
  let selection = Array.create ~len:desired_sample_size None in
  { rand; desired_sample_size; selection; num_seen = ref 0 }

let maybe_add {rand; desired_sample_size; selection; num_seen} elem =
  incr num_seen;
  if !num_seen <= desired_sample_size then
    Array.set selection (!num_seen - 1) (Some elem)
  else begin
    if rand !num_seen < desired_sample_size then begin
      Array.set selection (rand desired_sample_size) (Some elem);
    end
  end

let select ?random_state ~next desired_sample_size =
  let t = create ?random_state desired_sample_size in
  let rec loop () =
    match next () with
    | None -> ()
    | Some elem -> maybe_add t elem; loop ()
  in
  loop ();
  to_list t

