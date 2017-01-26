open Core

let random_split ?random_state array ~p =
  let a = Array.copy array in
  if p > 1.0 || p < 0.0 then
    failwith "Array.random_split: p is out of bounds [0 1]";
  let stop = Float.iround_exn ~dir:`Nearest (p *. (float (Array.length a))) in
  if stop = 0 then
    (* in slice a stop of 0 means slicing to the end of the array, which is not what we
       want *)
    ([||], a)
  else
    begin
      Array.permute a ?random_state;
      ((Array.slice a 0 stop), (Array.slice a stop 0))
    end

let random_sub ?random_state array ~p =
  fst (random_split ~p array ?random_state)
