open Core

type t = { mutable v : float } [@@deriving bin_io, sexp]

let create v = { v }
let set t v = t.v <- v
let get t = t.v

let%test_unit _ =
  let t = create 0. in
  for i = 1 to 1000 do
    let x = Float.of_int i in
    assert (get t = x -. 1.);
    set t x;
    assert (get t = x)
  done

let%bench_module "" = (module struct
  type nonrec t = {
    x : int;
    mutable y : float;
    z : t;
  }

  let%bench "float ref set" =
    let t = { x = 1; y = 1.; z = create 1. } in
    let a = 1. in
    for _ = 1 to 100 do
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a;
      t.y <- a
    done

  let%bench "Float_ref.set" =
    let t = { x = 1; y = 1.; z = create 1. } in
    let a = 1. in
    for _ = 1 to 100 do
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a;
      set t.z a
    done

  let%bench "float ref get" =
    let t = { x = 1; y = 1.; z = create 1. } in
    for _ = 1 to 100 do
      let _ = t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y in ()
    done

  let%bench "Float_ref.get" =
    let t = { x = 1; y = 1.; z = create 1. } in
    for _ = 1 to 100 do
      let _ =
           t.z.v +. t.z.v +. t.z.v +. t.z.v +. t.z.v
        +. t.z.v +. t.z.v +. t.z.v +. t.z.v +. t.z.v
      in ()
    done
end)
