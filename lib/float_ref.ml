open Core.Std

type t = { mutable v : float }

let create v = { v }
let set t v = t.v <- v
let get t = t.v

TEST_UNIT =
  let t = create 0. in
  for i = 1 to 1000 do
    let x = Float.of_int i in
    assert (get t = x -. 1.);
    set t x;
    assert (get t = x)
  done

BENCH_MODULE "" = struct
  type nonrec t = {
    x : int;
    mutable y : float;
    z : t;
  }

  BENCH "float ref set" =
    let t = { x = 1; y = 1.; z = create 1. } in
    let a = 1. in
    for _i = 1 to 100 do
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

  BENCH "Float_ref.set" =
    let t = { x = 1; y = 1.; z = create 1. } in
    let a = 1. in
    for _i = 1 to 100 do
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

  BENCH "float ref get" =
    let t = { x = 1; y = 1.; z = create 1. } in
    for _i = 1 to 100 do
      let _ = t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y +. t.y in ()
    done

  BENCH "Float_ref.get" =
    let t = { x = 1; y = 1.; z = create 1. } in
    for _i = 1 to 100 do
      let _ =
           t.z.v +. t.z.v +. t.z.v +. t.z.v +. t.z.v
        +. t.z.v +. t.z.v +. t.z.v +. t.z.v +. t.z.v
      in ()
    done
end
