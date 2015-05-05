type 'a t =
  {
    mutable v_opt : 'a option;
    mtx : Mutex.t;
    cond : Condition.t
  }

let init v_opt =
  {
    v_opt = v_opt;
    mtx = Mutex.create ();
    cond = Condition.create ()
  }

let signal sem v =
  Mutex.lock sem.mtx;
  let v_opt = sem.v_opt in
  sem.v_opt <- Some v;
  if v_opt = None then Condition.signal sem.cond;
  Mutex.unlock sem.mtx

let wait_return sem v =
  sem.v_opt <- None;
  Mutex.unlock sem.mtx;
  v

let rec wait_loop sem =
  Condition.wait sem.cond sem.mtx;
  match sem.v_opt with
  | None -> wait_loop sem
  | Some v -> wait_return sem v

let wait sem =
  Mutex.lock sem.mtx;
  match sem.v_opt with
  | None -> wait_loop sem
  | Some v -> wait_return sem v

let get sem =
  match sem.v_opt with
  | None as none -> none
  | _ ->
      Mutex.lock sem.mtx;
      let res = sem.v_opt in
      sem.v_opt <- None;
      Mutex.unlock sem.mtx;
      res

let look sem = sem.v_opt
