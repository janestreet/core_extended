(* This is a complete rewrite of David Mentré's Rw_mutex-module (part of
   his ocaml_thread_synchro-library):

   Copyright 2004 Jane Street Capital (author: Markus Mottl)
   Copyright 2001 David Mentré

   All rights reserved.  This file is distributed under the terms of
   the GNU Library General Public License.
*)

type pref = [ `Readers | `Writers | `NoPref ]

type 'pref kind = pref

let r_pref = `Readers
let w_pref = `Writers
let np_pref = `NoPref

type rw_t =
  {
    rw_mtx : Mutex.t;
    read_cond : Condition.t;
    write_cond : Condition.t;
    mutable rw_active : int;
    mutable read_wait : int;
    mutable write_wait : int;
  }

type np_t =
  {
    np_mtx : Mutex.t;
    cond : Condition.t;
    mutable np_active : int;
  }

type 'pref t = Readers of rw_t | Writers of rw_t | NoPref of np_t

let create_rw () =
  {
    rw_mtx = Mutex.create ();
    read_cond = Condition.create ();
    write_cond = Condition.create ();
    rw_active = 0;
    read_wait = 0;
    write_wait = 0;
  }

let create = function
  | `Readers -> Readers (create_rw ())
  | `Writers -> Writers (create_rw ())
  | `NoPref ->
      NoPref
        {
          np_mtx = Mutex.create ();
          cond = Condition.create ();
          np_active = 0;
        }

let r_lock = function
  | Readers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      if m.rw_active < 0 then (
        m.read_wait <- m.read_wait + 1;
        while m.rw_active < 0 do
          Condition.wait m.read_cond mtx
        done;
        m.read_wait <- m.read_wait - 1);
      m.rw_active <- m.rw_active + 1;
      Mutex.unlock mtx
  | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      if m.rw_active < 0 || m.write_wait > 0 then (
        m.read_wait <- m.read_wait + 1;
        while m.rw_active < 0 || m.write_wait > 0 do
          Condition.wait m.read_cond mtx
        done;
        m.read_wait <- m.read_wait - 1);
      m.rw_active <- m.rw_active + 1;
      Mutex.unlock mtx
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      while m.np_active < 0 do
        Condition.wait m.cond mtx
      done;
      m.np_active <- m.np_active + 1;
      Mutex.unlock mtx

let r_unlock = function
  | Readers m | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      let active_1 = m.rw_active - 1 in
      m.rw_active <- active_1;
      if active_1 = 0 && m.write_wait > 0 then
        Condition.signal m.write_cond;
      Mutex.unlock mtx
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      let active_1 = m.np_active - 1 in
      m.np_active <- active_1;
      if active_1 = 0 then Condition.signal m.cond;
      Mutex.unlock mtx

let w_lock = function
  | Readers m | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      if m.rw_active <> 0 then (
        m.write_wait <- m.write_wait + 1;
        while m.rw_active <> 0 do
          Condition.wait m.write_cond mtx
        done;
        m.write_wait <- m.write_wait - 1);
      m.rw_active <- -1;
      Mutex.unlock mtx
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      while m.np_active <> 0 do
        Condition.wait m.cond mtx
      done;
      m.np_active <- -1;
      Mutex.unlock mtx

let w_unlock = function
  | Readers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      m.rw_active <- 0;
      if m.read_wait > 0 then Condition.broadcast m.read_cond
      else if m.write_wait > 0 then Condition.signal m.write_cond;
      Mutex.unlock mtx
  | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      m.rw_active <- 0;
      if m.write_wait > 0 then Condition.signal m.write_cond
      else if m.read_wait > 0 then Condition.broadcast m.read_cond;
      Mutex.unlock mtx
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      m.np_active <- 0;
      Condition.broadcast m.cond;
      Mutex.unlock mtx

let try_r_lock = function
  | Readers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      let active = m.rw_active in
      let only_readers = active >= 0 in
      if only_readers then m.rw_active <- active + 1;
      Mutex.unlock mtx;
      only_readers
  | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      let active = m.rw_active in
      let only_readers = m.write_wait = 0 && active >= 0 in
      if only_readers then m.rw_active <- active + 1;
      Mutex.unlock mtx;
      only_readers
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      let active = m.np_active in
      let only_readers = active >= 0 in
      if only_readers then m.np_active <- active + 1;
      Mutex.unlock mtx;
      only_readers

let try_w_lock = function
  | Readers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      let no_others = m.rw_active = 0 && m.read_wait = 0 in
      if no_others then m.rw_active <- -1;
      Mutex.unlock mtx;
      no_others
  | Writers m ->
      let mtx = m.rw_mtx in
      Mutex.lock mtx;
      let no_others = m.rw_active = 0 in
      if no_others then m.rw_active <- -1;
      Mutex.unlock mtx;
      no_others
  | NoPref m ->
      let mtx = m.np_mtx in
      Mutex.lock mtx;
      let no_others = m.np_active = 0 in
      if no_others then m.np_active <- -1;
      Mutex.unlock mtx;
      no_others

let wrap_r_lock m f =
  r_lock m;
  let res = try f () with exc -> r_unlock m; raise exc in
  r_unlock m;
  res

let try_wrap_r_lock m f =
  if try_r_lock m then (
    let res = try f () with exc -> r_unlock m; raise exc in
    r_unlock m;
    Some res)
  else None

let btry_wrap_r_lock m f =
  try_r_lock m && (
    (try f () with exc -> r_unlock m; raise exc);
    r_unlock m;
    true)

let wrap_w_lock m f =
  w_lock m;
  let res = try f () with exc -> w_unlock m; raise exc in
  w_unlock m;
  res

let try_wrap_w_lock m f =
  if try_w_lock m then (
    let res = try f () with exc -> w_unlock m; raise exc in
    w_unlock m;
    Some res)
  else None

let btry_wrap_w_lock m f =
  try_w_lock m && (
    (try f () with exc -> w_unlock m; raise exc);
    w_unlock m;
    true)
