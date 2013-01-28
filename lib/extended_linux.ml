open Core.Std

INCLUDE "config.mlh"

type uids = {
  ruid:int;
  euid:int;
  suid:int
} with sexp,bin_io

module Statfs = struct
  module Raw = struct
    type t =
      {
        f_type    : Int32.t;
        f_bsize   : int;
        f_blocks  : int;
        f_bfree   : int;
        f_bavail  : int;
        f_files   : int;
        f_ffree   : int;
        f_namelen : int;
      }
    ;;
  end
  type f_type =
        ADFS_SUPER_MAGIC | AFFS_SUPER_MAGIC | BEFS_SUPER_MAGIC | BFS_MAGIC
      | CIFS_MAGIC_NUMBER | CODA_SUPER_MAGIC | COH_SUPER_MAGIC | CRAMFS_MAGIC
      | DEVFS_SUPER_MAGIC | EFS_SUPER_MAGIC | EXT_SUPER_MAGIC | EXT2_OLD_SUPER_MAGIC
      | EXT2_SUPER_MAGIC | EXT3_SUPER_MAGIC | HFS_SUPER_MAGIC | HPFS_SUPER_MAGIC
      | HUGETLBFS_MAGIC | ISOFS_SUPER_MAGIC | JFFS2_SUPER_MAGIC | JFS_SUPER_MAGIC
      | MINIX_SUPER_MAGIC | MINIX_SUPER_MAGIC2 | MINIX2_SUPER_MAGIC | MINIX2_SUPER_MAGIC2
      | MSDOS_SUPER_MAGIC | NCP_SUPER_MAGIC | NFS_SUPER_MAGIC | NTFS_SB_MAGIC
      | UNKNOWN_SUPER_MAGIC of Int32.t
  ;;
  type t =
    {
      f_type    : f_type;
      f_bsize   : int;
      f_blocks  : int;
      f_bfree   : int;
      f_bavail  : int;
      f_files   : int;
      f_ffree   : int;
      f_namelen : int;
    }
  ;;
  let of_rawstatfs raw =
    {
      f_type =
         begin match raw.Raw.f_type with
         | 0xadf5l     -> ADFS_SUPER_MAGIC
         | 0xADFFl     -> AFFS_SUPER_MAGIC
         | 0x42465331l -> BEFS_SUPER_MAGIC
         | 0x1BADFACEl -> BFS_MAGIC
         | 0xFF534D42l -> CIFS_MAGIC_NUMBER
         | 0x73757245l -> CODA_SUPER_MAGIC
         | 0x012FF7B7l -> COH_SUPER_MAGIC
         | 0x28cd3d45l -> CRAMFS_MAGIC
         | 0x1373l     -> DEVFS_SUPER_MAGIC
         | 0x00414A53l -> EFS_SUPER_MAGIC
         | 0x137Dl     -> EXT_SUPER_MAGIC
         | 0xEF51l     -> EXT2_OLD_SUPER_MAGIC
         | 0xEF53l     -> EXT2_SUPER_MAGIC
(*       | 0xEF53l     -> EXT3_SUPER_MAGIC *)
         | 0x4244l     -> HFS_SUPER_MAGIC
         | 0xF995E849l -> HPFS_SUPER_MAGIC
         | 0x958458f6l -> HUGETLBFS_MAGIC
         | 0x9660l     -> ISOFS_SUPER_MAGIC
         | 0x72b6l     -> JFFS2_SUPER_MAGIC
         | 0x3153464al -> JFS_SUPER_MAGIC
         | 0x137Fl     -> MINIX_SUPER_MAGIC
         | 0x138Fl     -> MINIX_SUPER_MAGIC2
         | 0x2468l     -> MINIX2_SUPER_MAGIC
         | 0x2478l     -> MINIX2_SUPER_MAGIC2
         | 0x4d44l     -> MSDOS_SUPER_MAGIC
         | 0x564cl     -> NCP_SUPER_MAGIC
         | 0x6969l     -> NFS_SUPER_MAGIC
         | 0x5346544el -> NTFS_SB_MAGIC
         | magic       -> UNKNOWN_SUPER_MAGIC magic
         end;
      f_bsize   = raw.Raw.f_bsize;
      f_blocks  = raw.Raw.f_blocks;
      f_bfree   = raw.Raw.f_bfree;
      f_bavail  = raw.Raw.f_bavail;
      f_files   = raw.Raw.f_files;
      f_ffree   = raw.Raw.f_ffree;
      f_namelen = raw.Raw.f_namelen
    }
  ;;
end ;;

IFDEF LINUX_EXT THEN

external setresuid : ruid:int -> euid:int -> suid:int -> unit = "linux_setresuid_stub"

let setresuid ?(ruid= -1) ?(euid= -1) ?(suid= -1) () =
  setresuid ~ruid ~euid ~suid

external getresuid : unit -> uids = "linux_getresuid_stub"

let setresuid = Ok setresuid
let getresuid = Ok getresuid

(* Splicing - zero-copies between kernel buffers *)

open Unix

module Splice = struct
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io
  type flags

  external unsafe_splice :
    bool ->
    fd_in : File_descr.t -> off_in : int ->
    fd_out : File_descr.t -> off_out : int ->
    len : int ->
    flags
    -> int * int * int = "linux_splice_stub_bc" "linux_splice_stub"

  let splice
        ?(assume_fd_is_nonblocking = false)
        ~fd_in ?off_in
        ~fd_out ?off_out ~len flags =
    let off_in =
      match off_in with
      | None -> -1
      | Some off_in when off_in < 0 -> invalid_arg "Splice.splice: off_in < 0"
      | Some off_in -> off_in
    in
    let off_out =
      match off_out with
      | None -> -1
      | Some off_out when off_out < 0 ->
          invalid_arg "Splice.splice: off_out < 0"
      | Some off_out -> off_out
    in
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_splice assume_fd_is_nonblocking ~fd_in ~off_in ~fd_out ~off_out ~len flags

  external unsafe_tee :
    bool -> fd_in : File_descr.t -> fd_out : File_descr.t -> int -> flags -> int
    = "linux_tee_stub"

  let tee ?(assume_fd_is_nonblocking = false) ~fd_in ~fd_out len flags =
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_tee assume_fd_is_nonblocking ~fd_in ~fd_out len flags

  external unsafe_vmsplice :
    bool -> File_descr.t -> int -> flags -> int = "linux_vmsplice_stub"

  let vmsplice ?(assume_fd_is_nonblocking = false) fd iovecs ?count flags =
    let count =
      match count with
      | None -> Array.length iovecs
      | Some count ->
          if count < 0 then invalid_arg "Splice.vmsplice: count < 0";
          let n_iovecs = Array.length iovecs in
          if count > n_iovecs then
            invalid_arg "Splice.vmsplice: count > n_iovecs";
          count
    in
    unsafe_vmsplice assume_fd_is_nonblocking fd count flags

  external make_flags : flag array -> flags = "linux_splice_make_flags_stub"
  let splice   = Ok splice
  let tee      = Ok tee
  let vmsplice = Ok vmsplice
end

external linux_statfs_stub : string -> Statfs.Raw.t = "linux_statfs_stub" ;;
let statfs path = Statfs.of_rawstatfs (linux_statfs_stub path) ;;
let statfs = Ok statfs

ELSE

let setresuid = unimplemented "Extended_linux.setresuid"
let getresuid = unimplemented "Extended_linux.getresuid"

module Splice = struct
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io
  type flags = flag array

  let make_flags = Fn.id
  let splice   = unimplemented "Extended_linux.Splice.splice"
  let tee      = unimplemented "Extended_linux.Splice.tee"
  let vmsplice = unimplemented "Extended_linux.Splice.vmsplice"
end

let statfs = unimplemented "Extended_linux.statfs"

ENDIF
