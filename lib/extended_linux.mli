open Core.Std
open Unix

type uids = {
  ruid:int;
  euid:int;
  suid:int
} with sexp,bin_io

val setresuid : (?ruid:int -> ?euid:int -> ?suid:int -> unit -> unit) Or_error.t
val getresuid : (unit -> uids) Or_error.t

(** {6 Splicing - zero-copies between kernel buffers} *)

(* Example usage diagram:

   In the below diagram, starting at the left upper corner, we first
   splice a socket into a pipe.  Then we duplicate this pipe into two
   other pipes using "tee".  The first pipe is spliced into a file
   descriptor (e.g. to log data coming in from a socket connection to
   a file).  The second pipe is used by the user to actually read data.

   After handling the received data, the user puts the new data to
   be sent out into an output buffer and vmsplices it into a pipe.
   Use double buffering ( = switching between two buffers) to prevent
   data inconsistencies while the kernel may be reading from the user
   provided pages, and make sure not to let buffers be reclaimed by the
   GC as long as they may still be in use!  These buffers currently need
   not be larger than 64KB each, which is the size of kernel buffers
   (= Unix-pipes).

   The end of the output pipe is then duplicated into two more output
   pipes using "tee" again.  If these two "tees" have seen all the data
   vmspliced from a user buffer, the user can safely switch to it again
   from the other double buffer.

   Finally, the first pipe is used to splice data into a socket to send
   out the user data, and the second pipe is used to stream this data
   to a file.  Note that using more pipes and more "tee"-calls one can
   very cheaply duplicate the data to even more destinations!

                       tee              splice
                           +----> pipe ----+---> fd
             splice       /
     socket ----+---> pipe
                          \             read
                           +----> pipe ---+--> user input space
                       tee                       |
                                                 + do stuff
                                                 |
                                               user output space
                                                 |
                                                 + vmsplice (double buffer!)
                                                 |
                                                pipe
                                                 |
                                                / \
                                           tee +   + tee
                                               |   |
                                             pipe pipe
                                             /       \
                                     splice +         + splice
                                           /           \
                                         sock           fd
*)

module Splice : sig
  (** {6 Splice flags} *)

  (** Type of Splice event flag *)
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io

  (** Type of Splice event flags *)
  type flags

  val make_flags : flag array -> flags
  (** [make_flags ar] @return flags constructed from the array of flags [ar]. *)


  (** {6 Splice functions} *)

  val splice :
    (?assume_fd_is_nonblocking : bool ->
     fd_in : File_descr.t -> ?off_in : int ->
     fd_out : File_descr.t -> ?off_out : int ->
     len : int ->
     flags
     -> int * int * int)
    Or_error.t
  (** [splice ?assume_fd_is_nonblocking ~fd_in ?off_in ~fd_out ?off_out
      ~len flags] see man-page for details.  @return the triple [(ret,
      ret_off_in, ret_off_out)], where [ret] corresponds to the return
      value of the system call, [ret_off_in] to the final input offset,
      and [ret_off_out] to the final output offset.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the offsets or length are invalid

      @param assume_fd_is_nonblocking default = false
      @param off_in default = 0
      @param off_out default = 0
  *)

  val tee :
    (?assume_fd_is_nonblocking : bool ->
     fd_in : File_descr.t -> fd_out : File_descr.t -> int -> flags -> int)
    Or_error.t
  (** [tee ?assume_fd_is_nonblocking ~fd_in ~fd_out len flags] see man-page
      for details.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the length is invalid

      @param assume_fd_is_nonblocking default = false
  *)

  val vmsplice :
    (?assume_fd_is_nonblocking : bool ->
     File_descr.t -> Bigstring.t IOVec.t array -> ?count : int -> flags -> int)
    Or_error.t
  (** [vmsplice ?assume_fd_is_nonblocking fd iovecs ?count flags]
      see man-page for details.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the count is invalid

      @param assume_fd_is_nonblocking default = false
      @param count default = [Array.length iovecs]
  *)
end

module Statfs : sig
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
end

val statfs : (string -> Statfs.t) Or_error.t
