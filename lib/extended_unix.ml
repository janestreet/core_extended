open Core.Std
open Unix

external raw_fork_exec :
  stdin : File_descr.t
  -> stdout : File_descr.t
  -> stderr : File_descr.t
  -> ?working_dir : string
  -> ?setuid : int
  -> ?setgid : int
  -> ?env : (string) array
  -> string
  -> string array
  -> Pid.t
  =  "extended_ml_spawn_bc" "extended_ml_spawn"

module Env = struct
  open String.Map
  type t = string String.Map.t

  let empty : t = empty

  let get ()  =
    Array.fold  (Unix.environment ())
      ~init:empty
      ~f:(fun env str ->
        match String.lsplit2 ~on:'=' str with
        | Some (key,data) -> add ~key ~data env
        | None ->
          failwithf
            "extended_unix.Env.get %S is not in the form of key=value"
            str
            ())

  let add ~key ~data env =
    if String.mem key '=' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an equal sign"
        key
        ()
    else if String.mem key '\000' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an null character"
        key
        ()
    else if String.mem data '\000' then
      failwithf "extended_unix.Env.add:\
  value (%S) to export in the environment for %S contains an null character"
        data
        key
        ()
    else
      String.Map.add ~key ~data env

  let to_string_array env =
    String.Map.to_alist env
    |! List.map ~f:(fun (k,v) -> k^"="^v)
    |! List.to_array
end

let fork_exec
    ?(stdin=Unix.stdin)
    ?(stdout=Unix.stdout)
    ?(stderr=Unix.stderr)
    ?(path_lookup=true)
    ?env
    ?working_dir
    ?setuid
    ?setgid
    prog
    args
    =
  let env = Option.map env
    ~f:(fun e ->
      let init,l = match e with
        | `Extend  l ->
          Env.get (),l
        | `Replace l ->
          Env.empty,l
      in
      List.fold_left l
        ~init
        ~f:(fun env (key,data) -> Env.add ~key ~data env)
      |! Env.to_string_array)

  and full_prog =
    if path_lookup then
      match Shell__core.which prog with
      | Some s -> s
      | None -> failwithf "fork_exec: Process not found %s"
        prog
        ()
    else
      prog
  in
  raw_fork_exec
    ~stdin
    ~stdout
    ~stderr
    ?working_dir
    ?setuid
    ?setgid
    ?env
    full_prog
    (Array.of_list (prog::args))

external seteuid : int -> unit = "extended_ml_seteuid"
external setreuid : uid:int -> euid:int -> unit = "extended_ml_setreuid"
external gettid : unit -> int = "extended_ml_gettid"

external htonl : Int32.t -> Int32.t = "extended_ml_htonl"
external ntohl : Int32.t -> Int32.t = "extended_ml_ntohl"

TEST =
  htonl (ntohl 0xdeadbeefl) = 0xdeadbeefl

type statvfs = {
  bsize: int;                           (** file system block size *)
  frsize: int;                          (** fragment size *)
  blocks: int;                          (** size of fs in frsize units *)
  bfree: int;                           (** # free blocks *)
  bavail: int;                          (** # free blocks for non-root *)
  files: int;                           (** # inodes *)
  ffree: int;                           (** # free inodes *)
  favail: int;                          (** # free inodes for non-root *)
  fsid: int;                            (** file system ID *)
  flag: int;                            (** mount flags *)
  namemax: int;                         (** maximum filename length *)
} with sexp, bin_io

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd = struct
  open Passwd

  let of_passwd_line_exn s =
    match String.split s ~on:':' with
    | name::passwd::uid::gid::gecos::dir::shell::[] ->
        { name = name;
          passwd = passwd;
          uid = Int.of_string uid;
          gid = Int.of_string gid;
          gecos = gecos;
          dir = dir;
          shell = shell
        }
    | _ -> failwithf "of_passwd_line: failed to parse: %s" s ()
  ;;
  let of_passwd_line s = Option.try_with (fun () -> of_passwd_line_exn s) ;;

  let of_passwd_file_exn fn =
    Exn.protectx (In_channel.create fn)
      ~f:(fun chan ->
        List.map (In_channel.input_lines chan) ~f:of_passwd_line_exn)
      ~finally:In_channel.close
  ;;

  let of_passwd_file f = Option.try_with (fun () -> of_passwd_file_exn f) ;;
end

external strptime : fmt:string -> string -> Unix.tm = "unix_strptime"

(* This is based on jli's util code and the python iptools implementation. *)
module Cidr = struct
  type t = {
    address : Unix.Inet_addr.t;
    bits : int;
  } with sexp, fields

  let of_string_exn s =
    match String.split ~on:'/' s with
    | [s_inet_address ; s_bits] ->
      begin
          let bits = Int.of_string s_bits in
          assert (bits >= 0);
          assert (bits <= 32);
          {address=Unix.Inet_addr.of_string s_inet_address; bits=bits}
      end
    | _ -> failwith ("Unable to parse "^s^" into a CIDR address/mask pair.")

  let of_string s =
    try
      Some (of_string_exn s)
    with _ -> None

  let to_string t =
    sprintf !"%{Unix.Inet_addr}/%d" t.address t.bits


  (** IPv6 addresses are not supported.
    The RFC regarding how to properly format an IPv6 string is...painful.

    Note the 0010 and 0000:
     # "2a03:2880:0010:1f03:face:b00c:0000:0025" |! Unix.Inet_addr.of_string |!
     Unix.Inet_addr.to_string ;;
      - : string = "2a03:2880:10:1f03:face:b00c:0:25"
    *)

  let inet6_addr_to_int_exn _addr =
    failwith "IPv6 isn't supported yet."

  let ip4_valid_range =
    List.map ~f:(fun y ->
          assert (y <= 255) ;
          assert (y >= 0);
          y)

  let inet4_addr_to_int_exn addr =
    let stringified = Unix.Inet_addr.to_string addr in
    match String.split ~on:'.' stringified
    |! List.map ~f:Int.of_string
    |! ip4_valid_range
    with
    | [a;b;c;d] ->
      let lower_24 = Int32.of_int_exn ((b lsl 16) lor (c lsl 8) lor d)
      and upper_8  = Int32.(shift_left (of_int_exn a) 24)
      in Int32.bit_or upper_8 lower_24
    | _ -> failwith (stringified ^ " is not a valid IPv4 address.")

  let inet4_addr_of_int_exn l =
    let lower_24 = Int32.(to_int_exn (bit_and l (of_int_exn 0xFF_FFFF))) in
    let upper_8  = Int32.(to_int_exn (shift_right_logical l 24)) in
    Unix.Inet_addr.of_string (sprintf "%d.%d.%d.%d"
      (upper_8         land 0xFF)
      (lower_24 lsr 16 land 0xFF)
      (lower_24 lsr  8 land 0xFF)
      (lower_24        land 0xFF))

  let inet_addr_to_int_exn addr =
    let stringified = Unix.Inet_addr.to_string addr in
    let has_colon = String.contains stringified ':' in
    let has_period = String.contains stringified '.' in
    match has_colon, has_period with
    | true, false -> inet6_addr_to_int_exn addr
    | false, true -> inet4_addr_to_int_exn addr
    | true, true -> failwith "Address cannot have both : and . in it."
    | false, false -> failwith "No address delimter (: or .) found."

  let cidr_to_block c =
    let baseip = inet_addr_to_int_exn c.address in
    let shift = 32 - c.bits in
    Int32.(shift_left (shift_right_logical baseip shift) shift)

  let match_exn t address =
    Int32.equal (cidr_to_block t) (cidr_to_block {t with address})

  let match_ t ip =
    try
      Some (match_exn t ip)
    with _ -> None

  (* This exists mostly to simplify the tests below. *)
  let match_strings c a =
    let c = of_string_exn c in
    let a = Unix.Inet_addr.of_string a in
    match_exn c a

  let _flag = Command.Spec.Arg_type.create of_string_exn
end

(* Can we parse some random correct netmasks? *)
TEST = Cidr.of_string "10.0.0.0/8" <> None
TEST = Cidr.of_string "172.16.0.0/12" <> None
TEST = Cidr.of_string "192.168.0.0/16" <> None
TEST = Cidr.of_string "192.168.13.0/24" <> None
TEST = Cidr.of_string "172.25.42.0/18" <> None

(* Do we properly fail on some nonsense? *)
TEST = Cidr.of_string "172.25.42.0/35" =  None
TEST = Cidr.of_string "172.25.42.0/sandwich" =  None
TEST = Cidr.of_string "sandwich/sandwich" =  None
TEST = Cidr.of_string "sandwich/39" =  None
TEST = Cidr.of_string "sandwich/16" =  None
TEST = Cidr.of_string "172.52.43/16" =  None
TEST = Cidr.of_string "172.52.493/16" =  None

TEST_MODULE = struct

  (* Can we convert ip addr to an int? *)
  let test_inet_addr_to_int str num =
    let inet = Unix.Inet_addr.of_string str in
    Cidr.inet_addr_to_int_exn inet = num

  TEST = test_inet_addr_to_int "0.0.0.1"           1l
  TEST = test_inet_addr_to_int "1.0.0.0"         0x1000000l
  TEST = test_inet_addr_to_int "255.255.255.255" 0xffffffffl
  TEST = test_inet_addr_to_int "172.25.42.1"     0xac192a01l
  TEST = test_inet_addr_to_int "4.2.2.1"         0x4020201l
  TEST = test_inet_addr_to_int "8.8.8.8"         0x8080808l
  TEST = test_inet_addr_to_int "173.194.73.103"  0xadc24967l
  TEST = test_inet_addr_to_int "98.139.183.24"   0x628bb718l

  (* And from an int to a string? *)
  let test_inet_addr_of_int num str =
    let inet = Unix.Inet_addr.of_string str in
    Cidr.inet4_addr_of_int_exn num = inet

  TEST = test_inet_addr_of_int 0xffffffffl "255.255.255.255"
  TEST = test_inet_addr_of_int 0l          "0.0.0.0"
  TEST = test_inet_addr_of_int 0x628bb718l "98.139.183.24"
  TEST = test_inet_addr_of_int 0xadc24967l "173.194.73.103"

(* And round trip for kicks *)
  TEST_UNIT =
    let inet  = Unix.Inet_addr.of_string "4.2.2.1" in
    let inet' = Cidr.inet4_addr_of_int_exn (Cidr.inet_addr_to_int_exn inet) in
    if inet <> inet' then
      failwithf "round-tripping %s produced %s"
        (Unix.Inet_addr.to_string inet)
        (Unix.Inet_addr.to_string inet') ()
end

(* Basic match tests *)
TEST = Cidr.match_strings "10.0.0.0/8" "9.255.255.255" = false
TEST = Cidr.match_strings "10.0.0.0/8" "10.0.0.1" = true
TEST = Cidr.match_strings "10.0.0.0/8" "10.34.67.1" = true
TEST = Cidr.match_strings "10.0.0.0/8" "10.255.255.255" = true
TEST = Cidr.match_strings "10.0.0.0/8" "11.0.0.1" = false

TEST = Cidr.match_strings "172.16.0.0/12" "172.15.255.255" = false
TEST = Cidr.match_strings "172.16.0.0/12" "172.16.0.0" = true
TEST = Cidr.match_strings "172.16.0.0/12" "172.31.255.254" = true

TEST = Cidr.match_strings "172.25.42.0/24" "172.25.42.1" = true
TEST = Cidr.match_strings "172.25.42.0/24" "172.25.42.255" = true
TEST = Cidr.match_strings "172.25.42.0/24" "172.25.42.0" = true

TEST = Cidr.match_strings "172.25.42.0/16" "172.25.0.1" = true
TEST = Cidr.match_strings "172.25.42.0/16" "172.25.255.254" = true
TEST = Cidr.match_strings "172.25.42.0/16" "172.25.42.1" = true
TEST = Cidr.match_strings "172.25.42.0/16" "172.25.105.237" = true

(* And some that should fail *)
TEST = Cidr.match_strings "172.25.42.0/24" "172.26.42.47" = false
TEST = Cidr.match_strings "172.25.42.0/24" "172.26.42.208" = false

module Inet_port = struct
  type t = int with sexp

  let of_int_exn x =
    if x > 0 && x < 65536 then
      x
    else
      failwith (sprintf "%d is not a valid port number." x)

  let of_int x =
    try
      Some (of_int_exn x )
    with _ ->
      None

  let of_string_exn x =
    Int.of_string x |! of_int_exn

  let of_string x =
    try
      Some (of_string_exn x)
    with _ ->
      None

  let to_string x =
    Int.to_string x

  let to_int x =
    x

  let t_of_sexp sexp = String.t_of_sexp sexp |! of_string_exn
  let sexp_of_t t = to_string t |! String.sexp_of_t

  let _flag = Command.Spec.Arg_type.create of_string_exn
end

TEST = Inet_port.of_string "88" = Some 88
TEST = Inet_port.of_string "2378472398572" = None
TEST = Inet_port.of_int 88 = Some 88
TEST = Inet_port.of_int 872342 = None

module Mac_address = struct
  (* An efficient internal representation would be something like a 6 byte array,
     but let's use a hex string to get this off the ground. *)
  type t = string with sexp, bin_io
  let ( = ) = String.( = )
  let equal = ( = )
  let rex = Pcre.regexp "[^a-f0-9]"
  let of_string s =
    let addr = String.lowercase s |! Pcre.qreplace ~rex ~templ:"" in
    let length = String.length addr in
    if length <> 12 then
      failwithf "MAC address '%s' has the wrong length: %d" s length ();
    addr

  let to_string t =
    let rec loop acc = function
      | a::b::rest ->
        let x = String.of_char_list [a; b] in
        loop (x :: acc) rest
      | [] -> List.rev acc |! String.concat ~sep:":"
      | _ -> assert false
    in
    loop [] (String.to_list t)

  let to_string_cisco t =
    let lst = String.to_list t in
    let a = List.take lst 4 |! String.of_char_list
    and b = List.take (List.drop lst 4) 4 |! String.of_char_list
    and c = List.drop lst 8 |! String.of_char_list in
    String.concat ~sep:"." [a; b; c]
  let t_of_sexp sexp = String.t_of_sexp sexp |! of_string
  let sexp_of_t t = to_string t |! String.sexp_of_t

  let _flag = Command.Spec.Arg_type.create of_string
end

TEST = Mac_address.to_string (Mac_address.of_string "00:1d:09:68:82:0f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string (Mac_address.of_string "00-1d-09-68-82-0f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string (Mac_address.of_string "001d.0968.820f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string_cisco (Mac_address.of_string "00-1d-09-68-82-0f") = "001d.0968.820f"


module Quota = struct

  type bytes  = Int63.t with sexp
  type inodes = Int63.t with sexp

  let bytes  x = x
  let inodes x = x

  type 'units limit = {
    soft  : 'units sexp_option;
    hard  : 'units sexp_option;
    grace : Time.t sexp_option;
  } with sexp

  type 'units usage = private 'units

  (* None is encoded as zero *)
  type 'units c_limit = {
    c_soft  : 'units;
    c_hard  : 'units;
    c_grace : Time.t;
  }

  let zero_bytes  = bytes  Int63.zero
  let zero_inodes = inodes Int63.zero

  let ml_limit_of_c_limit ~zero { c_soft; c_hard; c_grace } =
    { soft  = (if c_soft = zero then None else Some c_soft);
      hard  = (if c_hard = zero then None else Some c_hard);
      grace = (if c_grace = Time.epoch then None else Some c_grace); }

  let c_limit_of_ml_limit ~zero { soft; hard; grace } =
    { c_soft  = (match soft  with None -> zero | Some x -> x);
      c_hard  = (match hard  with None -> zero | Some x -> x);
      c_grace = (match grace with None -> Time.epoch | Some x -> x); }

  external quota_query
    : [ `User | `Group ]
    -> id:int
    -> path:string
    -> ( bytes c_limit * bytes usage * inodes c_limit * inodes usage)
    = "quota_query"

  external quota_modify
    : [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes  c_limit
    -> inodes c_limit
    -> unit
    = "quota_modify"

  let query user_or_group ~id ~path =
    try
      let blimit, busage, ilimit, iusage = quota_query user_or_group ~id ~path in
      Ok (ml_limit_of_c_limit ~zero:zero_bytes blimit, busage,
          ml_limit_of_c_limit ~zero:zero_inodes ilimit, iusage)
    with Unix.Unix_error _ as exn ->
      Or_error.of_exn exn

  let set user_or_group ~id ~path byte_limit inode_limit =
    try
      Ok (quota_modify user_or_group ~id ~path
            (c_limit_of_ml_limit ~zero:zero_bytes byte_limit)
            (c_limit_of_ml_limit ~zero:zero_inodes inode_limit))
    with Unix.Unix_error _ as exn ->
      Or_error.of_exn exn
end

module Mount_entry = struct
  (* see: man 3 getmntent *)
  type t = {
    fsname     : string;
    directory  : string;
    fstype     : string;
    options    : string;
    dump_freq  : int sexp_option;
    fsck_pass  : int sexp_option;
  } with sexp, fields

  let escape_seqs = [ "040", " " ;
                      "011", "\t";
                      "012", "\n";
                      "134", "\\";
                      "\\",  "\\"; ]
  let rec unescape s =
    match String.lsplit2 s ~on:'\\' with
    | None -> s
    | Some (l, r) ->
      match
        List.find_map escape_seqs ~f:(fun (prefix, replacement) ->
          Option.map (String.chop_prefix ~prefix r)
            ~f:(fun r -> l ^ replacement ^ unescape r))
      with
      | None -> l ^ "\\" ^ unescape r
      | Some ret -> ret

  let parse_optional_int = function
    | "0" -> None
    |  s  -> Some (Int.of_string s)

  let parse_line line =
    if String.is_empty line then Ok None
    else if line.[0] = '#' then Ok None
    else
      match
        List.map ~f:unescape
          (String.split_on_chars ~on:[' ';'\t'] (String.strip line))
      with
      | [] | [""] -> Ok None
      | fsname :: directory :: fstype :: options
        :: ([] | [_] | [_;_] as dump_freq_and_fsck_pass) ->
        begin
          let dump_freq, fsck_pass =
            match dump_freq_and_fsck_pass with
            | [                    ] -> None,           None
            | [dump_freq           ] -> Some dump_freq, None
            | [dump_freq; fsck_pass] -> Some dump_freq, Some fsck_pass
            | _ -> assert false
          in
          try
            let dump_freq = Option.bind dump_freq parse_optional_int in
            let fsck_pass = Option.bind fsck_pass parse_optional_int in
            if String.equal fstype "ignore"
            then Ok (None)
            else Ok (Some { fsname; directory; fstype;
                            options; dump_freq; fsck_pass })
          with exn ->
            Or_error.of_exn exn
        end
      | _ -> Or_error.error "wrong number of fields" line String.sexp_of_t

  let visible_filesystem ts =
    let add_slash = function
      | "" -> "/"
      | p  -> if p.[String.length p - 1] = '/' then p else p ^ "/"
    in
    let overlay map t =
      let remove_prefix = add_slash (directory t) in
      let rec loop map =
        match String.Map.next_key map remove_prefix with
        | None ->
          map
        | Some (key, _) ->
          if not (String.is_prefix ~prefix:remove_prefix key) then
            map
          else
            loop (String.Map.remove map key)
      in
      String.Map.add (loop map) ~key:(directory t) ~data:t
    in
    List.fold ts ~init:String.Map.empty ~f:(fun map t ->
      if not (String.is_prefix ~prefix:"/" (directory t)) then
        map
      else
        overlay map t)
end
