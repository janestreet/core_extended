open Core.Std


module type T = sig
  val encode : string -> string
  val decode : string -> string
end

module Make(D : sig
    (* Usually [ '+' ] *)
    val char62 : char
    (* Usually [ '/' ] *)
    val char63 : char
    (* Usually [ '=' ] *)
    val pad_char : char option
    (* Usually [ Char.is_whitespace ] *)
    val ignore_char : char -> bool

  end) = struct

  let encode_char i =
    if 0<=i && i<=25 then Char.of_int_exn (Char.to_int 'A' + i)
    else if 26<=i && i<=51 then Char.of_int_exn (Char.to_int 'a' + i - 26)
    else if 52<=i && i<=61 then Char.of_int_exn (Char.to_int '0' + i - 52)
    else if i=62 then D.char62
    else if i=63 then D.char63
    else failwithf "not representable as a base64 character: %d" i ()

  let decode_char c =
    if 'A'<=c && c<='Z' then Char.to_int c - Char.to_int 'A'
    else if 'a'<=c && c<='z' then Char.to_int c - Char.to_int 'a' + 26
    else if '0'<=c && c<='9' then Char.to_int c - Char.to_int '0' + 52
    else if c=D.char62 then 62
    else if c=D.char63 then 63
    else failwithf "not a recognized base64 character: %c" c ()

  let encode source =
    let dest = String.create (((Bytes.length source + 2 ) / 3) * 4) in
    let source_get i =
      if i < String.length source then
        String.get source i |> Char.to_int
      else
        0
    in
    let rec loop ~i ~j =
      if i < String.length source then begin
        let a = source_get (i + 0) in
        let b = source_get (i + 1) in
        let c = source_get (i + 2) in
        String.set dest (j + 0)
          Int.(                         shift_right a 2  |> bit_and 0x3f |> encode_char);
        String.set dest (j + 1)
          Int.(bit_or (shift_left a 4) (shift_right b 4) |> bit_and 0x3f |> encode_char);
        String.set dest (j + 2)
          Int.(bit_or (shift_left b 2) (shift_right c 6) |> bit_and 0x3f |> encode_char);
        String.set dest (j + 3)
          Int.(                                     c    |> bit_and 0x3f |> encode_char);
        loop ~i:(i+3) ~j:(j+4)
      end else begin
        match String.length source % 3, D.pad_char with
        | 0, _ -> dest
        | rest, None ->
          (* Remove extra bytes *)
          String.sub dest ~pos:0 ~len:(j-3+rest)
        | 1, Some fill ->
          (* Set padding *)
          String.set dest (j-2) fill;
          String.set dest (j-1) fill;
          dest
        | 2, Some fill ->
          (* Set padding *)
          String.set dest (j-1) fill;
          dest
        | _, _ -> failwith "Impossible"
      end
    in
    loop ~i:0 ~j:0

  let decode source =
    let unexpected_char c =
      failwithf "Unexpected character '%c' while base64 decoding" c ()
    in
    let incomplete () =
      failwithf "Base64 encoded string is unexpectedly terminated" ()
    in
    (* 3 extra bytes because the base64 encoding may have no padding *)
    let dest = Bytes.create ((Bytes.length source / 4 + 1) * 3) in
    let rec read i =
      if i < String.length source then begin
        let c = String.get source i in
        if D.ignore_char c then
          read (i+1)
        else if Some c = D.pad_char then
          `End, i+1
        else
          `Ok (decode_char c), i+1
      end else `End, i
    in
    let rec stop ~i =
      if i < String.length source then begin
        if D.ignore_char (String.get source i) then
          stop ~i:(i+1)
        else
          unexpected_char (String.get source i)
      end;
    in
    let rec loop ~i ~j =
      let a,i = read i in
      let b,i = read i in
      let c,i = read i in
      let d,i = read i in
      let a,b,c,d,n = match a,b,c,d with
        | `Ok a, `Ok b, `Ok c, `Ok d -> a,b,c,d,3
        | `Ok a, `Ok b, `Ok c, `End  -> a,b,c,0,2
        | `Ok a, `Ok b, `End,  `End  -> a,b,0,0,1
        | `End,  `End,  `End,  `End  -> 0,0,0,0,0
        | _ -> incomplete ()
      in
      String.set dest (j+0)
        Int.(bit_or (shift_left a 2) (shift_right b 4) |> bit_and 0xff |> Char.of_int_exn);
      String.set dest (j+1)
        Int.(bit_or (shift_left b 4) (shift_right c 2) |> bit_and 0xff |> Char.of_int_exn);
      String.set dest (j+2)
        Int.(bit_or (shift_left c 6)              d    |> bit_and 0xff |> Char.of_int_exn);
      let j = j + n in
      if n = 3 then
        loop ~i ~j
      else begin
        stop ~i;
        String.sub dest ~pos:0 ~len:j
      end
    in
    loop ~i:0 ~j:0
end

include Make(struct
    let char62 = '+'
    let char63 = '/'
    let pad_char = Some '='
    let ignore_char = Char.is_whitespace
  end)

let%test_module "RFC 4686 test vectors" =
  (module struct
    let%expect_test _ =
      printf "%s" (encode "");         [%expect_exact {||}]
    let%expect_test _ =
      printf "%s" (encode "f");        [%expect_exact {|Zg==|}]
    let%expect_test _ =
      printf "%s" (encode "fo");       [%expect_exact {|Zm8=|}]
    let%expect_test _ =
      printf "%s" (encode "foo");      [%expect_exact {|Zm9v|}]
    let%expect_test _ =
      printf "%s" (encode "foob");     [%expect_exact {|Zm9vYg==|}]
    let%expect_test _ =
      printf "%s" (encode "fooba");    [%expect_exact {|Zm9vYmE=|}]
    let%expect_test _ =
      printf "%s" (decode "");         [%expect_exact {||}]
    let%expect_test _ =
      printf "%s" (decode "Zg==");     [%expect_exact {|f|}]
    let%expect_test _ =
      printf "%s" (decode "Zm8=");     [%expect_exact {|fo|}]
    let%expect_test _ =
      printf "%s" (decode "Zm9v");     [%expect_exact {|foo|}]
    let%expect_test _ =
      printf "%s" (decode "Zm9vYg=="); [%expect_exact {|foob|}]
    let%expect_test _ =
      printf "%s" (decode "Zm9vYmE="); [%expect_exact {|fooba|}]
    let%expect_test _ =
      printf "%s" (decode "Zm9vYmFy"); [%expect_exact {|foobar|}]
  end)

let%expect_test "Ignored characters" =
  printf "%s" (decode "Z m 9vY\nmFy");
  [%expect_exact {|foobar|}]

let%expect_test "Forgiving" =
  printf "%s" (decode "Zg");
  [%expect_exact {|f|}]

let%expect_test "encode char 62" =
  printf "%s" (encode "\248");
  [%expect_exact {|+A==|}]

let%expect_test "decode char 62" =
  printf !"%{sexp:string}" (decode "+A==");
  [%expect {| "\248" |}]

let%expect_test "encode char 63" =
  printf "%s" (encode "\252");
  [%expect_exact {|/A==|}]

let%expect_test "decode char 63" =
  printf !"%{sexp:string}" (decode "/A==");
  [%expect {| "\252" |}]

let%test "Invalid characters" =
  try decode "Z_m-9vY" |> ignore; false with _ -> true

module Websafe = struct
  include Make(struct
      let  char62 = '-'
      let char63 = '_'
      let pad_char = None
      let ignore_char _ = false
    end)

  let%expect_test "encode" =
    printf "%s" (encode "ocaml");
    [%expect_exact {|b2NhbWw|}]

  let%expect_test "decode" =
    printf "%s" (decode "b2NhbWw");
    [%expect_exact {|ocaml|}]

  let%expect_test "encode char 62" =
    printf "%s" (encode "\248");
    [%expect_exact {|-A|}]

  let%expect_test "decode char 62" =
    printf !"%{sexp:string}" (decode "-A");
    [%expect {| "\248" |}]

  let%expect_test "encode char 63" =
    printf "%s" (encode "\252");
    [%expect_exact {|_A|}]

  let%expect_test "decode char 63" =
    printf !"%{sexp:string}" (decode "_A");
    [%expect {| "\252" |}]

  let%test "Invalid characters" =
    try decode "Z m\n9vY" |> ignore; false with _ -> true
end
