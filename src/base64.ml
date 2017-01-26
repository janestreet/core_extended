open Core
open Expect_test_helpers

module type T = sig
  val encode : string -> string
  val decode : string -> string
end

module Make(D : sig
    val char62 : char
    val char63 : char
    val pad_char : [ `None
                   | `Suggested of char
                   | `Required of char ]
    val ignore_char : char -> bool
  end) = struct

  let pad_char, allow_incorrect_padding =
    match D.pad_char with
    | `None -> None, true
    | `Suggested c -> Some c, true
    | `Required c -> Some c, false

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
        match String.length source % 3, pad_char with
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
    let unexpected_char char =
      raise_s [%message "Unexpected character while base64 decoding"
                          (char : char) (source : string) ]
    in
    let incomplete () =
      raise_s [%message "Base64 encoded string is unexpectedly terminated"
                          (source : string) ]
    in
    let fail_unless_allow_incorrect_padding () =
      if not allow_incorrect_padding then
        raise_s [%message "Base64 encoded string has incorrect padding"
                            (source : string) ]
    in
    (* 3 extra bytes because the base64 encoding may have no padding, but we go through
       [loop] one extra time *)
    let dest = Bytes.create ((Bytes.length source / 4 + 1) * 3) in
    let rec read i =
      if i < String.length source then begin
        let c = String.get source i in
        if D.ignore_char c then
          read (i+1)
        else if Some c = pad_char then
          `Pad, i+1
        else
          `Ok (decode_char c), i+1
      end else `End, i
    in
    let rec stop ~i =
      if i < String.length source then begin
        let c = String.get source i in
        if D.ignore_char c || ((Some c = pad_char) && allow_incorrect_padding) then
          stop ~i:(i+1)
        else
          unexpected_char c
      end;
    in
    let rec loop ~i ~j =
      let a,i = read i in
      let b,i = read i in
      let c,i = read i in
      let d,i = read i in
      let a,b,c,d,n = match a,b,c,d with
        (* 0/1/2 bytes of padding *)
        | `Ok a, `Ok b, `Ok c, `Ok d -> a,b,c,d,3
        | `Ok a, `Ok b, `Ok c, `Pad  -> a,b,c,0,2
        | `Ok a, `Ok b, `Pad,  `Pad  -> a,b,0,0,1
        (* Missing 1/2 bytes of padding *)
        | `Ok a, `Ok b, `Ok c, `End  ->
          fail_unless_allow_incorrect_padding ();
          a,b,c,0,2
        | `Ok a, `Ok b, `End,  `End  ->
          fail_unless_allow_incorrect_padding ();
          a,b,0,0,1
        (* Incorrect padding *)
        | `Pad, _, _, _ ->
          fail_unless_allow_incorrect_padding ();
          0,0,0,0,0
        (* End of input, no padding *)
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
    let pad_char = `Suggested '='
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

let%expect_test "too little padding" =
  printf "%s" (decode "Zg");
  [%expect_exact {|f|}]

let%expect_test "too much padding" =
  printf "%s" (decode "Zg===");
  [%expect_exact {|f|}]

let%expect_test "too much padding" =
  printf "%s" (decode "aGlp====");
  [%expect_exact {|hii|}]

let%expect_test "characters after padding" =
  printf "%s" (decode "aGlp=ZZZ");
  [%expect_exact {|hii|}]

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

module Padding_sensitive = struct
  include Make(struct
      let char62 = '+'
      let char63 = '/'
      let pad_char = `Required '='
      let ignore_char = Char.is_whitespace
    end)

  let%expect_test "too little padding" =
    show_raise (fun () -> printf "%s" (decode "Zg"));
    [%expect {|
        (raised ("Base64 encoded string has incorrect padding" (source Zg)))
    |}]

  let%expect_test "too much padding" =
    show_raise (fun () -> printf "%s" (decode "Zg==="));
    [%expect {|
        (raised (
          "Unexpected character while base64 decoding"
          (char   =)
          (source Zg===)))
    |}]
end

module Websafe = struct
  include Make(struct
      let  char62 = '-'
      let char63 = '_'
      let pad_char = `None
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

let%test_unit "Samples that have been validated against a reference implementation (cryptokit)" =
  List.iter
    ~f:(fun (str, reference_encoded) ->
      let encoded = encode str in
      assert (String.equal encoded reference_encoded);
      let decoded = decode encoded in
      assert (String.equal decoded str))
    [ "\198" , "xg=="
    ; "!F" , "IUY="
    ; "04H" , "MDRI"
    ; "NF5G" , "TkY1Rw=="
    ; "uMLrF" , "dU1MckY="
    ; "Sh7I65" , "U2g3STY1"
    ; "KT~yakA" , "S1R+eWFrQQ=="
    ; "wQT3\2357pK" , "d1FUM+s3cEs="
    ; "\2516=d)4\134UY" , "+zY9ZCk0hlVZ"
    ; "4c1Unx]uX\151" , "NGMxVW54XXVYlw=="
    ; "MfkL9aiTCGS" , "TWZrTDlhaVRDR1M="
    ; "4HoDGZz6Lvdx" , "NEhvREdaejZMdmR4"
    ; "ac3\241j6GJvFHZp" , "YWMz8Wo2R0p2RkhacA=="
    ; "6e3IvsJ`l19u\172H" , "NmUzSXZzSmBsMTl1rEg="
    ; "L\172h\131JTQfcE3fxd`" , "TKxog0pUUWZjRTNmeGRg"
    ; "uu35\226rPEJX0oOood" , "dXUzNeJyUEVKWDBvT29vZA=="
    ; "ZP9j\1748PQ7\139IipeDIm" , "WlA5aq44UFE3i0lpcGVESW0="
    ; "J1uiOAj1WnzFu6XNyt" , "SjF1aU9BajFXbnpGdTZYTnl0"
    ; "Sj\173wTSw4vEd\148RWBGRE&" , "U2qtd1RTdzR2RWSUUldCR1JFJg=="
    ; "SOCPe6/Ol1H3cbMkKfKc" , "U09DUGU2L09sMUgzY2JNa0tmS2M="
    ; "q0DhS\21884Sa21QwtkROBX7" , "cTBEaFPaODRTYTIxUXd0a1JPQlg3"
    ; "bgyoLSUC9V\1355\211emdcEME3e" , "Ymd5b0xTVUM5Voc102VtZGNFTUUzZQ=="
    ; "se4a66;K5cVuHo\250lwP9Cdt8" , "c2U0YTY2O0s1Y1Z1SG/6bHdQOUNkdDg="
    ; "ueTlLL\159Ey\194FO2NzKHWY1iPaS" , "dWVUbExMn0V5wkZPMk56S0hXWTFpUGFT"
    ; "fg\148wC-x4TH6a\1270iq6EbTDsBjQ" , "ZmeUd0MteDRUSDZhfzBpcTZFYlREc0JqUQ=="
    ; "2jVgNu71AI\160LPKQ2Va1\229gvN0TU" , "MmpWZ051NzFBSaBMUEtRMlZhMeVndk4wVFU="
    ; "4ViQIspFeZ5SF\209C2XU^vU7FTJV\168" , "NFZpUUlzcEZlWjVTRtFDMlhVXnZVN0ZUSlao"
    ; "PFkmhDzyz?K7Z8SYI7sqjDottmly" , "UEZrbWhEenl6P0s3WjhTWUk3c3FqRG90dG1seQ=="
    ; "Ub&ApeP3sYY9qHnqnKQ\152kXYbreIAG" , "VWImQXBlUDNzWVk5cUhucW5LUZhrWFlicmVJQUc="
    ; "hEjk,9TMVZG8N\187f\0213MtyuOJIsaa\031Pr" , "aEVqayw5VE1WWkc4TrtmFTNNdHl1T0pJc2FhH1By"
    ; "k>23KZqW53BHG0W4bYeh%rI9tFCfPgC" , "az4yM0tacVc1M0JIRzBXNGJZZWglckk5dEZDZlBnQw=="
    ; "A" , "QQ=="
    ; "hO" , "aE8="
    ; "0Dv" , "MER2"
    ; "q2Kf" , "cTJLZg=="
    ; "5(adQ" , "NShhZFE="
    ; "Terfzh" , "VGVyZnpo"
    ; "3w0D\1492v" , "M3cwRJUydg=="
    ; "gKAU+Jlu" , "Z0tBVStKbHU="
    ; "ZS9d4HqJE" , "WlM5ZDRIcUpF"
    ; "r\0079qTw\028H7l" , "cgc5cVR3HEg3bA=="
    ; "-N0xybPdB\249n" , "LU4weHliUGRC+W4="
    ; "Pio0X59ZC^p7" , "UGlvMFg1OVpDXnA3"
    ; "W4190Mt\1451L\222np" , "VzQxOTBNdJExTN5ucA=="
    ; "sjK67+/nsckrHI" , "c2pLNjcrL25zY2tySEk="
    ; "f01f5TMpmmAtUa6" , "ZjAxZjVUTXBtbUF0VWE2"
    ; "gGmrG\185KP2u9a5m0B" , "Z0dtcke5S1AydTlhNW0wQg=="
    ; "YENPfihs\130RIWMp$p\\" , "WUVOUGZpaHOCUklXTXAkcFw="
    ; "qBTJwIR#0bizDd0oow" , "cUJUSndJUiMwYml6RGQwb293"
    ; "Hh4sq1R5sXZuary8bS\029" , "SGg0c3ExUjVzWFp1YXJ5OGJTHQ=="
    ; "kDiz3nGVpqiERkIZ2U4X" , "a0RpejNuR1ZwcWlFUmtJWjJVNFg="
    ; "B2KlIfFz4pqJx{6ZMnfXt" , "QjJLbElmRno0cHFKeHs2Wk1uZlh0"
    ; "FQEfMw\178CX7X5N\131QmIK:+qm" , "RlFFZk13skNYN1g1ToNRbUlLOitxbQ=="
    ; "KA1aDTQbWb.7wD7aj1ZEy\207P" , "S0ExYURUUWJXYi43d0Q3YWoxWkV5z1A="
    ; "wxlyeI64fHvngQ^D7DTP58Te" , "d3hseWVJNjRmSHZuZ1FeRDdEVFA1OFRl"
    ; "WpoIAyUeCYav8b\187ady\1288r\161f4\209" , "V3BvSUF5VWVDWWF2OGK7YWR5gDhyoWY00Q=="
    ; "T\201o8|wGWNT\1867rLcMe1IyVp02fr" , "VMlvOHx3R1dOVLo3ckxjTWUxSXlWcDAyZnI="
    ; "095xnOoBK1hMDTxkwCMB9:eH4vf" , "MDk1eG5Pb0JLMWhNRFR4a3dDTUI5OmVINHZm"
    ; "g9c9JF$8\233agVmIg\236t\150BJ48nmLJV\219" , "ZzljOUpGJDjpYWdWbUln7HSWQko0OG5tTEpW2w=="
    ; "Jxfv8P\135b7IuZ8LhgcOC0FSULb\2011wn" , "SnhmdjhQh2I3SXVaOExoZ2NPQzBGU1VMYskxd24="
    ; "SxhqH4a3mB\184RNgmsJR6pGTg-1\2132Dr6" , "U3hocUg0YTNtQrhSTmdtc0pSNnBHVGctMdUyRHI2"
    ; "\"B&XGT1Dg3G1nlS0z@B\129Z\195`bJO\136wwm1" , "IkImWEdUMURnM0cxbmxTMHpAQoFaw2BiSk+Id3dtMQ=="
    ; "J" , "Sg=="
    ; "ja" , "amE="
    ; "IAU" , "SUFV"
    ; "\157\137se" , "nYlzZQ=="
    ; "pmrsi" , "cG1yc2k="
    ; "x\182OH (" , "eLZPSCAo"
    ; "wsD2r5d" , "d3NEMnI1ZA=="
    ; "\148honhcWJ" , "lGhvbmhjV0o="
    ; "0kFupp\130NA" , "MGtGdXBwgk5B"
    ; "8q9RJ4D0\239R" , "OHE5Uko0RDDvUg=="
    ; "B4aO2CFN9SW" , "QjRhTzJDRk45U1c="
    ; "u\2352vgi\030H\\gGf" , "desydmdpHkhcZ0dm"
    ; "2PlbwEnFxdRJq" , "MlBsYndFbkZ4ZFJKcQ=="
    ; "4m%j\160ejo4Y00b8" , "NG0laqBlam80WTAwYjg="
    ; "Du\180XUJJK\2361SAwAU" , "RHW0WFVKSkvsMVNBd0FV"
    ; "Gq5h5tj7oGJNeI3T" , "R3E1aDV0ajdvR0pOZUkzVA=="
    ; "iA\204b7JRSRE0sa\133<lB" , "aUHMYjdKUlNSRTBzYYU8bEI="
    ; "WAb5U\\G7X:bFrsymPf" , "V0FiNVVcRzdYOmJGcnN5bVBm"
    ; "6kxiA1GMA1Q69fx:h0\165" , "Nmt4aUExR01BMVE2OWZ4OmgwpQ=="
    ; "eSFtN7pCtdqoh1Vywioo" , "ZVNGdE43cEN0ZHFvaDFWeXdpb28="
    ; "iwKrIl_39APRHagaOlvmD" , "aXdLcklsXzM5QVBSSGFnYU9sdm1E"
    ; "\016Brf4W6jDkADGaSLZl0t5L" , "EEJyZjRXNmpEa0FER2FTTFpsMHQ1TA=="
    ; "Ks77q8F7eIdiV6c9V~DYSBA" , "S3M3N3E4RjdlSWRpVjZjOVZ+RFlTQkE="
    ; "KItSmn3)3LUuCI9E8H08aFDB" , "S0l0U21uMykzTFV1Q0k5RThIMDhhRkRC"
    ; "H:jsaqJb#W\255VI\199civ7aV8pJOM" , "SDpqc2FxSmIjV/9WScdjaXY3YVY4cEpPTQ=="
    ; "E0\"SlC0mUlST\020alI\030i5v8qemoz" , "RTAiU2xDMG1VbFNUFGFsSR5pNXY4cWVtb3o="
    ; "aGX5mLm\007BwXo9vvQ0T\199ikUbLeb\191" , "YUdYNW1MbQdCd1hvOXZ2UTBUx2lrVWJMZWK/"
    ; "hOFc6wBFqXBtv\186V5MqeqzgOX8uVp" , "aE9GYzZ3QkZxWEJ0drpWNU1xZXF6Z09YOHVWcA=="
    ; "xJXc6 JBAyxetAIo4Hq9MzqToyNAu" , "eEpYYzYgSkJBeXhldEFJbzRIcTlNenFUb3lOQXU="
    ; "f4iZMK6gWY!=44\200W\190PZ VNLZGZpWt3" , "ZjRpWk1LNmdXWSE9NDTIV75QWiBWTkxaR1pwV3Qz"
    ; "yt4\192xYNyAusB77t.\202'\202at4MD5ly^Em9" , "eXQ0wHhZTnlBdXNCNzd0LsonymF0NE1ENWx5XkVtOQ=="
    ; "c" , "Yw=="
    ; "XD" , "WEQ="
    ; "\141G8" , "jUc4"
    ; "qlef" , "cWxlZg=="
    ; "nH\127lm" , "bkh/bG0="
    ; "t6aTVY" , "dDZhVFZZ"
    ; "R7ZNaHo" , "UjdaTmFIbw=="]
;;

let%test_unit "Round trips" =
  Quickcheck.test
    String.gen
    ~sexp_of:[%sexp_of: string]
    ~f:(fun string ->
      let encoded = encode string in
      let decoded = decode encoded in
      [%test_eq: string ] string decoded)
;;
