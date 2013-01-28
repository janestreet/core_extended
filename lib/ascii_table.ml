open Core.Std

let list_sum ~f lst = List.fold lst ~init:0 ~f:(fun a b -> a + (f b))
let list_max ~f lst = List.fold lst ~init:0 ~f:(fun a b -> max a (f b))

module Art = struct (* Used when drawing the table. *)
  type t = Console.Ansi.attr list * string
  let none = ([], "")
  let spaces num_spaces = String.make num_spaces ' '
  let left ~spacing = ([], "|"^(spaces spacing))
  let mid ~spacing = ([], (spaces spacing)^"|"^(spaces spacing))
  let right_newline ~spacing = ([], (spaces spacing)^"|\n")

  let top w = ([], "|"^(String.make (w-1) '-')^"|\n")
  let mid_plusses ~spacing widths = match List.map widths ~f:((+)(spacing*2)) with
    | w1::ws_right ->
      let str_right w = "+"^(String.make w '-') in
      let str = (String.make w1 '-')
        ^(String.concat (List.map ~f:str_right ws_right))
      in
      ([], "|"^str^"|\n")
    | [] -> raise (Failure "Need at least one column to draw midline with plusses.")
  ;;
  let bottom w = ([], "|"^(String.make (w-1) '-')^"|\n")

  (* Add a list of attributes to a pirce of art. *)
  let style (old_attrs, str) attrs = (attrs @ old_attrs, str)

  let emptyp (_, str) = String.length str = 0
  let trail (attrs, str) = (* "abcdef" -> "abc..." *)
    let strlen = String.length str in
    attrs,
    if strlen < 3
    then String.make strlen '.'
    else (String.mapi str ~f:(fun i c -> if (strlen-i) <= 3 then '.' else c))
  ;;
end

module El = struct (* One element in the table. *)
  type t = Console.Ansi.attr list * string list
  type row = t list
  type grid = row list

  let create attr str = (attr, String.split ~on:'\n' str)
  let width (_, lines) = list_max ~f:String.length lines
  let height width (_, lines) =
    list_sum lines ~f:(fun s -> max (((String.length s)+(width-1))/width) 1)
  ;;

  let rec slice width (alst, lines) desired_row align_func = match lines with
    | [] -> [([], String.make width ' ')]
    | visual_row::remaining_rows ->
      let vrow_len = String.length visual_row in
      let num_vrows = (max ((vrow_len - 1) / width) 0)+1 in
      if num_vrows <= desired_row
      then slice width (alst, remaining_rows) (desired_row-num_vrows) align_func
      else let pos = desired_row*width in
           let len = min width (vrow_len - pos) in
           align_func
             alst
             (if (pos < vrow_len) then (String.sub visual_row ~pos ~len) else "")
             (if len < width then (min width (width - len)) else 0)
  ;;
end

module Align = struct
  type t = Console.Ansi.attr list -> string -> int -> Art.t list
  let left alst text num_spaces = [(alst, text); ([], String.make num_spaces ' ')]
  let right alst text num_spaces = [([], String.make num_spaces ' '); (alst, text)]
  let center alst text num_spaces =
    let num_left = num_spaces/2 in
    let num_right = num_spaces - num_left in
    [([], String.make num_left ' '); (alst, text); ([], String.make num_right ' ')]
  ;;
end

module Column = struct
  type 'a t = {
    max_text_width : int;
    header : string;
    col_func : 'a -> El.t;
    align_func : Align.t;
    min_width : int option;
  }

  type constraints = {
    total_width : int;
    min_widths : (string * int) list;
  } with sexp
  exception Impossible_table_constraints of constraints with sexp

  let create_attr ?(align=Align.left) ?min_width ?(max_width=90)
      str parse_func = {
    max_text_width = max_width+1;
    header = str;
    col_func = (fun x -> match parse_func x with (a, b) -> El.create a b);
    align_func = align;
    (* We add one for the '|' on the left. *)
    min_width = Option.map min_width ~f:((+)1);
  };;

  let create ?(align=Align.left) ?min_width ?(max_width=90)
      str parse_func =
    create_attr ?min_width ~align ~max_width str (fun x -> [], (parse_func x))
  ;;
  let header_to_el alst t = El.create alst t.header
  let make col_val t = t.col_func col_val

  let desired_width ~spacing data t  =
    let column_data = List.map data ~f:t.col_func in
    (* We need to account for the '|' to the left, so we add 1 plus the spacing
       on either side. *)
    1 + (2*spacing)
    + (min (t.max_text_width - (1+(2*spacing)))
         (max (String.length t.header) (list_max column_data ~f:El.width)))
  ;;

  let layout ~spacing table_width ts data =
    let desired_widths = List.map ts ~f:(desired_width ~spacing data) in
    let all_min_width = List.filter_map ts ~f:(fun t -> t.min_width) in

    (* The minimum number of characters for a column that doesn't have an
       [min_width] value. *)
    let generic_min_chars =
      let width = table_width - (list_sum all_min_width ~f:Fn.id) in
      let len = (List.length ts) - (List.length all_min_width) in
      (width/len)
    in
    if generic_min_chars < 1+(1+(spacing*2)) then
      raise (Impossible_table_constraints {
        total_width = (table_width+1);
        min_widths = (List.filter_map ts ~f:(fun t ->
          Option.map t.min_width ~f:(fun num_chars -> (t.header, num_chars))));
      });


    let left = ref ((list_sum ~f:Fn.id desired_widths) - table_width) in
    let stop = ref false in
    (* This layout algorithm looks unbearably inefficient, but it's
       simple and works reasonably well in the common case. *)
    let rec decide_widths desired_widths =
      if !stop then desired_widths else begin
        stop := true;
        assert(List.length ts = List.length desired_widths);
        decide_widths (List.map2_exn ts desired_widths ~f:(fun t column_width ->
          let min_chars = match t.min_width with
            | Some x -> x
            | None -> generic_min_chars
          in
          if (column_width <= min_chars || !left <= 0) then column_width else
            (left := !left - 1; stop := false; column_width - 1)))
      end
    in
    (* The widths used in [loop] include the '|' to the left of each element,
       which isn't important after layout, so we subtract off 1 and the spacing
       on either side. *)
    List.map ~f:(fun x -> x-(1+(spacing*2))) (decide_widths desired_widths)
  ;;
end

module Grid = struct
  type t = {
    data: El.grid;
    heights: int list;
    widths: int list;
    align_funcs: Align.t list;
  }

  let create ~spacing max_width h_attr cols raw_data =
    (* We subtract 1 from max_width because later we're going to add a line of
       '|'s to form the right wall of the table. *)
    let widths = Column.layout ~spacing (max_width-1) cols raw_data in
    let body = List.map raw_data ~f:(fun x -> List.map cols ~f:(Column.make x)) in
    let grid_data = (List.map cols ~f:(Column.header_to_el h_attr))::body in
    let heights = List.map grid_data ~f:(fun row ->
      assert(List.length widths = List.length row);
      list_max ~f:Fn.id (List.map2_exn widths row ~f:El.height))
    in
    let align_funcs = List.map cols ~f:(fun c -> c.Column.align_func) in
    {data = grid_data; heights = heights; widths = widths; align_funcs=align_funcs}
  ;;

  let draw ~spacing ~display t =
    (* The total width of the table includes the '|'s to the left of
       elements, so we add 1 and the spacing on either side when summing. *)
    let width = list_sum t.widths ~f:((+)(1+(spacing*2))) in
    let to_draw = Doubly_linked.create() in
    (* [add] adds an (attr,string) pair to the list of things to draw *)
    let add x = Fn.ignore (Doubly_linked.insert_last to_draw x) in
    add (Art.top width);
    assert(List.length t.data = List.length t.heights);
    List.iteri (List.zip_exn t.data t.heights)
      ~f:(display ~spacing ~add ~grid:t);
    add (Art.bottom width);
    Doubly_linked.to_list to_draw;
  ;;
end

module Display = struct
  open Grid
  type t = spacing:int -> add:(Art.t -> unit)
    -> grid:Grid.t -> int -> El.row *  int -> unit

  let box_gen ~spacing ~add ~grid ~art_mid index (row, height) =
    begin match index with
    | 0 -> ()
    | 1 -> add (Art.mid_plusses ~spacing grid.widths)
    | _ -> add art_mid
    end;
    let add_visual_row visual_row =
      assert(List.length row = List.length grid.widths
            && List.length grid.widths = List.length grid.align_funcs);
      List.iter2_exn row (List.zip_exn grid.widths grid.align_funcs)
        ~f:(fun el (column_width, align_func) ->
          let art =
            if not (List.is_empty row) && phys_equal el (List.hd_exn row)
            then Art.left ~spacing
            else Art.mid ~spacing
          in
          add (Art.style art (if (index mod 2 = 1) then [`Blue] else []));
          List.iter ~f:add (El.slice column_width el visual_row align_func));
      add (Art.right_newline ~spacing);
    in
    for i=0 to (height-1) do add_visual_row i done;
  ;;

  let tall_box ~spacing ~add ~grid index row_height =
    box_gen ~art_mid:(Art.mid_plusses ~spacing grid.widths)
      ~spacing ~add ~grid index row_height
  ;;
  let short_box ~spacing ~add ~grid index row_height =
    box_gen ~art_mid:(Art.none)
      ~spacing ~add ~grid index row_height
  ;;

  let line ~spacing ~add ~grid index (row, _) =
    assert(List.length row = List.length grid.widths
          && List.length grid.widths = List.length grid.align_funcs);
    List.iter2_exn row (List.zip_exn grid.widths grid.align_funcs)
      ~f:(fun el (column_width, align_func) ->
        let art =
          if not (List.is_empty row) && phys_equal el (List.hd_exn row)
          then Art.left ~spacing
          else Art.mid ~spacing
        in
        add (Art.style art (if (index mod 2 = 1) then [`Blue] else []));
        let to_add = El.slice column_width el 0 align_func in
        List.iter ~f:add
          begin match List.filter to_add ~f:(Fn.non Art.emptyp) with
          | [x] ->
            if El.height column_width el > 1
            then [Art.trail x]
            else [x]
          | _::_ -> to_add
          | [] -> raise (Failure "El.slice returned all empty strings!")
          end);
    add (Art.right_newline ~spacing);
    if not (List.is_empty grid.data) && phys_equal row (List.hd_exn grid.data)
    then add (Art.mid_plusses ~spacing grid.widths);
  ;;
end

let console_print outc (attrs, str) = Console.Ansi.output_string attrs outc str

type ('a,'rest) renderer =
  ?display : Display.t (* Default: short_box *)
  -> ?spacing : int (* Default: 1 *)
  -> ?limit_width_to : int (* defaults to 90 characters *)
  -> ?header_attr : Console.Ansi.attr list
  -> 'a Column.t list
  -> 'a list
  -> 'rest

let output ?(display=Display.short_box) ?(spacing=1) ?(limit_width_to=90)
    ?(header_attr=[]) cols data ~oc:outc =
  if cols = [] then () else
    List.iter ~f:(console_print outc)
      (Grid.draw ~spacing ~display
         (Grid.create ~spacing limit_width_to header_attr cols data))
;;

let to_string_gen ?(display=Display.short_box) ?(spacing=1) ?(limit_width_to=90)
    ?(header_attr=[]) cols data ~use_attr =
  let apply_attrs (attrs,text) =
    if use_attr then Console.Ansi.string_with_attr attrs text
    else text
  in
  if cols = [] then "" else
    String.concat
      (List.map ~f:apply_attrs
         (Grid.draw ~spacing ~display
            (Grid.create ~spacing limit_width_to header_attr cols data)))
;;


let to_string_noattr = to_string_gen ~use_attr:false
let to_string        = to_string_gen ~use_attr:true
