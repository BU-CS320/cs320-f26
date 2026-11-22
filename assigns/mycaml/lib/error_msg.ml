open Utils

type t =
  {
    line_num: int;
    col_num: int;
    underline_width: int;
    filename: string;
    message: string;
    dummy: bool;
  }

let get_line text i =
  text
  |> String.split_on_char '\n'
  |> Fun.flip List.nth (i - 1)

let mk (loc : Loc.t) message =
  let lloc = Loc.first loc in
  let rloc = Loc.last loc in
  let line_num = lloc.pos_lnum in
  let col_num = lloc.pos_cnum - lloc.pos_bol in
  let underline_width =
    if lloc.pos_lnum = rloc.pos_lnum
    then (rloc.pos_cnum - rloc.pos_bol) - col_num
    else 1
  in
  let filename = lloc.pos_fname in
  {
    line_num;
    col_num;
    underline_width;
    filename;
    message;
    dummy = loc = Loc.dummy;
  }

let to_string ?filename ?text t =
  let filename = Option.value filename ~default:t.filename in
  let filename =
    if filename <> "" && filename.[0] = '/'
    then filename
    else "./" ^ filename
  in
  let err_msg_first_line =
    Printf.sprintf "File \"%s\", line %d, characters %d-%d:"
      filename
      t.line_num
      t.col_num
      (t.col_num + t.underline_width)
  in
  if t.dummy
  then err_msg_first_line
  else
    let line_num_width = String.length (string_of_int t.line_num) in
    match text with
    | None -> err_msg_first_line
    | Some text ->
      let line = get_line text t.line_num in
      let err_msg_second_line = Printf.sprintf "%d | %s" t.line_num line in
      let err_msg_third_line =
        Printf.sprintf
          "%s %s%s"
          (String.make (line_num_width + 2) ' ')
          (String.make t.col_num ' ')
          (String.make t.underline_width '^')
      in
      let err_msg_fourth_line =
        Printf.sprintf
          "Error: %s"
          t.message
      in
      String.concat "\n"
        [
          err_msg_first_line;
          err_msg_second_line;
          err_msg_third_line;
          err_msg_fourth_line;
        ]
