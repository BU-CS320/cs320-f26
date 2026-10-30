open Utils

module Token = struct
  type t = Lpar | Rpar | Word of string

  let list_of_string s =
    let at_end_of_word i =
      i >= String.length s
      || match s.[i] with | '(' | ')' -> true | c -> is_ws c
    in
    let rec go acc i =
      if i >= String.length s
      then List.rev acc
      else
        match s.[i] with
        | '(' -> go (Lpar :: acc) (i + 1)
        | ')' -> go (Rpar :: acc) (i + 1)
        | c ->
           if is_ws c
           then go acc (i + 1)
           else go' acc i 1
    and go' acc start len =
      let i = start + len in
      if at_end_of_word i
      then go (Word (String.sub s start len) :: acc) i
      else go' acc start (len + 1)
    in go [] 0
end

type t =
  | Atom of string
  | List of t list

let of_tokens_opt ts =
  let rec go ts =
    match ts with
    | Token.Lpar :: ts ->
       begin
         match go' ts with
         | es, Token.Rpar :: ts -> Some (List es, ts)
         | _ -> None
       end
    | Word w :: ts -> Some (Atom w, ts)
    | _ -> None
  and go' ts =
    match go ts with
    | None -> [], ts
    | Some (e, ts) ->
       let es, ts = go' ts in
       e :: es , ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

let of_string_opt s = s |> Token.list_of_string |> of_tokens_opt

let of_string s =
  match of_string_opt s with
  | Some se -> se
  | None -> raise (Failure "Sexpr.of_string")
