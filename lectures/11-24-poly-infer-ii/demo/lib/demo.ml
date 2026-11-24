include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec eval (env : env) (e : expr) : value =
  let rec go e =
    match e with
    | Var x -> Env.find x env
    | Num n -> VNum n
    | Bool b -> VBool b
    | Let(x, e1, e2) -> eval (Env.add x (go e1) env) e2
    | App (e1, e2) -> (
      match go e1 with
      | VClos (x, e, env') -> eval (Env.add x (go e2) env') e
      | _ -> assert false
    )
    | Fun (x, e) -> VClos (x, e, env)
    | Add (e1, e2) -> (
      match go e1 , go e2 with
      | VNum n1, VNum n2 -> VNum (n1 + n2)
      | _ -> assert false
    )
    | Eq (e1, e2) -> (
      match go e1 , go e2 with
      | VNum n1, VNum n2 -> VNum (n1 + n2)
      | _ -> assert false
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | VBool b -> if b then go e2 else go e3
      | _ -> assert false
    )
  in go e

let rec eval_prog env prog =
  match prog with
  | [] -> env
  | (name, binding) :: p ->
     eval_prog (Env.add name (eval env binding) env) p


let rec nest_lets = function
  | [] -> Num 0
  | (name, binding) :: [] -> Let (name, binding, Var name)
  | (name, binding) :: p ->
    Let (name, binding, nest_lets p)

let interp (s : string) =
  match parse s with
  | Some p -> eval_prog Env.empty p |> Env.to_list
  | None -> raise (Failure "Parse error")
