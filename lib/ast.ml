module Ast = struct
  type number = float
  type boolean = bool
  type unary_op = Neg | Sin | Cos | Not
  type binary_op = Add | Sub | Mul | Div | Mod
  type logic_op = Gt | Gte | Lt | Lte | Eq
  type coordinate = X | Y
  type compile = Random | Rule of char

  type expr =
    | Const of number
    | Coord of coordinate
    | Bool of boolean
    | UnOp of unary_op * expr
    | BinOp of binary_op * expr * expr
    | Comp of logic_op * expr * expr
    | Ite of expr * expr * expr
    | Triple of expr * expr * expr
    | Comptime of compile

  let add a b = BinOp (Add, a, b)
  let sub a b = BinOp (Sub, a, b)
  let mul a b = BinOp (Mul, a, b)
  let div a b = BinOp (Div, a, b)
  let emod a b = BinOp (Mod, a, b)

  exception SyntaxError of string

  let rec string_of_expr (expr : expr) : string =
    match expr with
    | Const n -> string_of_float n
    | Bool b -> string_of_bool b
    | Coord c -> ( match c with X -> "x" | Y -> "y")
    | UnOp (op, a) -> (
        match op with
        | Neg -> Printf.sprintf "-(%s)" (string_of_expr a)
        | Sin -> Printf.sprintf "sin(%s)" (string_of_expr a)
        | Cos -> Printf.sprintf "cos(%s)" (string_of_expr a)
        | Not -> Printf.sprintf "!(%s)" (string_of_expr a))
    | BinOp (op, a, b) ->
        let op_str =
          match op with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Mod -> "%"
        in
        Printf.sprintf "(%s %s %s)" (string_of_expr a) op_str (string_of_expr b)
    | Comp (op, a, b) ->
        let op_str =
          match op with
          | Gt -> ">"
          | Gte -> ">="
          | Lt -> "<"
          | Lte -> "<="
          | Eq -> "=="
        in
        Printf.sprintf "(%s %s %s)" (string_of_expr a) op_str (string_of_expr b)
    | Ite (i, t, e) ->
        Printf.sprintf "if %s then %s else %s" (string_of_expr i)
          (string_of_expr t) (string_of_expr e)
    | Triple (a, b, c) ->
        Printf.sprintf "(%s, %s, %s)" (string_of_expr a) (string_of_expr b)
          (string_of_expr c)
    | Comptime op -> (
        match op with Random -> "rng" | Rule r -> Printf.sprintf "rule(%c)" r)

  let rec eval (ast : expr) (x : float) (y : float) : expr =
    match ast with
    | Const _ -> ast
    | Bool _ -> ast
    | Coord c -> ( match c with X -> Const x | Y -> Const y)
    | UnOp (op, a) -> (
        match eval a x y with
        | Const n -> (
            match op with
            | Neg -> Const (-.n)
            | Sin -> Const (sin n)
            | Cos -> Const (cos n)
            | Not -> raise (SyntaxError "Unable to apply UnOp(Not) to Const"))
        | Bool b -> (
            match op with
            | Not -> Bool (not b)
            | _ ->
                raise (SyntaxError "UnOp other than Not applied to Condition"))
        | _ ->
            raise
              (SyntaxError
                 "UnOp applied to something that is neither Const nor Condition")
        )
    | BinOp (op, a, b) -> (
        match (eval a x y, eval b x y) with
        | Const n1, Const n2 -> (
            match op with
            | Add -> Const (n1 +. n2)
            | Sub -> Const (n1 -. n2)
            | Mul -> Const (n1 *. n2)
            | Div -> Const (n1 /. n2)
            | Mod -> Const (mod_float n1 n2))
        | _, _ -> raise (SyntaxError "BinOp applied to non Const arguments"))
    | Comp (op, a, b) -> (
        match (eval a x y, eval b x y) with
        | Const a, Const b -> (
            match op with
            | Gt -> Bool (a > b)
            | Gte -> Bool (a >= b)
            | Lt -> Bool (a < b)
            | Lte -> Bool (a <= b)
            | Eq -> Bool (a = b))
        | _, _ -> raise (SyntaxError "Comp applied to non Const arguments"))
    | Ite (i, t, e) -> (
        match (eval i x y, eval t x y, eval e x y) with
        | Bool c, t, e -> if c then t else e
        | _, _, _ ->
            raise (SyntaxError "ITE condition does not evaluate to Condition"))
    | Triple (a, b, c) -> Triple (eval a x y, eval b x y, eval c x y)
    | Comptime _ ->
        raise (SyntaxError "Encountered comptime element during evaluation")

  let rec optimise (ast : expr) : expr =
    match ast with
    | BinOp (op, a, b) -> (
        let a = optimise a in
        let b = optimise b in
        match (op, a, b) with
        | Add, Const a, Const b -> Const (a +. b)
        | Sub, Const a, Const b -> Const (a -. b)
        | Mul, Const a, Const b -> Const (a *. b)
        | Div, Const 0., _ -> Const 0.
        | Div, a, Const 1. -> optimise a
        | Div, Const a, Const b -> Const (a /. b)
        | Mod, Const a, Const b -> Const (mod_float a b)
        | _ -> BinOp (op, a, b))
    | UnOp (op, a) -> (
        let a = optimise a in
        match (op, a) with
        | Neg, Const a -> Const (-.a)
        | Sin, Const a -> Const (sin a)
        | Cos, Const a -> Const (cos a)
        | _ -> UnOp (op, a))
    | Comp (op, a, b) -> (
        let a = optimise a in
        let b = optimise b in
        match (op, a, b) with
        | Gt, Const a, Const b -> Bool (a > b)
        | Gte, Const a, Const b -> Bool (a >= b)
        | Lt, Const a, Const b -> Bool (a < b)
        | Lte, Const a, Const b -> Bool (a <= b)
        | Eq, Const a, Const b -> Bool (a = b)
        | _ -> Comp (op, a, b))
    | Ite (e1, e2, e3) -> Ite (optimise e1, optimise e2, optimise e3)
    | Triple (a, b, c) -> Triple (optimise a, optimise b, optimise c)
    | Comptime c -> Comptime c
    | Const _ | Coord _ | Bool _ -> ast

  let partial_substitute_y (ast : expr) (coord : float) =
    let rec substitute = function
      | Const c -> Const c
      | Bool b -> Bool b
      | Coord Y -> Const coord
      | Coord X -> Coord X
      | UnOp (op, a) -> UnOp (op, substitute a)
      | BinOp (op, a, b) -> BinOp (op, substitute a, substitute b)
      | Comp (op, a, b) -> Comp (op, substitute a, substitute b)
      | Ite (i, t, e) -> Ite (substitute i, substitute t, substitute e)
      | Triple (a, b, c) -> Triple (substitute a, substitute b, substitute c)
      | Comptime _ ->
          raise (SyntaxError "Encountered Comptime during evaluation")
    in
    substitute ast |> optimise

  let rec token_len (ast : expr) : int =
    match ast with
    | Const _ | Coord _ | Bool _ | Comptime _ -> 1
    | UnOp (_, a) -> 1 + token_len a
    | BinOp (_, a, b) | Comp (_, a, b) -> 1 + token_len a + token_len b
    | Ite (a, b, c) | Triple (a, b, c) ->
        1 + token_len a + token_len b + token_len c
end

let ( +! ) = Ast.add
let ( -! ) = Ast.sub
let ( *! ) = Ast.mul
let ( /! ) = Ast.div
let ( %! ) = Ast.emod
