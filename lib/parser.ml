module rec ParserCommon : sig
  type chrs = char list
  type pos = { line : int; col : int }
  type input = { chars : chrs; pos : pos }
  type 'a parser = input -> ('a * input, string) result
  type char_pred = char -> bool

  val string_of_chrs : chrs -> string
  val chrs_of_string : string -> chrs
  val initial_input : string -> input
  val input_from : chrs -> input -> input
  val next_char : input -> (char * input) option
  val pure : 'a -> 'a parser
  val fail_at : string -> pos -> ('a, string) result
  val fail : string -> 'a parser
  val expect : char_pred -> string -> char parser
  val many : 'a parser -> 'a list parser
  val more : 'a parser -> 'a list parser
  val as_long : char_pred -> string -> chrs parser
  val as_long_not : char_pred -> char -> string -> chrs parser
  val is_whitespace : char_pred
  val is_lower : char_pred
  val is_upper : char_pred
  val is_not : char -> char_pred
  val is_any : char_pred
  val parse_char : char -> char parser
  val parse_any_char : char parser
  val parse_ws : chrs parser
  val parse_literal : string -> string parser
  val token : 'a parser -> 'a parser
  val ( &! ) : char_pred -> char_pred -> char_pred
  val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
  val ( <|> ) : 'a parser -> 'a parser -> 'a parser
  val ( <*> ) : ('b -> 'a) parser -> 'b parser -> 'a parser
  val ( <* ) : 'a parser -> 'b parser -> 'a parser
  val ( *> ) : 'a parser -> 'b parser -> 'b parser
  val ( let$ ) : 'a parser -> ('a -> 'b parser) -> 'b parser
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
end = struct
  open ParserCommon

  type chrs = char list
  type pos = { line : int; col : int }
  type input = { chars : chrs; pos : pos }
  type 'a parser = input -> ('a * input, string) result
  type char_pred = char -> bool

  let string_of_chrs chars = String.init (List.length chars) (List.nth chars)
  let chrs_of_string str = str |> String.to_seq |> List.of_seq

  let initial_input s =
    { chars = chrs_of_string (String.trim s); pos = { line = 1; col = 1 } }

  let input_from chrs input = { chars = chrs; pos = input.pos }

  let advance_pos pos = function
    | '\n' -> { line = pos.line + 1; col = 1 }
    | _ -> { pos with col = pos.col + 1 }

  let next_char input =
    match input.chars with
    | [] -> None
    | c :: cs -> Some (c, { chars = cs; pos = advance_pos input.pos c })

  let pure (x : 'a) : 'a parser = fun input -> Ok (x, input)

  let pen (p1 : ('b -> 'a) parser) (p2 : 'b parser) : 'a parser =
   fun input ->
    let* f, input = p1 input in
    let* a, input = p2 input in
    Ok (f a, input)

  let rpen (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
   fun input ->
    let* _, input = p1 input in
    let* b, input = p2 input in
    Ok (b, input)

  let lpen (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
   fun input ->
    let* a, input = p1 input in
    let* _, input = p2 input in
    Ok (a, input)

  let alternative (p1 : 'a parser) (p2 : 'a parser) =
   fun input ->
    match p1 input with
    | Ok _ as r1 -> r1
    | Error _ -> (
        match p2 input with Ok _ as r2 -> r2 | Error _ as err -> err)

  let fmap (f : 'a -> 'b) (p : 'a parser) : 'b parser =
   fun input ->
    match p input with Ok (x, input') -> Ok (f x, input') | Error _ as e -> e

  let pmap (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
   fun input ->
    match p input with Ok (x, input') -> f x input' | Error _ as e -> e

  let comp (f : char -> bool) (g : char -> bool) = fun b -> f b && g b
  let ( &! ) = comp
  let ( <$> ) = fmap
  let ( <|> ) = alternative
  let ( <*> ) = pen
  let ( <* ) = lpen
  let ( *> ) = rpen
  let ( let* ) = Result.bind
  let ( let$ ) = pmap

  let fail_at (msg : string) (pos : pos) : (_, string) result =
    Error (Printf.sprintf "%d:%d: %s" pos.line pos.col msg)

  let fail (msg : string) (input : input) : (_, string) result =
    fail_at msg input.pos

  let expect pred what =
   fun input ->
    match next_char input with
    | Some (c, input') when pred c -> Ok (c, input')
    | Some (_, _) -> fail (Printf.sprintf "expected %s" what) input
    | None -> fail "unexpected EOF" input

  let rec many (p : 'a parser) : 'a list parser =
   fun input ->
    match p input with
    | Ok (x, rest) -> (
        match many p rest with
        | Ok (xs, rest) -> Ok (x :: xs, rest)
        | Error _ -> Ok ([], input))
    | Error _ -> Ok ([], input)

  let more (p : 'a parser) : 'a list parser =
   fun input ->
    let* x, rest = p input in
    let* xs, rest = many p rest in
    Ok (x :: xs, rest)

  let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_not c = ( <> ) c
  let is_any _ = true

  let as_long (f : char -> bool) (msg : string) : char list parser =
    expect f msg |> many

  let as_long_not f c msg = as_long (is_not c &! f) msg
  let parse_char c = expect (( = ) c) (Printf.sprintf "'%c'" c)
  let parse_any_char = expect (fun _ -> true) "any character"
  let parse_ws = many (expect is_whitespace "whitespace")
  let token (p : 'a parser) : 'a parser = parse_ws *> p <* parse_ws

  let parse_literal (s : string) : string parser =
    let chrs = chrs_of_string s in
    fun input ->
      let rec loop chrs input acc =
        match chrs with
        | c :: cs ->
            let* c, result = parse_char c input in
            loop cs result (c :: acc)
        | [] -> Ok (acc, input)
      in
      let* chrs, rest = loop chrs input [] in
      Ok (string_of_chrs (List.rev chrs), rest)
end

module rec ExprParser : sig
  open Ast.Ast
  open ParserCommon

  val parse_coordinate : expr parser
  val parse_const : expr parser
  val parse_rule_ref : expr parser
  val parse_brackets : expr parser
  val parse_expr : expr parser
  val parse_binop : (expr -> expr -> expr) parser
  val parse_unary : expr parser
  val parse_cmp : (expr -> expr -> expr) parser
end = struct
  open ExprParser
  open Ast.Ast
  open ParserCommon

  let parse_digits =
    more (expect (function '0' .. '9' -> true | _ -> false) "numerical value")

  let int_of_chrs chrs = chrs |> string_of_chrs |> int_of_string

  let chainl1 p op =
    let rec rest acc =
      (let$ f = op in
       let$ y = p in
       rest (f acc y))
      <|> pure acc
    in
    let$ x = p in
    rest x

  let parse_int =
    let$ pre = parse_digits in
    pure (int_of_chrs pre, 0)

  let parse_rule_ref =
    let$ r =
      expect (function 'A' .. 'Z' -> true | _ -> false) "uppercase identifier"
    in
    pure (Comptime (Rule r))

  let parse_rng = parse_literal "rng" *> pure (Comptime Random)

  let parse_func =
    let$ name = as_long_not is_lower '(' "lowercase function name" in
    let$ _ = parse_char '(' in
    let$ body = parse_expr in
    let$ _ = parse_char ')' in
    pure (name, body)

  let parse_func_expr =
    let$ name, body = parse_func in
    match string_of_chrs name with
    | "sin" -> pure (UnOp (Sin, body))
    | "cos" -> pure (UnOp (Cos, body))
    | name -> fail ("unknown function " ^ name)

  let parse_float =
    let$ pre = parse_digits in
    let$ _ = parse_char '.' in
    let$ post = parse_digits in
    pure (int_of_chrs pre, int_of_chrs post)

  let parse_triple =
    let$ _ = parse_char '(' in
    let$ a = parse_expr in
    let$ _ = token (parse_char ',') in
    let$ b = parse_expr in
    let$ _ = token (parse_char ',') in
    let$ c = parse_expr in
    let$ _ = parse_char ')' in
    pure (Triple (a, b, c))

  let parse_ite =
    let$ _ = token (parse_literal "if") in
    let$ i = parse_brackets in
    let$ _ = token (parse_char '{') in
    let$ t = parse_expr in
    let$ _ = token (parse_char '}') in
    let$ _ = parse_literal "else" in
    let$ _ = token (parse_char '{') in
    let$ e = parse_expr in
    let$ _ = token (parse_char '}') in
    pure (Ite (i, t, e))

  let parse_atom =
    parse_ite <|> parse_func_expr <|> parse_triple <|> parse_brackets
    <|> parse_const <|> parse_coordinate <|> parse_rule_ref <|> parse_rng
    <|> fail "Syntax error. Unrecognized expression"

  let parse_expr = chainl1 parse_unary (parse_binop <|> parse_cmp)

  let parse_coordinate =
    ( function 'x' -> Coord X | 'y' -> Coord Y | _ -> failwith "?" )
    <$> (parse_char 'x' <|> parse_char 'y')

  let parse_const =
    let$ pre, post = parse_float <|> parse_int in
    (fun f -> Const f)
    <$> pure (float_of_string (Printf.sprintf "%d.%d" pre post))

  let parse_brackets =
    token (parse_char '(') *> parse_expr <* token (parse_char ')')

  let parse_binop =
    let helper c op =
      token (parse_char c) *> pure (fun l r -> BinOp (op, l, r))
    in
    helper '+' Add <|> helper '-' Sub <|> helper '*' Mul <|> helper '/' Div
    <|> helper '%' Mod

  let parse_cmp =
    let helper c op =
      token (parse_literal c) *> pure (fun l r -> Comp (op, l, r))
    in
    helper ">" Gt <|> helper ">=" Gte <|> helper "<" Lt <|> helper "<=" Lte
    <|> helper "==" Eq

  let parse_unary_op =
    let helper c op = parse_char c *> pure (fun e -> UnOp (op, e)) in
    helper '-' Neg <|> helper '!' Not

  let parse_unary = parse_unary_op <*> parse_unary <|> parse_atom
end

module GrammarParser : sig
  open Ast.Ast
  open Grammar.Grammar

  type parsed_branch = int * expr
  type parsed_rule = char * parsed_branch list

  val parse_rules : string -> (grammar, string) result
end = struct
  open ParserCommon
  open Ast.Ast
  open Grammar.Grammar

  type parsed_branch = int * expr
  type parsed_rule = char * parsed_branch list

  let parse_bars =
    let$ bars = more (parse_char '|') in
    let$ _ = parse_ws in
    pure (List.length bars)

  let parse_comment = parse_char '#' *> as_long_not is_any '\n' "?" <|> pure []

  let parse_expr =
    as_long (fun c -> c <> ';' && c <> '|') "lowercase expression" |> token

  let parse_branch =
   fun input ->
    let* _, input = token parse_comment input in
    let* prob, input = parse_bars input in
    let* _, input = token parse_comment input in
    let* expr, input = parse_expr input in
    let* _, input = token parse_comment input in
    let* body, _ = ExprParser.parse_expr (input_from expr input) in
    pure (prob, body) input

  let parse_rule =
    let$ _ = token parse_comment in
    let$ name = token (expect is_upper "uppercase identifier") in
    let$ opt_branches = more parse_branch <* parse_char ';' in
    pure (name, opt_branches)

  let check_parens input =
    let rec check input state =
      match next_char input with
      | Some (c, rest) -> (
          match c with
          | '(' -> check rest ((c, input.pos) :: state)
          | ')' -> (
              match state with
              | ('(', _) :: state' -> check rest state'
              | _ :: _ -> fail_at "Mismatched closing paren" input.pos
              | [] -> fail_at "Closing paren has no opening paren" input.pos)
          | '{' -> check rest ((c, input.pos) :: state)
          | '}' -> (
              match state with
              | ('{', _) :: state' -> check rest state'
              | _ :: _ -> fail_at "Mismatched closing paren" input.pos
              | [] -> fail_at "Closing paren has no opening paren" input.pos)
          | _ -> check rest state)
      | None ->
          let* _ =
            match state with
            | [] -> Ok ()
            | (_, input) :: _ -> fail_at "Unclosed delimiter" input
          in
          Ok ()
    in
    check input []

  let check_rule_dupes rules =
    let rec check rules acc =
      match rules with
      | r :: rs ->
          if List.mem r rs then Error (Printf.sprintf "Duplicate rule: %c" r)
          else check rs (r :: acc)
      | [] -> Ok ()
    in
    check (List.map (fun (a, _) -> a) rules) []

  let parse_rules =
    let rec consume (acc : parsed_rule list) (input : input) =
      match input.chars with
      | [] -> Ok (List.rev acc, input)
      | _ ->
          let* rule, rem = parse_rule input in
          consume (rule :: acc) rem
    in
    fun input ->
      let input = initial_input input in
      let* _ = check_parens input in
      let* ls, _ = consume [] input in
      let* _ = check_rule_dupes ls in
      let g = Hashtbl.create (List.length ls) in
      let rec build_ht = function
        | [] -> ()
        | (name, rules) :: rs ->
            Hashtbl.add g name
              (Array.of_list
                 (List.map (fun (prob, ast) -> { ast; prob }) rules));
            build_ht rs
      in
      build_ht ls;
      Ok g
end
