module Grammar = struct
  open Ast
  open Rational

  type grammar_branch = { ast : Ast.expr; prob : Rational.rat }
  type grammar_rule = grammar_branch array
  type grammar = grammar_rule array

  exception CompileError of string

  let branch_terminal (b : grammar_branch) : bool =
    match b.ast with
    | Ast.UnOp (_, Comptime _)
    | BinOp (_, Comptime _, _)
    | BinOp (_, _, Comptime _)
    | Comp (_, Comptime _, _)
    | Comp (_, _, Comptime _)
    | Ite (Comptime _, _, _)
    | Ite (_, Comptime _, _)
    | Ite (_, _, Comptime _)
    | Triple (Comptime _, _, _)
    | Triple (_, Comptime _, _)
    | Triple (_, _, Comptime _)
    | Comptime _ ->
        false
    | _ -> true

  let all_terminal (rule : grammar_rule) : bool =
    Array.for_all branch_terminal rule

  let first_terminal (rule : grammar_rule) : grammar_branch option =
    Array.find_opt branch_terminal rule

  let pick_weighted (rule : grammar_rule) : grammar_branch =
    let rand = Random.float 1. in
    let rec pick acc i =
      let item = rule.(i) in
      if i = Array.length rule - 1 then item
      else
        let acc = acc +. Rational.float_of_rat item.prob in
        if rand <= acc then item else pick acc (i + 1)
    in
    pick 0.0 0

  let rec force_terminate (g : grammar) (rid : int) : Ast.expr =
    let rule = g.(rid) in
    if all_terminal rule then (pick_weighted rule).ast
    else
      let rec find_terminal_branch i =
        if i >= Array.length rule then
          raise
            (CompileError
               ("Cannot find terminal path from rule " ^ string_of_int rid))
        else
          let branch = rule.(i) in
          if branch_terminal branch then branch.ast
          else force_resolve_expr branch.ast
      and force_resolve_expr (e : Ast.expr) : Ast.expr =
        match e with
        | Comptime Random -> Const (Random.float 2.0 -. 1.0)
        | Comptime (Rule rid) -> force_terminate g rid
        | UnOp (op, a) -> UnOp (op, force_resolve_expr a)
        | BinOp (op, a, b) ->
            BinOp (op, force_resolve_expr a, force_resolve_expr b)
        | Comp (op, a, b) ->
            Comp (op, force_resolve_expr a, force_resolve_expr b)
        | Ite (i, t, e) ->
            Ite
              (force_resolve_expr i, force_resolve_expr t, force_resolve_expr e)
        | Triple (a, b, c) ->
            Triple
              (force_resolve_expr a, force_resolve_expr b, force_resolve_expr c)
        | _ -> e
      in
      find_terminal_branch 0

  let check_rules (g : grammar) =
    let failed = Array.make (Array.length g) false in
    for i = 0 to Array.length g - 1 do
      let probs = Array.map (fun a -> a.prob) g.(i) in
      if Array.fold_left Rational.add (0, 1) probs <> (1, 1) then
        failed.(i) <- true
      else ()
    done;
    failed

  let compile_grammar (g : grammar) start max_depth seed =
    match Array.find_index (fun e -> e) (check_rules g) with
    | Some fail ->
        Error
          (Printf.sprintf "Rule %d does not have a total of 1 probability" fail)
    | None ->
        let _ =
          match seed with
          | Some s -> Random.init s
          | None -> Random.self_init ()
        in
        let rec gen_node node depth =
          match node with
          | Ast.Coord _ | Const _ | Bool _ -> node
          | UnOp (op, opr) -> UnOp (op, gen_node opr depth)
          | BinOp (op, lhs, rhs) ->
              BinOp (op, gen_node lhs depth, gen_node rhs depth)
          | Comp (op, lhs, rhs) ->
              Comp (op, gen_node lhs depth, gen_node rhs depth)
          | Ite (iff, thenn, elze) ->
              Ite (gen_node iff depth, gen_node thenn depth, gen_node elze depth)
          | Triple (a, b, c) ->
              Triple (gen_node a depth, gen_node b depth, gen_node c depth)
          | Comptime operation -> (
              match operation with
              | Random -> Const (Random.float 2.0 -. 1.0)
              | Rule r -> gen_rule r (depth - 1))
        and gen_rule rid depth =
          if depth <= 0 then force_terminate g rid
          else
            let branch = (pick_weighted g.(rid)).ast in
            gen_node branch (depth - 1)
        in
        Ok (gen_rule start max_depth)
end
