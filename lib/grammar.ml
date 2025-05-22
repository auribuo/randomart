module Grammar = struct
  open Ast

  let ( let* ) = Result.bind

  type grammar_branch = { ast : Ast.expr; prob : int }
  type grammar_rule = grammar_branch array
  type grammar = (char, grammar_rule) Hashtbl.t

  let find_rule (g : grammar) (rid : char) : (grammar_rule, string) result =
    Option.to_result (Hashtbl.find_opt g rid)
      ~none:(Printf.sprintf "Rule %c not found in grammar" rid)

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

  let prob_den (rule : grammar_rule) : float =
    Array.fold_left
      (fun acc e -> acc +. e)
      0.0
      (Array.map (fun e -> float_of_int e.prob) rule)

  let pick_weighted (rule : grammar_rule) : grammar_branch =
    let rand = Random.float 1. in
    let den = prob_den rule in
    let rec pick acc i =
      let item = rule.(i) in
      if i = Array.length rule - 1 then item
      else
        let acc = acc +. (float_of_int item.prob /. den) in
        if rand <= acc then item else pick acc (i + 1)
    in
    pick 0.0 0

  let rec force_terminate (g : grammar) (rid : char) : (Ast.expr, string) result
      =
    match Hashtbl.find_opt g rid with
    | Some rule ->
        if all_terminal rule then Ok (pick_weighted rule).ast
        else
          let rec find_terminal_branch i : (Ast.expr, string) result =
            if i >= Array.length rule then
              Error
                (Printf.sprintf "Cannot find terminal path from rule %c" rid)
            else
              let branch = rule.(i) in
              if branch_terminal branch then Ok branch.ast
              else
                match force_resolve_expr branch.ast with
                | Ok ast -> Ok ast
                | Error err -> Error err
          and force_resolve_expr (e : Ast.expr) : (Ast.expr, string) result =
            match e with
            | Comptime Random -> Ok (Const (Random.float 2.0 -. 1.0))
            | Comptime (Rule rid) -> force_terminate g rid
            | UnOp (op, a) ->
                let* a = force_resolve_expr a in
                Ok (Ast.UnOp (op, a))
            | BinOp (op, a, b) ->
                let* a = force_resolve_expr a in
                let* b = force_resolve_expr b in
                Ok (Ast.BinOp (op, a, b))
            | Comp (op, a, b) ->
                let* a = force_resolve_expr a in
                let* b = force_resolve_expr b in
                Ok (Ast.Comp (op, a, b))
            | Ite (i, t, e) ->
                let* i = force_resolve_expr i in
                let* t = force_resolve_expr t in
                let* e = force_resolve_expr e in
                Ok (Ast.Ite (i, t, e))
            | Triple (a, b, c) ->
                let* a = force_resolve_expr a in
                let* b = force_resolve_expr b in
                let* c = force_resolve_expr c in
                Ok (Ast.Triple (a, b, c))
            | _ -> Ok e
          in
          find_terminal_branch 0
    | None -> Error (Printf.sprintf "Rule %c not found in grammar" rid)

  let compile_grammar (g : grammar) start max_depth seed =
    let _ =
      match seed with Some s -> Random.init s | None -> Random.self_init ()
    in
    let rec gen_node node depth =
      match node with
      | Ast.Coord _ | Const _ | Bool _ -> Ok node
      | UnOp (op, opr) ->
          let* node = gen_node opr depth in
          Ok (Ast.UnOp (op, node))
      | BinOp (op, lhs, rhs) ->
          let* lhs = gen_node lhs depth in
          let* rhs = gen_node rhs depth in
          Ok (Ast.BinOp (op, lhs, rhs))
      | Comp (op, lhs, rhs) ->
          let* lhs = gen_node lhs depth in
          let* rhs = gen_node rhs depth in
          Ok (Ast.Comp (op, lhs, rhs))
      | Ite (i, t, e) ->
          let* i = gen_node i depth in
          let* t = gen_node t depth in
          let* e = gen_node e depth in
          Ok (Ast.Ite (i, t, e))
      | Triple (a, b, c) ->
          let* a = gen_node a depth in
          let* b = gen_node b depth in
          let* c = gen_node c depth in
          Ok (Ast.Triple (a, b, c))
      | Comptime operation -> (
          match operation with
          | Random -> Ok (Const (Random.float 2.0 -. 1.0))
          | Rule r ->
              let* rule = gen_rule r (depth - 1) in
              Ok rule)
    and gen_rule rid depth : (Ast.expr, string) result =
      if depth <= 0 then
        let* final = force_terminate g rid in
        Ok final
      else
        let* rule = find_rule g rid in
        let branch = (pick_weighted rule).ast in
        gen_node branch (depth - 1)
    in
    let* output = gen_rule start max_depth in
    let temp_eval = Ast.eval output 0.0 0.0 in
    match temp_eval with
    | Triple (Const _, Const _, Const _) | Const _ | Bool _ -> Ok output
    | _ ->
        Error
          "Expr does not evaluate to Triple(Const, Const, Const), Const or Bool"

  let string_of_grammar (g : grammar) =
    Hashtbl.fold
      (fun k v acc ->
        acc
        ^ Printf.sprintf "Rule %c with %d branches:\n%s\n" k (Array.length v)
            (Array.fold_left
               (fun acc { prob; ast } -> acc ^ (Printf.sprintf "\t %d -> %s\n" prob (Ast.string_of_expr ast)))
               "" v))
      g ""
end
