open Randomart.Ast
open Randomart.Grammar
open Randomart.Rational
open Render

let () =
  let e : Grammar.grammar_rule =
    [|
      {
        ast = Triple (Comptime (Rule 2), Comptime (Rule 2), Comptime (Rule 2));
        prob = Rational.rat_of_int 1;
      };
    |]
  in

  let a : Grammar.grammar_rule =
    [|
      { ast = Comptime Random; prob = (1, 8) };
      { ast = Coord X; prob = (1, 4) };
      { ast = Coord Y; prob = (2, 4) };
      { ast = Coord X +! Coord Y; prob = (1, 8) };
    |]
  in
  let c : Grammar.grammar_rule =
    [|
      { ast = Comptime (Rule 1); prob = (1, 4) };
      { ast = Comptime (Rule 2) +! Comptime (Rule 2); prob = (3, 8) };
      { ast = Comptime (Rule 2) *! Comptime (Rule 2); prob = (3, 8) };
    |]
  in
  let g : Grammar.grammar = [| e; a; c |] in
  for i = 0 to 9 do
    print_endline "Starting grammar compilation";
    let gt0 = Unix.gettimeofday () in
    let expr = Grammar.compile_grammar g 0 40 (Some 69) in
    let gt1 = Unix.gettimeofday () in
    match expr with
    | Ok expr ->
        let t0 = Unix.gettimeofday () in
        let opt = Ast.optimise expr in
        let t1 = Unix.gettimeofday () in
        (* print_endline (Ast.string_of_expr expr); *)
        (* print_endline "//////////////////////////"; *)
        (* print_endline (Ast.string_of_expr opt); *)
        let normal_len = Ast.token_len expr in
        let opt_len = Ast.token_len opt in
        Printf.printf
          "Generated %d tokens in %.3fs reduced to %d (%.2f%%) after cleaning \
           for %.3fs\n"
          normal_len (gt1 -. gt0) opt_len
          (100. -. (float_of_int opt_len /. float_of_int normal_len *. 100.))
          (t1 -. t0);
        let width = 512 in
        let height = width in
        let path = Printf.sprintf "out/image%d.png" i in
        Printf.printf "Starting to generate %s" path;
        print_newline ();
        Render.render_image path width height opt;
        print_endline "-----------------------------------------------"
    | Error err -> Printf.printf "Compilation failed: %s" err
  done
