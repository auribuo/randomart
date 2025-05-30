module Render = struct
  open Ast
  open Domainslib.Task
  open Image
  open ImageUtil_unix
  open Grammar

  let ( let* ) = Result.bind

  let out_to_color v =
    let clamp_float v lo hi = if v < lo then lo else if v > hi then hi else v in
    int_of_float (clamp_float ((v +. 1.) /. 2. *. 255.) 0. 255.)

  let show_progress total current =
    let bar_width = 50 in
    let percent = float_of_int current /. float_of_int total in
    let filled = int_of_float (percent *. float_of_int bar_width) in
    let bar = String.make filled '#' ^ String.make (bar_width - filled) '-' in
    Printf.printf "\rRendering: [%s] %3d%%" bar (int_of_float (percent *. 100.));
    flush stdout

  let render_frame g start_rule width height max_depth seed dumpe =
    let* expr = Grammar.compile_grammar g start_rule max_depth seed in
    let expr = expr |> Ast.optimise in
    Printf.printf "Compiled grammar to %d tokens\n" (Ast.token_len expr);
    let _ = if dumpe then print_endline (Ast.string_of_expr expr) else () in
    let width_f = float_of_int width in
    let height_f = float_of_int height in
    let total = width * height in
    let pixels = Array.make total (0, 0, 0) in
    let counter = Atomic.make 0 in
    let pool =
      setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()
    in
    run pool (fun () ->
        parallel_for pool ~start:0 ~finish:(height - 1) ~body:(fun y ->
            let ny = (float_of_int y /. height_f *. 2.) -. 1. in
            let sub = Ast.partial_substitute_y expr ny in
            parallel_for pool ~start:0 ~finish:(width - 1) ~body:(fun x ->
                let nx = (float_of_int x /. width_f *. 2.) -. 1. in
                let evald = Ast.eval sub nx ny in
                let r, g, b =
                  match evald with
                  | Triple (Const r, Const g, Const b) -> (r, g, b)
                  | Const c -> (c, c, c)
                  | Bool b -> if b then (1., 1., 1.) else (-1., -1., -1.)
                  | _ -> failwith "UNREACHABLE"
                in
                let r, g, b =
                  (out_to_color r, out_to_color g, out_to_color b)
                in
                pixels.((y * width) + x) <- (r, g, b);
                Atomic.incr counter;
                if Atomic.get counter mod 10000 = 0 || Atomic.get counter = total
                then show_progress total (Atomic.get counter))));
    teardown_pool pool;
    Ok pixels

  let generate_image pixels width height =
    let rchan = Pixmap.create8 width height in
    let gchan = Pixmap.create8 width height in
    let bchan = Pixmap.create8 width height in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let r, g, b = pixels.((y * width) + x) in
        Pixmap.set rchan x y r;
        Pixmap.set gchan x y g;
        Pixmap.set bchan x y b
      done
    done;
    { width; height; max_val = 255; pixels = RGB (rchan, gchan, bchan) }

  let write_image f path image =
    let oc = open_out_bin path in
    let wr = chunk_writer_of_out_channel oc in
    f wr image;
    flush oc;
    close_out oc
end
