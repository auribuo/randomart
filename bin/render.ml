module Render = struct
  open Image
  open ImageUtil_unix
  open Randomart.Ast
  open Domainslib.Task

  let out_to_color v =
    let clamp_float v lo hi = if v < lo then lo else if v > hi then hi else v in
    int_of_float (clamp_float ((v +. 1.) /. 2. *. 255.) 0. 255.)

  let create_image_data width height expr =
    let width_f = float_of_int width in
    let height_f = float_of_int height in

    let rchan = Pixmap.create8 width height in
    let gchan = Pixmap.create8 width height in
    let bchan = Pixmap.create8 width height in
    let pool =
      setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()
    in
    run pool (fun () ->
        parallel_for pool ~start:0 ~finish:(height - 1) ~body:(fun y ->
            let ny = (float_of_int y /. height_f *. 2.) -. 1. in
            let sub = Ast.partial_substitute_y expr ny in
            for x = 0 to width - 1 do
              let nx = (float_of_int x /. width_f *. 2.) -. 1. in
              let evald = Ast.eval sub nx ny in
              let r, g, b =
                match evald with
                | Triple (Const r, Const g, Const b) -> (r, g, b)
                | Const c -> (c, c, c)
                | Bool b -> if b then (1., 1., 1.) else (-1., -1., -1.)
                | _ ->
                    raise
                      (Ast.SyntaxError
                         "Expr does not evaluate to Triple(Const, Const, \
                          Const), Const or Bool")
              in
              let r, g, b = (out_to_color r, out_to_color g, out_to_color b) in
              Pixmap.set rchan x y r;
              Pixmap.set gchan x y g;
              Pixmap.set bchan x y b
            done));
    teardown_pool pool;
    { width; height; max_val = 255; pixels = RGB (rchan, gchan, bchan) }

  let render_image path width height expr =
    let img_data = create_image_data width height expr in
    let oc = open_out_bin path in
    let wr = chunk_writer_of_out_channel oc in
    Printf.printf "Writing image %s\n" path;
    ImagePNG.write wr img_data;
    close_out oc;
    print_endline "Done!"
end
