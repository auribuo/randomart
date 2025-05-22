open Randomart.Render
open Randomart.Parser
open Randomart.Grammar

let usage =
  "randomart -width <width> [-height <height>] [-seed <seed>] -max-depth \
   <depth> -out <output> GRAMMAR"

let width = ref 0
let height = ref 0
let max_depth = ref 0
let out = ref ""
let seed = ref 0
let grammar = ref ""
let dump = ref false

let speclist =
  [
    ("-width", Arg.Set_int width, "Set image width");
    ("-height", Arg.Set_int height, "Set image height. Optional");
    ("-max-depth", Arg.Set_int max_depth, "Set max recursion depth");
    ("-seed", Arg.Set_int seed, "Set the generation seed");
    ("-out", Arg.Set_string out, "Set output name");
    ("-dump", Arg.Set dump, "Dump compiled grammar");
  ]

let ( let* ) = Result.bind

let run =
  Arg.parse speclist (fun s -> grammar := s) usage;
  if !width = 0 || !max_depth = 0 || !out = "" || !grammar = "" then
    Error
      "Options width, max_depth, out and grammar must be set and nonzero. \
       Provide -help for help\n"
  else
    let width = !width in
    let height = if !height = 0 then width else !height in
    let seed = if !seed = 0 then None else Some !seed in
    let chan = open_in !grammar in
    let len = in_channel_length chan in
    let content = really_input_string chan len in
    close_in chan;
    let* g = GrammarParser.parse_rules content in
    let _ = if !dump then print_string (Grammar.string_of_grammar g) else () in
    match Render.render_frame g 'E' width height !max_depth seed with
    | Ok frame ->
        let image = Render.generate_image frame width height in
        Render.write_image ImagePNG.write !out image;
        Ok ()
    | Error err -> Error (Printf.sprintf "Failed to generate frame: %s" err)

let () = match run with Ok _ -> () | Error err -> prerr_endline err
