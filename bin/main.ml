let usage   = "arith [-verbose] [<file>]"
let verbose = ref false
let input   = ref None
let anon_fun filename =
  input := Some filename

let spec_list =
  [("-verbose", Arg.Set verbose, "output debug information")]

let () = 
  Arg.parse spec_list anon_fun usage;
  Imp.Parser.pp_exceptions ();

  let expr = 
    if Option.is_some !input then
      Imp.Parser.parse_file (Option.get !input)
    else
      begin
        Printf.printf "imp 0.0.0 -- ctrl-d to complete the expression\n\n%!";
        Imp.Parser.parse_chan stdin
      end
  in

  if !verbose then
    Printf.printf "tokens:\n%s\n\n" Imp.Core.(show_cexp expr);

  let rec fold = function
  | (st, Imp.Core.Skip) -> st
  | (st, exp)           -> fold (Imp.Core.reduce_cexp st exp)
  in
    let res = fold ([], expr) in
    List.iter (fun (n, v) -> Printf.printf "%s = %d\n" n v) res
