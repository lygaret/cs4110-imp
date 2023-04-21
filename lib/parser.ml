type token  = Grammar.token
type result = Core.cexp

exception LexError of { msg: string; loc: Location.t }
exception ParseError of { token: token; loc: Location.t }

let pp_parser_exceptions = function
| LexError   {msg; loc} -> Some (Location.error ~loc msg)
| ParseError {loc; _  } -> Some (Location.error ~loc "[parser] unexpected token")
| _                     -> None

let pp_exceptions () = begin
  Location.register_error_of_exn pp_parser_exceptions;
  Printexc.register_printer (function exn ->
    try
      ignore (Format.flush_str_formatter ());
      Location.report_exception Format.str_formatter exn;
      Some (Format.flush_str_formatter ());
    with _ ->
      None
  );
end

let reraise exn = raise exn

let curr_token : token option ref = ref None

let next_token lexbuf =
  let token = Lexer.token lexbuf in
  curr_token := Some token;
  token

let parse ?(file="") lexbuf =
  Location.input_name := file;
  Location.input_lexbuf := Some lexbuf;

  try
    Grammar.main next_token lexbuf
  with
  | Lexer.LexError msg -> 
    raise (LexError { msg; loc = Location.curr lexbuf })
  | Grammar.Error -> 
    let[@warning "-8"] (Some token) = !curr_token in
    raise (ParseError { token; loc = Location.curr lexbuf })

let parse_string ?(pos: Lexing.position option) s =
  match pos with
  | Some ({ pos_fname = file; _ } as p) -> 
    parse ~file Lexing.{(from_string s) with lex_start_p=p; lex_curr_p=p}
  | None -> 
    parse (Lexing.from_string s)

  let parse_chan ?(pos : Lexing.position option) chan =
    match pos with
    | None ->
      parse (Lexing.from_channel chan)
    | Some ({pos_fname=file; _} as p) ->
      parse ~file Lexing.{(from_channel chan) with lex_start_p=p; lex_curr_p=p}

  let parse_file file =
    Stdio.In_channel.with_file file ~f:(fun chan ->
      let lexbuf = Lexing.from_channel chan in
      Location.init lexbuf file;
      parse ~file lexbuf
    )