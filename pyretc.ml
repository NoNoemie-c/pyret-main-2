let parseonly = ref false
let typeonly = ref false


let compile p =  
  let chan = open_in p in
  let lb = Lexing.from_channel chan in
  begin try
    let ast = Parser.file Lexer.token lb in
    if !parseonly then Ast.print_file ast
    else let tast = Typer.w ast in
      if !typeonly then Tast.print_file tast
      else let s = Emiter.code tast in
        X86_64.print_in_file ~file: (Filename.chop_extension p ^ ".s") s
        (* cat *.txt >> all.txt *)
  with 
  | Error.Lexer f -> 
    let sp = Lexing.lexeme_start_p lb in
    let ep = Lexing.lexeme_end_p lb in
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
    exit 1
  | Parser.Error -> (* the default menhir error *)
    Printf.eprintf "File \"%s\", line -1, characters -1--1:\nparsing error\n" p;
    exit 1
  | Error.Parser (sp, ep, f) -> 
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
    exit 1
  | Error.Typer (sp, ep, f) ->
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
      p sp.pos_lnum sp.pos_cnum ep.pos_cnum;
    f ();
    exit 1
  end;
  close_in chan

let () =
  Arg.parse
    [
      "--parse-only", Arg.Set parseonly, "parse the source, without typing and
        compiling it";
      "--type-only", Arg.Set typeonly, "parse and type the source, without
        compiling it";
    ]
    compile
    ""