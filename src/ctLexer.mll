{
  (*
    
    CamlTemplate: A template processor for Objective Caml programs.
    Copyright © 2003, 2004, 2005 Benjamin Geer
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA
    
    In addition, as a special exception, Benjamin Geer gives
    permission to link the code of this program with the Apache HTTP
    Server (or with modified versions of Apache that use the same
    license as Apache), and distribute linked combinations including
    the two. You must obey the GNU General Public License in all
    respects for all of the code used other than Apache. If you modify
    this file, you may extend this exception to your version of the
    file, but you are not obligated to do so. If you do not wish to do
    so, delete this exception statement from your version.
    
  *)

  (* $Id: ctLexer.mll,v 1.22 2005-05-29 09:31:33 ben Exp $ *)

  open CtParser ;;
  open CtSourcePos ;;
  open Printf ;;
  open CtExceptions ;;
  
  (* An ocamllex lexer for template source code. *)
  
  (* Our current position in the input. *)

  let cur_pos = new source_pos ;;

  (* Buffering for string literals. *)

  let string_buf = Buffer.create 256

  (* Buffering for template text. *)

  let text_buf = Buffer.create 256

  (* The position where the current string literal started. *)

  let string_start_pos = new source_pos ;;

  (* The current depth of nested comments. *)

  let comment_nesting_depth = ref 0 ;;
  
  (* The start position of the outermost comment. *)
  
  let comment_start_pos = new source_pos ;;

  (* The lexer works in different 'modes', depending on whether it's
     in template text, a statement, an expansion, etc. *)
  
  type lexer_mode =
      Text_mode
    | Expansion_mode
    | Dollar_mode
    | Statement_mode
    | Eof_mode ;;

  let cur_mode = ref Text_mode ;;

  (* The position where we entered the current mode. *)

  let mode_start_pos = new source_pos ;;

  (* We have to count parentheses to know when to leave statement mode. *)

  let paren_count = ref 0 ;;

  (* Functions for changing modes. *)

  let to_text_mode () = 
    (* eprintf "to_text_mode "; *)
    cur_mode := Text_mode;
    mode_start_pos#set_from cur_pos;
    Buffer.reset text_buf ;;

  let to_dollar_mode () =
    (* eprintf "to_dollar_mode "; *)
    cur_mode := Dollar_mode;
    mode_start_pos#set_from cur_pos ;;

  let to_expansion_mode () =
    (* eprintf "to_expansion_mode "; *)
    cur_mode := Expansion_mode;
    mode_start_pos#set_from cur_pos;
    paren_count := 0 ;;

  let to_statement_mode () =
    (* eprintf "to_statement_mode "; *)
    cur_mode := Statement_mode;
    mode_start_pos#set_from cur_pos;
    paren_count := 0 ;;

  let to_eof_mode () =
    (* eprintf "to_eof_mode "; *)
    cur_mode := Eof_mode;
    mode_start_pos#set_from cur_pos ;;

  (* Called by the parser wrapper before parsing each template. *)

  let reset_lexer () =
    cur_pos#reset;
    comment_nesting_depth := 0;
    Buffer.reset string_buf;
    to_text_mode () ;;

  (* Called by the parser wrapper after parsing each template; may
     reduce memory usage. *)

  let clean_up_lexer () =
    Buffer.reset string_buf ;;

  (* Keywords. *)

  type keyword_types =
      TrueKwd
    | FalseKwd
    | NullKwd
    | IfKwd
    | ElseKwd
    | ElseIfKwd
    | ForEachKwd
    | InKwd
    | SetKwd
    | VarKwd
    | EndKwd
    | IncludeKwd
    | MacroKwd
    | OpenKwd ;;

  let token_for_keyword kwd pos =
    match kwd with
        TrueKwd -> TRUE pos
      | FalseKwd -> FALSE pos
      | NullKwd -> NULL pos
      | IfKwd -> IF pos
      | ElseKwd -> ELSE pos
      | ElseIfKwd -> ELSEIF pos
      | ForEachKwd -> FOREACH pos
      | InKwd -> IN pos
      | SetKwd -> SET pos
      | VarKwd -> VAR pos
      | EndKwd -> END pos
      | IncludeKwd -> INCLUDE pos
      | MacroKwd -> MACRO pos
      | OpenKwd -> OPEN pos ;;
  
  let keyword_table = Hashtbl.create 10 ;;

  let _ =
    List.iter (function (kwd_name, kwd) -> Hashtbl.add keyword_table kwd_name kwd)
      [ "true", TrueKwd;
        "false", FalseKwd;
        "null", NullKwd;
        "if", IfKwd;
        "else", ElseKwd;
        "else#", ElseKwd;
        "elseif", ElseIfKwd;
        "foreach", ForEachKwd;
        "in", InKwd;
        "set", SetKwd;
        "var", VarKwd;
        "end", EndKwd;
        "end#", EndKwd;
        "include", IncludeKwd;
        "macro", MacroKwd;
        "open", OpenKwd ] ;;
}

let blank = [ ' ' '\t' ]
let ident_first_char = [ 'A'-'Z' 'a'-'z' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ]
let int_literal = [ '0'-'9' ] [ '0'-'9' '_' ]*
let float_literal = [ '0'-'9' ] [ '0'-'9' '_' ]* ('.' [ '0'-'9' '_' ]* )?
  ([ 'e' 'E' ] [ '+' '-' ]? [ '0'-'9' ]+)?

rule main = parse
    | '\\'
        { (* eprintf "backslash[%d] " cur_pos#get_char_no; *)
          match !cur_mode with
              Text_mode ->
                cur_pos#incr_char_no;
                maybe_escape lexbuf
                  
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                    cur_pos)) }

    | '$' ([^ '{' '$' ] | eof)
        { (* eprintf "dollar(non-expansion)[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
          let str = Lexing.lexeme lexbuf in
          let len = String.length str in
            if len = 2 && str.[1] = '\n' then
              cur_pos#incr_line_no
            else
              cur_pos#advance_char_no len;
            match !cur_mode with
                Text_mode ->
                  if str.[len - 1] = '\\' then
                    (
                      Buffer.add_char text_buf '$';
                      maybe_escape lexbuf
                    )
                  else
                    (
                      Buffer.add_string text_buf str;
                      main lexbuf
                    )

              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '$' +
        { (* eprintf "dollar_start[%d] " cur_pos#get_char_no;  *)
          let pos = cur_pos#clone in
          let str = Lexing.lexeme lexbuf in
          let len = String.length str in
          match !cur_mode with
              Text_mode ->
                let text_start_pos = mode_start_pos#clone in
                  Buffer.add_string text_buf (String.sub str 0 (len - 1));
                  cur_pos#advance_char_no (len - 1);
                  to_dollar_mode ();
                  cur_pos#incr_char_no;
                  let dollar_start_pos = mode_start_pos#clone in
                    if (Buffer.length text_buf) > 0 then
                      TEXT (text_start_pos, dollar_start_pos, Buffer.contents text_buf)
                    else
                      main lexbuf

            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   cur_pos)) }
        
    | '{'
        { (* eprintf "expansion_start[%d] " cur_pos#get_char_no; *)
          match !cur_mode with
              Text_mode ->
                Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                cur_pos#incr_char_no;
                main lexbuf
                  
            | Dollar_mode ->
                (* Use the dollar's position as the start position of the EXPANSION. *)
                let dollar_start_pos = mode_start_pos#clone in
                  to_expansion_mode ();
                  mode_start_pos#set_from dollar_start_pos;
                  cur_pos#incr_char_no;
                  EXPANSION (dollar_start_pos)
                    
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   cur_pos)) }
        
    | '}'
        { (* eprintf "expansion_end[%d] " cur_pos#get_char_no; *)
          match !cur_mode with
              Text_mode ->
                Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                cur_pos#incr_char_no; 
                main lexbuf

            | Expansion_mode ->
                cur_pos#incr_char_no;
                to_text_mode ();
                main lexbuf
                  
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   cur_pos)) }

    | blank* "#*"
        { (* eprintf "comment_in_text[%d] " cur_pos#get_char_no; *)
          let len = String.length (Lexing.lexeme lexbuf) in
            cur_pos#advance_char_no (len - 2);
            match !cur_mode with
                Text_mode ->
                  comment_nesting_depth := 1;
                  comment_start_pos#set_from cur_pos#clone;
                  cur_pos#advance_char_no 2;
                  comment lexbuf;
                  main lexbuf
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                      cur_pos)) }
        
    | blank* '#'
        { (* eprintf "instr_start[%d] " cur_pos#get_char_no; *)
          let str = Lexing.lexeme lexbuf in
          let len = String.length str in
          let str_start_pos = cur_pos#clone in
            cur_pos#advance_char_no (len - 1);
            match !cur_mode with
                Text_mode ->
                  (* Eat preceding spaces only if they start at the beginning of the line. *)
                  let text_end_pos =
                    if str_start_pos#get_char_no = 0 && len > 1 then
                      (
                        str_start_pos
                      )
                    else
                      (
                        Buffer.add_string text_buf (String.sub str 0 (len - 1));
                        cur_pos#clone
                      )
                  in let text_start_pos = mode_start_pos#clone in
                    to_statement_mode ();
                    cur_pos#incr_char_no;
                    if (Buffer.length text_buf) > 0 then
                      TEXT (text_start_pos, text_end_pos, Buffer.contents text_buf)
                    else if str_start_pos#is_origin then
                      (* Make sure the parser's input begins with a token, so we
                         can always handle a syntax error when '#' is the first character
                         in the file. *)
                      START (str_start_pos)
                    else
                      main lexbuf
              | Expansion_mode ->
                  raise (Lexer_error (Unterminated_expansion, mode_start_pos))
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                      cur_pos)) }
        
    | '('
        { (* eprintf "lparen[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  paren_count := !paren_count + 1;
                  LPAREN pos
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | ')' '\n'?
        { (* eprintf "rparen[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
          let str = Lexing.lexeme lexbuf in
          let len = String.length str in
            if str.[len - 1] = '\n' then
              (
                (* eprintf "to_next_line "; *)
                cur_pos#incr_line_no
              )
            else
              (
                (* eprintf "to_next_char "; *)
                cur_pos#advance_char_no len
              );
            match !cur_mode with              
                Text_mode ->
                  (* eprintf "already_in_text "; *)
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                paren_count := !paren_count - 1;
                  (* eprintf "paren_count(%d) " !paren_count; *)
                  if !paren_count < 0 then
                    raise (Lexer_error (Mismatched_parens, pos))
                  else if !cur_mode = Statement_mode && !paren_count = 0 then
                    to_text_mode ();
                  RPAREN pos
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | '\n'
        { (* eprintf "newline[line %d] " (cur_pos#get_line_no + 1); *)
          let pos = cur_pos#clone in
            cur_pos#incr_line_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf '\n';
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  main lexbuf
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
                
    | blank
        { (* eprintf "blank[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  main lexbuf
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | "\""
        { (* eprintf "string[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  Buffer.reset string_buf;
                  string_start_pos#set_from pos;
                  string_token lexbuf;
                  STRING (string_start_pos#clone, (Buffer.contents string_buf))
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | int_literal
      { (* eprintf "int_digits[%d] " cur_pos#get_char_no; *)
        let pos = cur_pos#clone and
          str = Lexing.lexeme lexbuf in
          cur_pos#advance_char_no (String.length str);
          match !cur_mode with
              Text_mode ->
                Buffer.add_string text_buf str;
                main lexbuf
                  
            | Expansion_mode | Statement_mode ->
                  INT (pos, (int_of_string str))
                    
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   pos)) }

    | float_literal
      { (* eprintf "float_digits[%d] " cur_pos#get_char_no; *)
        let pos = cur_pos#clone and
          str = Lexing.lexeme lexbuf in
          cur_pos#advance_char_no (String.length str);
          match !cur_mode with
              Text_mode ->
                Buffer.add_string text_buf str;
                main lexbuf
                  
            | Expansion_mode | Statement_mode ->
                  FLOAT (pos, (float_of_string str))
                    
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   pos)) }

    | (ident_first_char ident_char* '#'? as word) '\n'?
      { (* eprintf "word[%d] " cur_pos#get_char_no; *)
        let pos = cur_pos#clone
        and str = Lexing.lexeme lexbuf in
        let str_len = String.length str in
          if str.[str_len - 1] = '\n' then
              cur_pos#incr_line_no
          else
              cur_pos#advance_char_no str_len;
          let word_len = String.length word in
          let word_ends_with_hash = word.[word_len - 1] = '#' in
          let reject_hash_at_end () =
            if word_ends_with_hash then
              let hash_pos = pos#clone in
                hash_pos#advance_char_no (word_len - 1);
                raise (Lexer_error (Illegal_character '#', hash_pos))
          in
            match !cur_mode with
                Text_mode ->
                  (* We might be here because we got something like foo#else, where 'foo#' matched
                     as a word.  If so, add 'foo', switch to statement mode, and return the text. *)
                  if word_ends_with_hash then
                    (
                    Buffer.add_string text_buf (String.sub word 0 (word_len - 1));
                      let text_start_pos = mode_start_pos#clone in
                        to_statement_mode ();
                        let statement_start_pos = mode_start_pos#clone in
                          TEXT (text_start_pos, statement_start_pos, Buffer.contents text_buf)
                    )
                  else
                    (* Otherwise, add the whole string. *)
                    (
                      Buffer.add_string text_buf str;
                      main lexbuf
                    )
                      
              | Expansion_mode ->
                  reject_hash_at_end ();                  
                  (
                    try
                      let kwd = Hashtbl.find keyword_table word in
                      let tok = token_for_keyword kwd pos in
                        (
                          (* eprintf "keyword \"%s\" " word; *)
                          match kwd with
                              TrueKwd -> TRUE pos
                            | FalseKwd -> FALSE pos
                            | _ ->
                                raise (Lexer_error (Keyword_as_ident word, pos))
                        )
                    with Not_found ->
                      (* eprintf "ident \"%s\" " word; *)
                      IDENT (pos, word)
                  )
                    
              | Statement_mode ->
                  (
                    try
                      let kwd = Hashtbl.find keyword_table word in
                      let tok = token_for_keyword kwd pos in
                        (* eprintf "keyword \"%s\" " word; *)
                        if kwd = ElseKwd || kwd = EndKwd then
                          to_text_mode ();
                        tok
                    with Not_found ->
                      (* eprintf "ident \"%s\" " word; *)
                      reject_hash_at_end ();                  
                      IDENT (pos, word)
                  )
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                      pos)) }

    | '['
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  LBRACKET pos
                      
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   pos)) }

    | ']'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  RBRACKET pos
                      
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   pos)) }

    | '+'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  PLUS pos
                      
            | _ ->
                raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                   pos)) }

    | '-'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  MINUS pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }


    | '*'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  TIMES pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | '/'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  DIV pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '%'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  MOD pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '='
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  EQUALS pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | "=="
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  EQUALS_EQUALS pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | "!="
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  NOT_EQUALS pos
                      
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '<'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  LESS pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '>'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  GREATER pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | "<="
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  LESS_OR_EQUAL pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | ">="
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  GREATER_OR_EQUAL pos
                      
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | "&&"
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  AND pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | "||"
        { (* eprintf "%s " (Lexing.lexeme lexbuf); *)
          let pos = cur_pos#clone and
            str = Lexing.lexeme lexbuf in
            cur_pos#advance_char_no (String.length str);
            match !cur_mode with
                Text_mode ->
                  Buffer.add_string text_buf str;
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  OR pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | '!'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  NOT pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }

    | '.'
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  DOT pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | ','
        { (* eprintf "%c " (Lexing.lexeme_char lexbuf 0); *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | Expansion_mode | Statement_mode ->
                  COMMA pos
                  
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | _
        { (* eprintf "other[%d] " cur_pos#get_char_no; *)
          let pos = cur_pos#clone in
            cur_pos#incr_char_no;
            match !cur_mode with
                Text_mode ->
                  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
                  main lexbuf
                    
              | _ ->
                  raise (Lexer_error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                                     pos)) }
        
    | eof
        { (* eprintf "eof "; *)
          match !cur_mode with
              Text_mode ->
                let text_start_pos = mode_start_pos#clone in
                  to_eof_mode ();
                  cur_pos#incr_char_no;
                  let eof_start_pos = mode_start_pos#clone in
                    if (Buffer.length text_buf) > 0 then
                      TEXT (text_start_pos, eof_start_pos, Buffer.contents text_buf)
                    else
                      main lexbuf
                        
            | Eof_mode
            | Statement_mode ->
                (* print_newline (); *)
                EOF

            | Dollar_mode ->
                raise (Lexer_error (Unterminated_expansion, mode_start_pos))

            | Expansion_mode ->
                raise (Lexer_error (Unterminated_expansion, mode_start_pos)) }

and string_token = parse
    '\"'
      { (* eprintf "string_end[%d] " cur_pos#get_char_no; *)
        cur_pos#incr_char_no;
        () }

  | '\\' ['\\' '\"' 'n' 't' 'r']
      { cur_pos#advance_char_no 2;
        let char_to_add =
          match Lexing.lexeme_char lexbuf 1 with
              'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | c -> c
        in
          Buffer.add_char string_buf char_to_add;
          string_token lexbuf }

  | eof
      { raise (Lexer_error (Unterminated_string, string_start_pos)) }

  | _
      { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0);
        cur_pos#incr_char_no;
        string_token lexbuf }

and comment = parse
    '\"'
      { (* eprintf "string_in_comment[%d] " cur_pos#get_char_no; *)
        Buffer.reset string_buf;
        string_start_pos#set_from cur_pos;
        cur_pos#incr_char_no;
        string_token lexbuf;
        comment lexbuf }
      
  | "#*"
      { (* eprintf "nested_comment[%d] " cur_pos#get_char_no; *)
        comment_nesting_depth := !comment_nesting_depth + 1;
        cur_pos#advance_char_no 2;
        comment lexbuf }

  | "*#" '\n'?
      { (* eprintf "comment_end[%d] " cur_pos#get_char_no; *)
        let str = Lexing.lexeme lexbuf in
        let len = String.length str in
          if str.[len - 1] = '\n' then
            cur_pos#incr_line_no
          else
            cur_pos#advance_char_no len;
          comment_nesting_depth := !comment_nesting_depth - 1;
          if !comment_nesting_depth = 0 then
            ()
          else
            comment lexbuf }
      
  | '\n'
      { cur_pos#incr_line_no;
        comment lexbuf }

  | _
      { cur_pos#incr_char_no;
        comment lexbuf}
      
  | eof
      { raise (Lexer_error (Unterminated_comment, comment_start_pos)) }

and maybe_escape = parse
    '\\' * ("${" | '#' | '\n')
      { (* eprintf "escape[%d] " cur_pos#get_char_no; *)
        let pos = cur_pos#clone in
        let str = Lexing.lexeme lexbuf in
        let len = String.length str in
          if str.[len - 1] = '\n' then
            cur_pos#incr_line_no
          else
            (
              cur_pos#advance_char_no len;
              Buffer.add_string text_buf str
            );
          main lexbuf }
      
  | _
      { (* eprintf "non_escape[%d] " cur_pos#get_char_no; *)
        Buffer.add_char text_buf '\\';
        Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
        cur_pos#incr_char_no;
        main lexbuf }
