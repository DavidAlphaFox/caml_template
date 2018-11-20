(*

  CamlTemplate: A template processor for Objective Caml programs.
  Copyright © 2003, 2004, 2005 Benjamin Geer
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St., 5th Floor, Boston MA 02110-1301
  USA
  
  In addition, as a special exception, Benjamin Geer gives permission
  to link the code of this program with the Apache HTTP Server (or
  with modified versions of Apache that use the same license as
  Apache), and distribute linked combinations including the two. You
  must obey the GNU General Public License in all respects for all of
  the code used other than Apache. If you modify this file, you may
  extend this exception to your version of the file, but you are not
  obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

*)

(* $Id: ctTemplate.ml,v 1.7 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtTemplateTypes ;;
open CtContext ;;
open CtScope ;;
open CtStatement ;;
open CtExceptions ;;


(*******************************************************************************
  The template implementation class.
*)

class template_impl
  (template_name : string)
  (statements : statement list)
  (macros : macro list)
  (get_template_fun : template_name:string -> template) =
object (self)
  val template_name = template_name
  val statements = statements
  val get_template_fun = get_template_fun
  val macro_cache =
    let hash = ((Hashtbl.create 16) : (string, macro) Hashtbl.t) in
      List.iter
        (function mac ->
           Hashtbl.replace hash mac#get_macro_name mac)
        macros;
      hash

  method merge ~model ~buf =
    let ctx = new context_impl
                ~template_name
                ~cur_scope:(new scope model)
                ~opened_modules:[ template_name ]
                ~buf
                ~get_template_fun
    in
      try
        interpret_statements ctx statements
      with Template_error msg ->
        let msg_with_name = sprintf "In template %s:\n%s" template_name msg in
          raise (Template_error msg_with_name)
            
  method get_name = template_name

  method get_macro macro_name =
    Hashtbl.find macro_cache macro_name
      
  method dump =
    sprintf "[template %s: %s]" template_name (dump_statements statements)
end ;;


(* Parses and constructs a template_impl from a string containing template
   source code. *)
let make_template template_name template_text get_template_fun =
  let lexbuf = Lexing.from_string template_text in
    (* Tell the parser the name of the current template. *)
    CtParserAux.current_template_name := template_name;
    
    try
      (* Parse and return the template. *)
      CtParserAux.reset_parser (); 
      CtLexer.reset_lexer ();
      let statements = CtParser.input CtLexer.main lexbuf in
      let macros = CtParserAux.get_macros () in
      let tmpl = new template_impl
                   template_name
                   statements
                   macros
                   get_template_fun in
        CtParserAux.clean_up_parser ();
        CtLexer.clean_up_lexer ();
        tmpl
    with
        Lexer_error (error_type, pos) ->
          let detail = lexer_error_message error_type in
          let msg = syntax_error_message template_name pos detail in
            raise (Syntax_error msg)
      | CtParserAux.ParserError (pos) ->
          let msg = syntax_error_message template_name pos "" in
            raise (Syntax_error msg)
      | Parsing.Parse_error ->
          let msg = syntax_error_message template_name CtLexer.cur_pos
                      "Unhandled parse error: please report as a bug" in
            raise (Syntax_error msg) ;;
