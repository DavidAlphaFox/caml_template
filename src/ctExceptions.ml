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

(* $Id: ctExceptions.ml,v 1.8 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;

(* Exceptions and related functions. *)

type lexer_error =
    Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Unterminated_expansion
  | Mismatched_parens
  | Keyword_as_ident of string ;;

exception Lexer_error of lexer_error * source_pos ;;

let lexer_error_message error_type =
  match error_type with
      Illegal_character ch ->
        sprintf "Illegal character '%c'" ch
    | Unterminated_string ->
        "Unterminated string"
    | Unterminated_comment ->
        "Unterminated comment"
    | Unterminated_expansion ->
        "Unterminated expansion"
    | Mismatched_parens ->
        "Mismatched parentheses"
    | Keyword_as_ident keyword ->
        sprintf "Keyword \"%s\" used as identifier" keyword ;;

exception Syntax_error of string ;;

let syntax_error_message template_name pos detail =
  if detail = "" then
    sprintf "In template %s:\nSyntax error near line %d, character %d"
      template_name pos#get_line_no pos#get_char_no
  else
    sprintf "In template %s:\nSyntax error near line %d, character %d:\n%s"
      template_name pos#get_line_no pos#get_char_no detail

exception Template_error of string ;;

let template_error_message pos detail =
  sprintf "At line %d, character %d:\n%s"
    pos#get_line_no pos#get_char_no detail ;;
