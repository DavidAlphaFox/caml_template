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

(* $Id: ctMacro.ml,v 1.13 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtTemplateModel ;;
open CtScope ;;
open CtTemplateTypes ;;
open CtExpression ;;
open CtIdent ;;
open CtStatement ;;
open CtExceptions ;;

(* Classes and functions relating to template macros. *)

(* Helper functions for the macro_impl class. *)

let add_arg_to_scope (cur_scope : scope) (arg_name : ident) (arg : tvalue) =
  cur_scope#add arg_name#get_name arg ;;

let remove_arg_from_scope (cur_scope : scope) (arg_name : ident) =
  cur_scope#remove arg_name#get_name ;;

(* Pads a list of macro arguments with nulls to make it the right length. *)
let pad_args args len =
  let rec pad_args_aux padded =
    if List.length padded = len then
      padded
    else
      pad_args_aux (Tnull :: padded)
  in
    List.rev (pad_args_aux (List.rev args))


(* A macro definition.  Macros are stored in templates, but they
   aren't statements. *)
class macro_impl
  ~(template_name : string)
  ~(pos : source_pos)
  ~(macro_name : string)
  ~(arg_names : ident list)
  ~(statements : statement list) =
object
  val template_name = template_name
  val pos = pos
  val macro_name = macro_name
  val arg_names = arg_names
  val statements = statements

  method call ~args ~ctx =
    let arg_count = List.length args in
    let arg_name_count = List.length arg_names in
      if arg_count > arg_name_count then
        let msg =
          sprintf "Too many arguments for macro %s, defined in template %s at line %d, character %d"
            macro_name template_name pos#get_line_no pos#get_char_no in
          raise (Template_error msg)
      else let padded_args =
        if arg_count < arg_name_count then
          pad_args args arg_name_count
        else
          args
      in
        List.iter2 (add_arg_to_scope ctx#get_cur_scope) arg_names padded_args;
        (
          try
            interpret_statements ctx statements;
          with Template_error msg ->
            let msg_with_macro_desc =
              sprintf "In template %s:\nIn macro %s:\n%s" template_name macro_name msg in
              raise (Template_error msg_with_macro_desc)
        );
        List.iter (remove_arg_from_scope ctx#get_cur_scope) arg_names
          
  method get_macro_name = macro_name
              
  method dump =
    dump_statements statements
end ;;
