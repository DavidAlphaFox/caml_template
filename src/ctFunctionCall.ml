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

(* $Id: ctFunctionCall.ml,v 1.10 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtTemplateModel ;;
open CtExpression ;;
open CtExceptions ;;

(* Implements calls to functions in the template model. *)
class function_call_op ~(pos : source_pos) ~(left : expression) ~(params : expression list) =
object (self)
  inherit expression pos

  val left = left
  val params = params

  method get_value model =
    let left_value = (left#get_value model) in
        match left_value with
            Tfun f ->
              (
                let get_param_value model expr =
                  expr#get_value model in
                let param_values = List.map (get_param_value model) params in
                  try
                    f param_values
                  with
                      Tfun_error text ->
                        let detail = sprintf "Error in function %s: %s"
                                       left#get_desc text in
                        let msg = template_error_message pos detail in
                          raise (Template_error msg)
              )

          | Tnull ->
              let detail = sprintf "Function %s is null" left#get_desc in
              let msg = template_error_message self#get_pos detail in
                raise (Template_error msg)
                  
          | _ ->
              let detail = sprintf "%s is of type %s (function expected)"
                             left#get_desc (get_type_name left_value) in
              let msg = template_error_message self#get_pos detail in
                raise (Template_error msg)

  method dump =
    sprintf "(%s(...))" left#dump
end ;;
