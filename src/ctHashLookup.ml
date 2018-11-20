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

(* $Id: ctHashLookup.ml,v 1.11 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtTemplateModel ;;
open CtScope ;;
open CtExpression ;;
open CtIdent ;;
open CtExceptions ;;

(* Base class for hash lookup operators defined below. *)
class virtual hash_lookup_op ~(pos : source_pos) ~(left : expression) =
object (self)
  inherit expression pos

  val left = left

  method virtual do_lookup : cur_scope:scope -> left_hash:thash -> tvalue

  method get_value cur_scope =
    let left_value = (left#get_value cur_scope) in
      match left_value with
          Thash left_hash ->
            self#do_lookup ~cur_scope ~left_hash
            
        | Tnull ->
            raise (Template_error (template_error_message self#get_pos "Value on left side of hash lookup is null"))
            
        | _ ->
            let detail =
              sprintf "Value on left side of hash lookup is of type %s (hash expected)"
                (get_type_name left_value) in
            let msg = template_error_message self#get_pos detail in
              raise (Template_error msg)
end ;;

(* Represents the dot operator. *)
class dot_op ~(pos : source_pos) ~(left : expression) ~(key : ident) =
object (self)
  inherit hash_lookup_op pos left

  val key = key

  method do_lookup ~cur_scope ~left_hash =
    try
      Hashtbl.find left_hash key#get_name
    with Not_found ->
      Tnull
                
  method get_desc =
    key#get_desc

  method dump =
    sprintf "(%s.%s)" left#dump key#dump
end ;;

(* Represents the bracket operator. *)
class bracket_op ~(pos : source_pos) ~(left : expression) ~(key : expression) =
object (self)
  inherit hash_lookup_op pos left

  val key = key

  method do_lookup ~cur_scope ~left_hash =
    let key_value = key#to_string cur_scope in
      try
        Hashtbl.find left_hash key_value
      with Not_found ->
        Tnull

  method get_desc =
    key#get_desc

  method dump =
    sprintf "(%s[%s])" left#dump key#dump
end ;;
