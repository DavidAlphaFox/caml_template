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

(* $Id: ctExpression.ml,v 1.14 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtTemplateModel ;;
open CtScope ;;
open CtExceptions ;;

(* Provides base classes for expressions and type conversion
functions for scalars. *)

exception TypeError of (string option) ;;

let get_type_name value =
  match value with
      Tnull -> "null"
    | Tstr _ -> "string"
    | Tint _ -> "integer"
    | Tfloat _ -> "float"
    | Tbool _ -> "boolean"
    | Tlist _ -> "list"
    | Thash _ -> "hash"
    | Tfun _ -> "function" ;;

let is_null value =
  match value with
      Tnull -> true
    | Tstr s -> s = ""
    | Tint i -> i = 0
    | Tfloat f -> f = 0.0
    | Tbool b -> not b
    | _ -> false ;;
  
let string_for_value value =
  match value with
      Tstr s -> s
    | Tint i -> string_of_int i
    | Tfloat f -> string_of_float f
    | Tbool b -> if b then "true" else ""
    | Tnull -> ""
    | _ -> raise (TypeError None) ;;
  
let int_for_value value =
  match value with
      Tint i -> i
    | Tstr s ->
        (
          try
            int_of_string s
          with
              Failure "int_of_string" -> raise (TypeError (Some s))
        )
    | Tbool b -> if b then 1 else 0
    | Tfloat f -> int_of_float f
    | Tnull -> 0
    | _ -> raise (TypeError None) ;;

let float_for_value value =
  match value with
      Tint i -> float_of_int i
    | Tstr s ->
        (
          try
            float_of_string s
          with
              Failure "float_of_string" -> raise (TypeError (Some s))
        )
    | Tbool b -> if b then 1.0 else 0.0
    | Tfloat f -> f
    | Tnull -> 0.0
    | _ -> raise (TypeError None) ;;

let bool_for_value value =
  match value with
      Tbool b -> b
    | Tstr s -> s <> ""
    | Tint i -> i <> 0
    | Tfloat f -> f <> 0.0
    | Tnull -> false
    | Tlist l -> l <> []
    | _ -> true ;;

(* Virtual base class for AST nodes that have a value. *)
class virtual expression ~(pos : source_pos) =
object (self)
  val pos = pos

  method get_pos = pos

  method virtual get_value : scope -> tvalue

  method virtual dump : string

  method get_desc = "expression"

  method value_to_string value =
    try
      string_for_value value
    with
        TypeError repr -> raise (self#conversion_error value repr "string")
  
  method to_string (cur_scope : scope) =
    let value = self#get_value cur_scope in
      self#value_to_string value
  
  method value_to_int value =
    try
      int_for_value value
    with
        TypeError repr -> raise (self#conversion_error value repr "integer")

  method to_int (cur_scope : scope) =
    let value = self#get_value cur_scope in
      self#value_to_int value

  method value_to_float value =
    try
      float_for_value value
    with
        TypeError repr -> raise (self#conversion_error value repr "float")

  method to_float (cur_scope : scope) =
    let value = self#get_value cur_scope in
      self#value_to_float value
        
  method value_to_bool value =
    try
      bool_for_value value
    with
        TypeError repr -> raise (self#conversion_error value repr "boolean")

  method to_bool (cur_scope : scope) =
    let value = self#get_value cur_scope in
      self#value_to_bool value

  method private conversion_error value repr dest_type =
    let detail =
      match repr with
          None ->
            sprintf "Can't convert %s, of type %s, to type %s"
            self#get_desc (get_type_name value) dest_type
            
        | Some str ->
            sprintf "Can't convert %s, of type %s (value %S), to type %s"
            self#get_desc (get_type_name value) str dest_type
    in let msg = template_error_message self#get_pos detail in
      Template_error msg
end ;;

(* Upcasting function for expressions. *)
let as_expression expr = (expr :> expression) ;;

(* Mixin class for all operators that return a boolean value.
  Derived classes must override to_bool.*)
class virtual boolean_op =
object (self)
  method get_value (cur_scope : scope) =
    Tbool (self#to_bool cur_scope)
end ;;
