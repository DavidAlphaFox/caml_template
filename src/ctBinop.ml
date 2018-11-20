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

(* $Id: ctBinop.ml,v 1.10 2005-06-08 15:22:08 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtTemplateModel ;;
open CtExpression ;;
open CtExceptions ;;

(* Most binary operators are defined in this module. *)

(* A virtual base class for binary operators. *)
class virtual binary_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit expression pos

  val left = left
  val right = right

  method calculate left_value right_value int_fun float_fun =
    (* Promote ints to floats if necessary. *)
    match (left_value, right_value) with
        (Tfloat left_float, _) -> Tfloat (float_fun left_float (right#value_to_float right_value))
      | (_, Tfloat right_float) -> Tfloat (float_fun (left#value_to_float left_value) right_float)
      | (_, _) -> Tint (int_fun (left#value_to_int left_value) (right#value_to_int right_value))
    
end ;;

(* Addition and string concatenation operator. *)
class plus_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right

  method get_value cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      match left_value with
          (* Concatenate strings if the lhs is a string. *)
          Tstr s -> Tstr (s ^ (right#value_to_string right_value))
        | _ -> self#calculate left_value right_value (+) (+.)
                
  method dump =
    sprintf "(%s + %s)" left#dump right#dump
end ;;

(* Subtraction operator. *)
class minus_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right

  method get_value cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      self#calculate left_value right_value (-) (-.)

  method dump =
    sprintf "(%s - %s)" left#dump right#dump
end ;;

(* Multiplication operator. *)
class times_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right

  method get_value cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      self#calculate left_value right_value ( * ) ( *. )

  method dump =
    sprintf "(%s * %s)" left#dump right#dump
end ;;

(* Division operator. *)
class div_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right

  method get_value cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      try
        self#calculate left_value right_value (/) (/.)
      with Division_by_zero ->
        raise (Template_error (template_error_message pos "Division by zero"))
        
  method dump =
    sprintf "(%s / %s)" left#dump right#dump
end ;;

(* Integer modulo operator. *)
class mod_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right

  method get_value cur_scope =
    try
      Tint ((left#to_int cur_scope) mod (right#to_int cur_scope))
    with Division_by_zero ->
      raise (Template_error (template_error_message pos "Division by zero"))

  method dump =
    sprintf "(%s %% %s)" left#dump right#dump
end ;;

(* Logical AND operator. *)
class and_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right
  inherit boolean_op

  method to_bool cur_scope =
    (left#to_bool cur_scope) && (right#to_bool cur_scope)

  method dump =
    sprintf "(%s && %s)" left#dump right#dump
end ;;

(* Logical OR operator. *)
class or_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right
  inherit boolean_op

  method to_bool cur_scope =
    (left#to_bool cur_scope) || (right#to_bool cur_scope)

  method dump =
    sprintf "(%s || %s)" left#dump right#dump
end ;;

(* Virtual base class for operators that compare two values. *)
class virtual comparison_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit binary_op pos left right
  inherit boolean_op

  method do_compare left_value right_value =
    match (left_value, right_value) with
        (* Compare strings if the lhs is a string.  Otherwise, promote ints to floats if necessary. *)
        (Tstr left_str, _) -> compare left_str (right#value_to_string right_value)
      | (Tfloat left_float, _) -> compare left_float (right#value_to_float right_value)
      | (_, Tfloat right_float) -> compare (left#value_to_float left_value) right_float
      | (Tint left_int, _) -> compare left_int (right#value_to_int right_value)
      | (Tbool left_bool, _) -> compare left_bool (right#value_to_bool right_value)
      | _ ->
          let detail = sprintf "Can't compare %s of type %s with %s of type %s"
                         left#get_desc (get_type_name left_value)
                         right#get_desc (get_type_name right_value)
          in
            raise (Template_error (template_error_message self#get_pos detail))
end

(* Equality operator. *)
class equals_equals_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      match left_value with
          Tnull -> is_null right_value
        | _ -> ((self#do_compare left_value right_value) = 0)

  method dump =
    sprintf "(%s == %s)" left#dump right#dump
end ;;

(* Inequality operator. *)
class not_equals_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      match left_value with
          Tnull -> is_null right_value
        | _ -> ((self#do_compare left_value right_value) <> 0)

  method dump =
    sprintf "(%s <> %s)" left#dump right#dump
end ;;

(* Less-than operator. *)
class less_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      (self#do_compare left_value right_value) < 0

  method dump =
    sprintf "(%s < %s)" left#dump right#dump
end ;;

(* Greater-than operator. *)
class greater_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      (self#do_compare left_value right_value) > 0

  method dump =
    sprintf "(%s > %s)" left#dump right#dump
end ;;

(* Less-than-or-equal-to operator. *)
class less_or_equal_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      (self#do_compare left_value right_value) <= 0

  method dump =
    sprintf "(%s < %s)" left#dump right#dump
end ;;

(* Greater-than-or-equal-to operator. *)
class greater_or_equal_op ~(pos : source_pos) ~(left : expression) ~(right : expression) =
object (self)
  inherit comparison_op pos left right

  method to_bool cur_scope =
    let left_value = (left#get_value cur_scope) in
    let right_value = (right#get_value cur_scope) in
      (self#do_compare left_value right_value) >= 0

  method dump =
    sprintf "(%s >= %s)" left#dump right#dump
end ;;
