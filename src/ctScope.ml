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

(* $Id: ctScope.ml,v 1.4 2005-06-08 15:22:09 ben Exp $ *)

open CtTemplateModel ;;


(* Constructor called by template when invoked *)
class scope (model : thash) =
object
  val model = model
  val template_scope : thash = Hashtbl.create 8
  val macro_scope : thash option = None

  method get_model = model

  (* Returns the value of a variable in the innermost scope
     where it's set. *)
  method lookup key =
    try
      match macro_scope with
          Some hash ->
            Hashtbl.find hash key
        | None -> raise Not_found
      with Not_found ->
        try
          Hashtbl.find template_scope key
        with Not_found ->
          try
            Hashtbl.find model key
          with Not_found ->
            Tnull

  (* Sets the value of a variable in macro scope if it's already
     set there, otherwise in template scope. *)
  method set key value =
    try
      match macro_scope with
          Some hash ->
            ignore (Hashtbl.find hash key);
            Hashtbl.replace hash key value
        | None -> raise Not_found
      with Not_found ->
        Hashtbl.replace template_scope key value

  (* Adds a variable in the innermost available scope, shadowing
     any existing value. *)
  method add key value =
    try
      match macro_scope with
          Some hash ->
            Hashtbl.add hash key value
        | None -> raise Not_found
      with Not_found ->
        Hashtbl.add template_scope key value

  (* Removes a variable added with #add *)
  method remove key =
    try
      match macro_scope with
          Some hash ->
            ignore (Hashtbl.find hash key);
            Hashtbl.remove hash key
        | None -> raise Not_found
      with Not_found ->
        Hashtbl.remove template_scope key

  (* Defines a variable in the innermost available scope. *)
  method define key value =
    match macro_scope with
        Some hash ->
          Hashtbl.replace hash key value
      | None ->
          Hashtbl.replace template_scope key value

  (* Call this method on a template scope to make a macro scope. *)
  method make_macro_scope =
    {< macro_scope = Some (Hashtbl.create 8) >}
end ;;
