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

(* $Id: ctIdent.ml,v 1.10 2005-06-08 15:22:09 ben Exp $ *)

open CtSourcePos ;;
open CtTemplateModel ;;
open CtScope ;;
open Printf ;;
open CtExpression ;;

(* Represents identifiers. *)
class ident ~(pos : source_pos) ~(name : string) =
object (self)
  inherit expression pos

  val name = name

  method get_value cur_scope =
    cur_scope#lookup name

  method get_name = name
      
  method get_desc =
    sprintf "'%s'" name

  method dump = name                  
end ;;
