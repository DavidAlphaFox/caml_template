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

(* $Id: ctTemplateTypes.ml,v 1.4 2005-06-08 15:22:09 ben Exp $ *)

open CtTemplateModel ;;
open CtScope ;;

(* The template, macro and context abstractions used internally by the
   interpreter.  These are all in one place because they're
   interdependent. *)

(* Instances of the template type are returned to the user (as an
   opaque type). *)
class type template =
object
  method merge : model:thash -> buf:Buffer.t -> unit
  method get_name : string
  method dump : string
  method get_macro : string -> macro
end
(* Represents a macro definition; macros are kept in the templates where
   they were defined. *)
and macro =
object
  method call : args:(tvalue list) -> ctx:context -> unit
  method get_macro_name : string              
  method dump : string
end
(* Holds the interpreter's context while a template is being merged. *)
and context =
object
  method get_template_name : string
  method get_cur_scope : scope
  method get_opened_modules : string list
  method add_opened_module : string -> unit
  method get_buf : Buffer.t
  method get_template : string -> template
  method make_macro_context : context
end ;;
