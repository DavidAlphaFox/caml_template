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

(* $Id: ctCache.mli,v 1.5 2005-06-08 15:22:09 ben Exp $ *)

(* See CamlTemplate.mli for a description of this module. *)

open CtTemplateTypes ;;

type t ;;
  
type source_check_result =
    TemplateUnchanged
  | TemplateChanged
  | TemplateDeleted ;;
      
class type source_loader =
object
  method check :
    template_name:string -> load_time:float -> source_check_result
  method load : template_name:string -> string
end ;;

val as_source_loader : #source_loader -> source_loader ;;

val make_file_loader : template_dir:string -> source_loader ;;
      
val create : ?loader:source_loader -> ?check_interval:float -> unit -> t ;;

val get_template :
  cache:t -> template_name:string -> template ;;
