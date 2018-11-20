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

(* $Id: camlTemplate.mli,v 1.37 2005-06-08 15:22:08 ben Exp $ *)

(** A template processor.

  Copyright © 2003, 2004, 2005 Benjamin Geer.  Please see the file COPYING
  for licence information.

  The latest version of this software can be found at
  {{:http://saucecode.org/camltemplate}http://saucecode.org/camltemplate}.
*)

(** {1 Overview}

  To use templates, first write template source code (see the manual
  for instructions).  Then create a template cache using the
  {!CamlTemplate.Cache} module, and call
  {!CamlTemplate.Cache.get_template} to create a
  {!CamlTemplate.template} from your template source code.

  To marge a template with data, put the data in a
  {!CamlTemplate.Model.thash}.  Then call {!CamlTemplate.merge}.
*)

(** {1 Template Data Models} *)
 
(** Provides the types used in the data models that are merged with
  templates. *)
module Model : sig
  (** A value in a template data model. *)
  type tvalue =
      Tnull (** A null value. *)
    | Tstr of string (** A string value. *)
    | Tint of int (** An integer value. *)
    | Tfloat of float (** A floating-point value. *)
    | Tbool of bool (** A boolean value. *)
    | Tlist of tlist (** A list. *)
    | Thash of thash (** A set of key-value pairs. *)
    | Tfun of tfun
        (** An Objective Caml function that can be called by a template. *)
  and tlist = tvalue list
      (** The type contained in a [Tlist]: a list of
        [tvalue]s. *)
  and thash = (string, tvalue) Hashtbl.t
      (** The type contained in a [Thash]: a collection of
        [tvalue]s, each of which has a name. *)
  and tfun = (args:(tvalue list) -> tvalue)
      (** The type contained in a [Tfun]: a function that
        takes [tvalue]s as arguments, and returns a
        [tvalue]. *)

  (** An exception that [tfun] functions can raise when
    called by a template. *)
  exception Tfun_error of string
end ;;

(** {1 Templates} *)

(** Represents a parsed template. *)
type template ;;

(** Merges the data in a {!CamlTemplate.Model.thash} with
  the template, and returns the resulting text in the buffer
  provided.
  
  @raise Template_error if an error occurs in the
  template.  *)
val merge : tmpl:template -> model:Model.thash -> buf:Buffer.t -> unit ;;

(** Returns the name of a template. *)
val get_name : template -> string ;;

(** Returns a simple string representation of the parse tree,
  for debugging purposes. *)
val dump : template -> string ;;

(** {1 Exceptions} *)

(** Raised if an error is found when parsing template source code. *)
exception Syntax_error of string ;;

(** Raised if an error occurs when merging data with a template. *)  
exception Template_error of string ;;

(** {1 Template Loading and Caching} *)

(** Caches templates. *)
module Cache :
sig
  (**
    {1 Customising How Source Code is Loaded}
  *)

  (** The type returned by a template source loader when it checks
    whether a template's source code has changed. *)
  type source_check_result =
      TemplateUnchanged
        (** Indicates that the source code of the checked template has
          not changed since it was loaded. *)
    | TemplateChanged
        (** Indicates that the source code of the checked template has
          changed since it was loaded. *)
    | TemplateDeleted
        (** Indicates that the source code of the checked template was
          deleted since it was loaded. *)
  ;;

  (** An implementation of this class type can be used by a template
    cache to load template source code. *)
  class type source_loader =
  object
    (** Checks whether a template's source has changed since it was last
      loaded.  The load time is a time in seconds, as returned by
      [Unix.time]. *)
    method check : template_name:string -> load_time:float -> source_check_result

    (** Loads the source code for a template. *)
    method load : template_name:string -> string
  end ;;

  (** Upcasting function for {!CamlTemplate.Cache.source_loader}. *)
  val as_source_loader : #source_loader -> source_loader ;;

  (** Returns a {!CamlTemplate.Cache.source_loader} that loads template source code
    from files in a directory.  The name of each template is used as the
    filename.

    @param template_dir the directory in which the template source files
    are located.
  *)
  val make_file_loader : template_dir:string -> source_loader ;;

  (**
    {1 Using Template Caches}
  *)

  (** The type of template caches. *)
  type t

  (** Creates a template cache.

    @param loader the [source_loader] that will be used to load
    template source code for the cache.  If omitted, the cache uses a
    [source_loader] that loads template source code from the current
    working directory.

    @param check_interval the interval at which the template cache
    should be refreshed.  The default is 5 minutes.  If the interval
    is zero, the cache will be refreshed every time
    {!CamlTemplate.Cache.get_template} is called.  If the interval is
    negative, it will never be refreshed.
  *)
  val create : ?loader:source_loader -> ?check_interval:float -> unit -> t ;;

  (** Given a cache and the name of a template, returns the template from the
    cache.  If the template is not in the cache, it is loaded
    and cached.
    
    If the cache is due to be refreshed, this method refreshes the cache
    (i.e. reloads any templates that have been modified since they were
    last loaded, and removes any deleted templates from the cache)
    before looking for the requested template.

    @raise CamlTemplate.Syntax_error if a template cannot be parsed.
  *)
  val get_template : cache:t -> template_name:string -> template ;;
end ;;

(** {1 Miscellaneous} *)

(**
  Adds the following template functions to a template data model:

  - [urlEncode] URL-encodes a string.
  - [escHtml] Escapes special characters in text to be included in an HTML document.
  - [escHtmlAttr] Escapes special characters in text to be included in an HTML attribute.
  - [escHtmlTextarea] Escapes special characters in text to be included in an HTML [textarea].
  - [asList] Converts any value to a list, if it isn't already a list.  If the argument is
  a list, returns the argument.  If the argument is null, returns an empty list.  Otherwise,
  returns a single-element list containing the argument.  This may be useful for dealing with
  form input fields that can have multiple values.

  Each of these functions expects one argument.
*)
val add_web_functions : Model.thash -> unit ;;
