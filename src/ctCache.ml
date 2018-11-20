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

(* $Id: ctCache.ml,v 1.12 2005-06-08 15:22:08 ben Exp $ *)

(*******************************************************************************
  Template loading and caching.
*)

open CtTemplateTypes ;;
open CtTemplate ;;

type source_check_result =
    TemplateUnchanged
  | TemplateChanged
  | TemplateDeleted ;;

class type source_loader =
object
  method check : template_name:string -> load_time:float -> source_check_result
  method load : template_name:string -> string
end ;;

let as_source_loader loader = (loader :> source_loader) ;;

class file_loader ~(template_dir : string) =
object
  val template_dir = template_dir

  method check ~template_name ~load_time =
    let filename = Filename.concat template_dir template_name in    
      try
        let file_stats = Unix.stat filename in
          if file_stats.Unix.st_mtime > load_time then
            TemplateChanged
          else
            TemplateUnchanged
      with
          Unix.Unix_error (error_code, fun_name, arg) as e ->
            if error_code = Unix.ENOENT then
              TemplateDeleted
            else
              raise e

  method load ~template_name =
    let filename = Filename.concat template_dir template_name in
      CtUtil.read_file_as_string filename
end ;;

let make_file_loader ~(template_dir : string) =
  as_source_loader (new file_loader template_dir) ;;

(* Represents an entry in the template cache. *)
type entry = {
  (* The parsed template. *)
  mutable tmpl : template;
  
  (* The time when the template was loaded. *)
  mutable load_time : float;
} ;;

(* The type of template caches. *)
type t = {
  (* Keys are template names. *)
  entries : (string, entry) Hashtbl.t;

  (* The template source loader. *)
  loader : source_loader;

  (* The interval at which the cache should be checked. *)
  check_interval : float;
  
  (* The last time the cache was checked. *)
  mutable last_check_time : float
} ;;

let create ?(loader = as_source_loader (new file_loader Filename.current_dir_name))
  ?(check_interval = 300.0) () =
  {
    entries = Hashtbl.create 16;
    loader = loader;
    check_interval = check_interval;
    last_check_time = 0.0
  } ;;

(* Refreshes the cache if necessary. *)
let rec check_cache cache =
  let current_time = Unix.time () in
    Hashtbl.iter
      (fun template_name entry ->
         match cache.loader#check template_name entry.load_time with
             TemplateUnchanged -> ()
           | TemplateChanged ->
               entry.tmpl <- (make_template template_name
                                (cache.loader#load template_name)
                                (get_template ~cache));
               entry.load_time <- current_time
           | TemplateDeleted -> Hashtbl.remove cache.entries template_name)
      cache.entries;
    cache.last_check_time <- current_time

(* Loads a template from the cache.*)
and get_template ~cache ~template_name =
  CtUtil.call_in_mutex
    ~f:(function () ->
          (* Is it time to check the cache? *)
          let current_time = Unix.time () in
            if cache.check_interval >= 0.0 &&
              current_time -. cache.last_check_time >= cache.check_interval then
                check_cache cache;
            
            (* Do we have the template? *)
            try
              let entry = Hashtbl.find cache.entries template_name in
                entry.tmpl
            with Not_found ->
              (* No; load it. *)
              let new_tmpl =
                make_template
                  template_name
                  (cache.loader#load template_name)
                  (get_template ~cache) in
              let new_entry = { tmpl = new_tmpl;
                                load_time = current_time } in
                Hashtbl.add cache.entries template_name new_entry;
                new_tmpl) ~mutex:!CtUtil.static_mutex ;;
