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

(* $Id: thread_test.ml,v 1.11 2005-06-08 15:22:08 ben Exp $ *)

open Printf ;;
open CamlTemplate.Model ;;

let thread_fun cache thread_num =
  (* Create a model. *)
  let model = Hashtbl.create 4 in
    Hashtbl.add model "threadNum" (Tint thread_num);
    for i = 1 to 5 do
      try
        (* Get the template. *)
        let tmpl = CamlTemplate.Cache.get_template cache "thread_test.tmpl"
                     
        (* Make a buffer for the output. *)
        and buf = Buffer.create 256 in
          
          (* Generate output. *)
          CamlTemplate.merge tmpl model buf;
          printf "Thread %d: %s\n" thread_num (Buffer.contents buf);
          Thread.yield ()
      with
          CamlTemplate.Syntax_error msg ->
            eprintf "\n%s\n" msg
        | CamlTemplate.Template_error msg ->
            eprintf "\n%s\n" msg
    done ;;

let _ =
  (* Make a template cache. *)
  let cache = CamlTemplate.Cache.create () in
    
  let threads = Stack.create () in
    for thread_num = 1 to 5 do
      Stack.push (Thread.create (thread_fun cache) thread_num) threads
    done;
    
    try
      while true do
        Thread.join (Stack.pop threads)
      done
    with Stack.Empty ->
      () ;;
