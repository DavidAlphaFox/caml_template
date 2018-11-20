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

(* $Id: ctUtil.ml,v 1.21 2005-06-08 15:22:09 ben Exp $ *)

(* Miscellaneous utility functions. *)

(* Returns the contents of a file as a string.  (Thanks to Markus
   Mottl.) *)
let read_file_as_string filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
    try
      let buf = String.create size in
        really_input file buf 0 size;
        close_in file;
        buf
    with e ->
      (try close_in file with _ -> ());
      raise e

(* Implements try-with-finally. *)
let try_with_finally ~(f : unit -> 'a) ~(finally: unit -> unit) =
  let res =
    try
      f ()
    with e ->
      finally ();
      raise e
  in
    finally ();
    res ;;

type mutex_pair = 
    { 
      lock : unit -> unit;
      unlock : unit -> unit;
    }

(* In multi-threading mode, the variable create_lock_unlock_pair is
   initialized right at startup with a reasonable mutex creator.  In
   non-mt mode, the default value of this variable contains a dummy
   creator that does not lock anything.  Don't use this function to
   create a static mutex; instead, use the static_mutex reference
   defined below. *)
let create_lock_unlock = 
  ref (fun () -> 
         { lock = (fun () -> ()); unlock = (fun () -> ()) })

(* Locks a mutex, calls a function, then unlocks the mutex.  The mutex
   is also unlocked if the function throws an exception. *)
let call_in_mutex ~(f : unit -> 'a) ~mutex =
  mutex.lock ();
  try_with_finally ~f ~finally:(function () -> mutex.unlock ()) ;;

(* A static real or dummy mutex that can be used by other modules.
   Don't store the value of this reference in a variable; instead, it
   must be dereferenced every time it is used. *)
let static_mutex =
  ref (!create_lock_unlock ()) ;;
