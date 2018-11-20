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

(* $Id: filetest.ml,v 1.22 2008-06-25 08:10:41 gds Exp $ *)

open Printf ;;
open CamlTemplate.Model ;;

(* The sort of data you might get from a database. *)

let poem_row_names = [ "title"; "year"; "excerpt" ] ;;

let poem_rows =
  [ [ Tstr "Kubla Khan";
      Tint 1797;
      Tstr "In Xanadu did Kubla Khan / A stately pleasure-dome decree." ];
    [ Tstr "Christabel";
      Tint 1800;
      Tstr ("'Tis the middle of the night by the castle clock, / " ^
      "And the owls have awakened the crowing cock;") ];
    [ Tstr "Dejection: An Ode";
      Tint 1802;
      Tstr ("Well! If the Bard was weather-wise, who made / " ^
      "The grand old ballad of Sir Patrick Spence") ];
    [ Tstr "The Rime of the Ancient Mariner";
      Tint 1817;
      Tstr "It is an ancient Mariner / And he stoppeth one of three." ] ] ;;

let add_to_hash hash name value = Hashtbl.add hash name value; hash ;;

let make_hash_from_row poem_row =
  let row_hash = Hashtbl.create 3 in
    Thash (List.fold_left2 add_to_hash row_hash poem_row_names poem_row) ;;

let make_poem_table () =
  Tlist (List.map make_hash_from_row poem_rows) ;;

(* A hash containing he birthdate of Samuel Taylor Coleridge, in hash
   keys "day" "month" and "year". *)

let birthdate =
  let hash = Hashtbl.create 3 in
    Hashtbl.add hash "day" (Tint 21);
    Hashtbl.add hash "month" (Tint 10);
    Hashtbl.add hash "year" (Tint 1772);
    Thash hash ;;

(* A template function that returns a substring of a string. *)

let substring ~args =
  match args with
      [ Tstr str; Tint start; Tint len ] ->
        Tstr (String.sub str start len)
    | _ -> raise (Tfun_error "Invalid argument") ;;

(* Puts all of the above and some other bits into a template model. *)

let make_model () =
  let root = Hashtbl.create 10 in
    Hashtbl.add root "title" (Tstr "CamlTemplate Test Page");
    Hashtbl.add root "poet" (let hash = Hashtbl.create 2 in
                               Hashtbl.add hash "name" (Tstr "Samuel Taylor Coleridge");
                               Hashtbl.add hash "birthdate" birthdate;
                               Thash hash);
    Hashtbl.add root "substring" (Tfun substring);
    Hashtbl.add root "someText" (Tstr "<Sam & Sara>");
    Hashtbl.add root "poems" (make_poem_table ());
    CamlTemplate.add_web_functions root;
    root ;;

(* Runs coleridge.tmpl with the above model. *)

let _ =
  (* Merge a template. *)
  let input_filename = "coleridge.tmpl"
  and output_filename = "coleridge.html" in
    eprintf "Reading %s\n" input_filename;
    let cache = CamlTemplate.Cache.create () in
      try
        let tmpl = CamlTemplate.Cache.get_template cache input_filename in
          (* eprintf "\nParse tree:\n%s\n" (CamlTemplate.dump tmpl); *)
        let buf = Buffer.create 256 in
          CamlTemplate.merge ~tmpl ~model:(make_model ()) ~buf;
          let out = open_out output_filename in
            set_binary_mode_out out true;
            output_string out (Buffer.contents buf);
            close_out out;
            eprintf "Wrote %s\n" output_filename
      with
          CamlTemplate.Syntax_error msg ->
            eprintf "\n%s\n" msg; exit 1
        | CamlTemplate.Template_error msg ->
            eprintf "\n%s\n" msg; exit 1 ;;
