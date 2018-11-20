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

(* $Id: ctWeb.ml,v 1.13 2008-06-25 07:51:22 gds Exp $ *)

(* Provides template functions for escaping URLs and HTML text. *)
open Str;;
open CtTemplateModel ;;

(* Code from wserver.ml, (C) 1997 Daniel de Rauglaudre, INRIA. *)

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x) ;;

(* Characters to be escaped in URLs, according to RFC 1738. *)

let special = function 
  | '\000'..'\031' | '\127'..'\255'                      (* non US ASCII *)
  | '<' | '>' | '"' | '#' | '%'                          (* space should be here, but its encoding uses only one char *)
  | '{' | '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' (* unsafe *)
  | ';' | '/' | '?' | ':' | '@' | '=' | '&'              (* reserved *)
      -> true
  | '+' -> true
  | _ -> false ;;
      
(* URL-encodes a string. *)
      
let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        | ' ' -> true
    | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
    compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          | ' ' -> s1.[i1] <- '+'; succ i1
          | c ->
              if special c then
                (
                  s1.[i1] <- '%';
                  s1.[i1 + 1] <- hexa_digit (Char.code c / 16);
                  s1.[i1 + 2] <- hexa_digit (Char.code c mod 16);
                  i1 + 3
                )
              else ( s1.[i1] <- c; succ i1 )
      in
      copy_code_in s1 (succ i) i1
    else 
      s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else 
    s ;;

(* Characters that may need to be escaped in HTML. *)
let amp_re = regexp "&" ;;
let lt_re = regexp "<" ;;
let gt_re = regexp ">" ;;
let quot_re = regexp "\"" ;;
let eol_re = regexp "\r?\n" ;;

(* Escapes HTML text between tags. *)
let esc_html str =
  let str = global_replace amp_re "&amp;" str in
  let str = global_replace lt_re "&lt;" str in
  let str = global_replace gt_re "&gt;" str in
  let str = global_replace quot_re "&quot;" str in
  let str = global_replace eol_re "<br>" str in
    str ;;

(* Escapes HTML text in attributes. *)
let esc_html_attr str =
  let str = global_replace amp_re "&amp;" str in
  let str = global_replace lt_re "&lt;" str in
  let str = global_replace gt_re "&gt;" str in
  let str = global_replace quot_re "&quot;" str in
  let str = global_replace eol_re "&#13;&#10;" str in
    str ;;

(* Escapes HTML text in the value of a <textarea>. *)
let esc_html_textarea str =
  let str = global_replace amp_re "&amp;" str in
  let str = global_replace lt_re "&lt;" str in
  let str = global_replace gt_re "&gt;" str in
  let str = global_replace quot_re "&quot;" str in
    str ;;

(* Template function wrappers for the above functions. *)

let url_encode_tfun ~args =
  match args with
      [ Tstr str ] ->
        Tstr (encode str)
    | _ -> raise (Tfun_error "Invalid argument") ;;

let esc_html_tfun ~args =
  match args with
      [ Tstr str ] ->
        Tstr (esc_html str)
    | _ -> raise (Tfun_error "Invalid argument") ;;

let esc_html_attr_tfun ~args =
  match args with
      [ Tstr str ] ->
        Tstr (esc_html_attr str)
    | _ -> raise (Tfun_error "Invalid argument") ;;

let esc_html_textarea_tfun ~args =
  match args with
      [ Tstr str ] ->
        Tstr (esc_html_textarea str)
    | _ -> raise (Tfun_error "Invalid argument") ;;

(* Other HTML-related functions. *)

(* Converts any tvalue to a list, if it isn't already a list. *)
let as_list_tfun ~args =
  match args with
    [ Tnull ] -> Tlist []
  | [ Tstr str ] -> Tlist [ Tstr str ]
  | [ Tint i ] -> Tlist [ Tint i ]
  | [ Tbool b ] -> Tlist [ Tbool b ]
  | [ Tlist ls ] -> Tlist ls
  | [ Thash h ] -> Tlist [ Thash h ]
  | [ Tfun f ] -> Tlist [ Tfun f ]
  | _ -> raise (Tfun_error "Invalid argument") ;;


(* Adds these functions to a template data model. *)
let add_web_functions model =
  Hashtbl.add model "urlEncode" (Tfun url_encode_tfun);
  Hashtbl.add model "escHtml" (Tfun esc_html_tfun);
  Hashtbl.add model "escHtmlAttr" (Tfun esc_html_attr_tfun);
  Hashtbl.add model "escHtmlTextarea" (Tfun esc_html_textarea_tfun);
  Hashtbl.add model "asList" (Tfun as_list_tfun) ;;
