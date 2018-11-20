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

(* $Id: ctSourcePos.ml,v 1.6 2005-06-08 15:22:09 ben Exp $ *)

(* Represents a position (line number and character number) in
  template source code.  Line numbers are 1-based; character numbers
  are 0-based. *)
class source_pos =
object (self)
  val mutable line_no = 1
  val mutable char_no = 0

  method get_line_no =
    line_no

  method set_line_no ln =
    line_no <- ln

  method get_char_no =
    char_no

  method set_char_no cn =
    char_no <- cn

  method set_from (pos : source_pos) =
    line_no <- pos#get_line_no;
    char_no <- pos#get_char_no;

  method advance_char_no offset =
    char_no <- char_no + offset

  method incr_line_no =
    line_no <- line_no + 1;
    self#reset_char_no;

  method incr_char_no =
    char_no <- char_no + 1

  method reset_line_no =
    line_no <- 1

  method reset_char_no =
    char_no <- 0

  method reset =
    self#reset_line_no;
    self#reset_char_no;

  method is_origin =
    line_no = 1 && char_no = 0
  
  method clone = {< >}
end ;;
