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

(* $Id: ctStatement.ml,v 1.16 2005-06-08 15:22:09 ben Exp $ *)

open Printf ;;
open CtSourcePos ;;
open CtScope ;;
open CtTemplateModel ;;
open CtExpression ;;
open CtIdent ;;
open CtExceptions ;;
open CtTemplateTypes ;;

(* This module includes all of the statement classes. *)

(* Class type for AST nodes that produce output. *)
class type statement =
object
  method interpret : ctx:context -> unit
  method dump : string
end ;;

(* Upcasting function for statements. *)
let as_statement stmt = (stmt :> statement) ;;

(* Interprets a statement, using the specified context. *)
let interpret_statement (ctx : context) (stmt : statement) =
  stmt#interpret ctx

(* Interprets a list of statements, using the specified context. *)
let interpret_statements (ctx : context) (statements : statement list) =
  List.iter (interpret_statement ctx) statements

(* Dumps the parse tree of a list of statements. *)
let dump_statements (statements : statement list) =
  List.fold_left (fun str stmt -> sprintf "%s%s" str stmt#dump) "" statements

(* A statement that just outputs some text. *)
class text_statement ~(text : string) =
object
  val text = text

  method interpret ~(ctx : context) =
    Buffer.add_string ctx#get_buf text

  method dump =
    "[text]"
end ;;

(* A statement that prints the value of an expression. *)
class expansion_statement ~(expr : expression) =
object
  val expr = expr

  method interpret ~(ctx : context) =
    Buffer.add_string ctx#get_buf (expr#to_string ctx#get_cur_scope)

  method dump =
    sprintf "[print %s]" expr#dump
end ;;

(* A standard foreach statement. *)
class for_each_statement
  ~(index : ident) ~(list_expr : expression) ~(statements : statement list) =
object (self)
  val index = index
  val list_expr = list_expr
  val statements = statements

  (* Fishes out the list from a Tlist. *)
  method private get_value_list cur_scope =
    let list_expr_value = list_expr#get_value cur_scope in
      match list_expr_value with
          Tlist ls -> ls
            
        | Tnull ->
            raise (Template_error (template_error_message list_expr#get_pos "List is null"))
            
        | _ ->
            let detail = sprintf "Value after \"in\" is of type %s (list expected)"
                        (get_type_name list_expr_value) in
            let msg = template_error_message list_expr#get_pos detail in
              raise (Template_error msg)
  
  method interpret ~(ctx : context) =
    let index_name = index#get_name
    and value_list = self#get_value_list ctx#get_cur_scope
    in let for_each_iter elem =
        (* Within the scope of the foreach, the value of the index
           variable hides the value of any variable with the same
           name. *)
        ctx#get_cur_scope#add index_name elem;
         interpret_statements ctx statements;
         ctx#get_cur_scope#remove index_name
    in
      List.iter for_each_iter value_list
        
  method dump =
    sprintf "[foreach %s in %s %s]" index#dump list_expr#dump (dump_statements statements)
end ;;

(* Represents an 'if' or 'elseif' branch. *)
type cond_branch = expression * (statement list) ;;

(* A standard if-elseif-else statement. *)
class if_statement ~(cond_branches : cond_branch list) ~(else_statements : statement list) =
object (self)
  val cond_branches = cond_branches
  val else_statements = else_statements

  method private interpret_branch ctx branch =
    let expr = fst branch in
    let branch_statements = snd branch in
      if expr#to_bool ctx#get_cur_scope then
        (
          interpret_statements ctx branch_statements;
          true
        )
      else
        false

  method interpret ~(ctx : context) =
    if not (self#interpret_branches ctx) then     
      interpret_statements ctx else_statements

  method private interpret_branches ctx =
    let rec interpret_branches_aux branches =
      match branches with
          [] -> false
        | branch :: tail ->
            (self#interpret_branch ctx branch) || (interpret_branches_aux tail)
    in
      interpret_branches_aux cond_branches

  method dump =
    let else_dump =
      match else_statements with
          [] -> ""
        | _ -> sprintf "[else %s]"
            (dump_statements else_statements)
    in
      sprintf "[if %s%s]" self#dump_branches else_dump
            
  method private dump_branches =
    List.fold_left
      (fun str (expr, stmts) -> sprintf "%s[cond_branch %s: %s]"
         str expr#dump (dump_statements stmts)) "" cond_branches
end ;;

(* A statement that sets the value of a variable. *)
class set_statement ~(left : ident) ~(right : expression) =
object
  val left = left
  val right = right

  method interpret ~(ctx : context) =
    let ident_name = left#get_name in
      ctx#get_cur_scope#set ident_name (right#get_value ctx#get_cur_scope)

  method dump =
    sprintf "[set %s = %s]" left#dump right#dump
end ;;

(* A statement that defines a variable in the innermost scope. *)
class var_statement ~(left : ident) ~(right : expression option) =
object
  val left = left
  val right = right

  method interpret ~(ctx : context) =
    let ident_name = left#get_name in
    let right_value =
      match right with
          Some right_expr -> right_expr#get_value ctx#get_cur_scope
        | None -> Tnull
    in
      ctx#get_cur_scope#define ident_name right_value

  method dump =
    match right with
        Some right_expr -> sprintf "[var %s = %s]" left#dump right_expr#dump
      | None -> sprintf "[var %s]" left#dump
    
end ;;

(* A statement that interprets an included template. *)
class include_statement ~(pos : source_pos) ~(template_name : expression) =
object
  method interpret ~(ctx : context) =
    let included_tmpl = ctx#get_template (template_name#to_string ctx#get_cur_scope) in
      try
        included_tmpl#merge ctx#get_cur_scope#get_model ctx#get_buf
      with Template_error msg ->
        let msg_with_include_pos = template_error_message pos msg in
          raise (Template_error msg_with_include_pos)
            
  method dump =
    "[include]"
end ;;

(* A statement that opens a module. *)
class open_statement ~(pos : source_pos) ~(template_name : expression) =
object
  method interpret ~(ctx : context) =
    let template_name_str = template_name#to_string ctx#get_cur_scope in
      
      (* Make sure the template is loaded and up to date. *)
      ignore (ctx#get_template template_name_str);
      
      (* Add its name to the list of opened modules. *)
      if not (List.mem template_name_str ctx#get_opened_modules) then
        ctx#add_opened_module template_name_str
            
  method dump =
    "[open]"
end ;;

(* Macro-related statements. *)

(* Searches for a macro in a list of templates. *)
let rec find_macro get_template template_names macro_name =
  match template_names with
      [] -> raise Not_found
    | template_name :: tail ->
        try
          let tmpl = get_template template_name in
            tmpl#get_macro macro_name
        with Not_found ->
          find_macro get_template tail macro_name ;;

(* A statement that calls a macro. *)
class macro_call_statement ~(pos : source_pos) ~(macro_name : string) ~(args : expression list) =
object
  val pos = pos
  val macro_name = macro_name
  val args = args

  method interpret ~(ctx : context) =
    let mac =
      try
        find_macro ctx#get_template ctx#get_opened_modules macro_name
      with Not_found ->
        let detail = sprintf "Macro %s not found" macro_name in
        let msg = template_error_message pos detail in
          raise (Template_error msg)
    in let arg_values =
        List.map (fun arg -> arg#get_value ctx#get_cur_scope) args
    in
      try
        mac#call arg_values ctx#make_macro_context
      with Template_error msg ->
        let msg_with_call_pos = template_error_message pos msg in
          raise (Template_error msg_with_call_pos)
            
  method dump =
    sprintf "[call %s]" macro_name
end ;;

(* A statement that does nothing.  (Replaces a macro in the template
   where it was defined.) *)
class null_statement =
object
  method interpret ~(ctx : context) = ()

  method dump =
    "[null]"
end ;;
