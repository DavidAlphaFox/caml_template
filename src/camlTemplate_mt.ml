
open CtUtil ;;

(* In multi-threading mode, the variable create_lock_unlock_pair is
   initialized right at startup with a reasonable mutex creator, and
   static_mutex is initialised with a real mutex.  In non-mt mode, the
   default value of these variables contain a dummy creator that does
   not lock anything. *)

let _ = 
  CtUtil.create_lock_unlock :=
    (fun () -> 
       let mtx = Mutex.create () in
       { lock = (fun () -> Mutex.lock mtx);
         unlock = (fun () -> Mutex.unlock mtx) } );
  CtUtil.static_mutex := !CtUtil.create_lock_unlock () ;;
