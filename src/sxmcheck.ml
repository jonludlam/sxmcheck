(* Test SXM VDI migrate *)

open Lwt
open Xen_api
open Xen_api_lwt_unix

(* Check that the VM has n valid VDIs *)
let check rpc session_id vm n =
  VM.get_VBDs rpc session_id vm >>=
  Lwt_list.iter_s (fun vbd ->
      VBD.get_VDI rpc session_id vbd >>= fun vdi ->
      VDI.get_uuid rpc session_id vdi >>= fun _ ->
      return ()
  )

let migrate rpc session_id vm vdi sr1 sr2 =
  Async.VDI.pool_migrate rpc session_id vdi >>= fun task ->
  ()
 
