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
    ) >>= fun () ->
  Printf.printf "Check: OK!\n%!";
  Lwt.return ()

(* Issue concurrent VDI pool_migrates for each disk *)
let migrate rpc session_id vm sr1 sr2 =
  let pick_dest_sr r =
    if r.API.vDI_SR = sr1 then sr2 else sr1
  in
  VM.get_VBDs ~rpc ~session_id ~self:vm
  >>=
  Lwt_list.map_s (fun self -> VBD.get_VDI ~rpc ~session_id ~self)
  >>=
  Lwt_list.map_s (fun self -> VDI.get_record ~rpc ~session_id ~self >>= fun r -> return (self,r))
  >>=
  Lwt_list.map_p (fun (vdi,vdi_record) ->
      Async.VDI.pool_migrate rpc session_id vdi (pick_dest_sr vdi_record) [])
  >>= fun tasks ->
  Printf.printf "Migrations started\n%!";
  return tasks

let iter rpc session_id vm n sr1 sr2 =
  check rpc session_id vm n >>= fun () ->
  migrate rpc session_id vm sr1 sr2 >>= fun tasks ->
  Lwt_unix.sleep (Random.float 50.0) >>= fun () ->
  Printf.printf "Cancelling tasks\n%!";
  Lwt_list.iter_s (fun task -> Task.cancel ~rpc ~session_id ~task) tasks >>= fun () ->
  Lwt.join (List.map (fun self ->
      let rec wait () =
        Task.get_status ~rpc ~session_id ~self >>= fun s ->
        if s = `pending then Lwt_unix.sleep 1.0 >>= wait
        else return ()
      in wait ()) tasks) >>= fun () ->
  check rpc session_id vm n

let run host uname pwd vm_name sr1_uuid sr2_uuid n ndisks =
  let lwt =
    let rpc = make (Printf.sprintf "http://%s/" host) in
    Session.login_with_password ~rpc ~uname ~pwd ~version:"1.0" ~originator:"sxmcheck" >>= fun session_id ->
    VM.get_by_name_label rpc session_id vm_name >>= fun vms ->
    if List.length vms <> 1 then failwith (Printf.sprintf "Expected 1 VM named '%s'" vm_name);
    let vm = List.hd vms in
    SR.get_by_uuid rpc session_id sr1_uuid >>= fun sr1 ->
    SR.get_by_uuid rpc session_id sr2_uuid >>= fun sr2 ->
    let rec inner n =
      if n=0 then Lwt.return ()
      else iter rpc session_id vm ndisks sr1 sr2 >>= fun () -> inner (n-1)
    in
    inner n
  in
  Lwt_main.run lwt

open Cmdliner

let sr_arg1 =
  let doc = "An SR UUID to migrate to/from" in
  Arg.(required & opt (some string) None & info ["sr1"] ~doc)

let sr_arg2 =
  let doc = "An SR UUID to migrate to/from" in
  Arg.(required & opt (some string) None & info ["sr2"] ~doc)

let vm_name =
  let doc = "The name of the VM to migrate" in
  Arg.(required & opt (some string) None & info ["vm"] ~doc)

let uname =
  let doc = "Username" in
  Arg.(required & opt (some string) None & info ["u"; "username"] ~doc)

let pwd =
  let doc = "Password" in
  Arg.(required & opt (some string) None & info ["p";"password"] ~doc)

let host =
  let doc = "XenServer hostname" in
  Arg.(required & opt (some string) None & info ["s";"server"] ~doc)

let n =
  let doc = "Number of iterations to attempt" in
  Arg.(required & opt (some int) None & info ["n"] ~doc)

let ndisks =
  let doc = "Number of disks to expect on the VM" in
  Arg.(required & opt (some int) None & info ["ndisks"] ~doc)

let run_t =
  let doc = "Run a number of VDI.pool_migrate operations, cancelling
them after a short timeout. Runs a check in between iterations to verify
that no disks have been destroyed" in
  Term.(const run $ host $ uname $ pwd $ vm_name $ sr_arg1 $ sr_arg2 $ n $ ndisks),
  Term.info "sxmcheck" ~version:"1.0.0" ~doc

let () =
  match Term.eval run_t with `Error _ -> exit 1 | _ -> exit 0
