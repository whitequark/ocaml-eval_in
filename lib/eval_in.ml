open Lwt
open ExtString

let evaluate ~lang ~code =
  (* Common eval.in parameters *)
  let base_uri = Uri.of_string "https://eval.in/" in
  let headers  = Cohttp.Header.init () in
  let headers  = Cohttp.Header.add headers "user-agent" "ocaml-eval_in" in
  (* Request for evaluation *)
  let params   = Cohttp.Header.init () in
  let params   = Cohttp.Header.add params "lang"       lang in
  let params   = Cohttp.Header.add params "code"       code in
  let params   = Cohttp.Header.add params "execute"    "1" in
  Cohttp_lwt_unix.Client.post_form ~params ~headers base_uri >>= fun (resp, _) ->
  assert ((Cohttp.Response.status resp) = `Found);
  (* Request for JSON with info and output *)
  let headers = Cohttp.Response.headers resp in
  let loc     = Option.get (Cohttp.Header.get headers "location") in
  let uri     = Uri.of_string loc in
  let uri     = Uri.with_path base_uri ((Uri.path uri) ^ ".json") in
  Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
  assert ((Cohttp.Response.status resp) = `OK);
  Cohttp_lwt_body.to_string body >>= fun body ->
  (* Format response *)
  let json    = Yojson.Basic.from_string body in
  let output  = Yojson.Basic.Util.(json |> member "output" |> to_string) in
  let result  = Re.(get (exec (Re_perl.compile_pat "^(File \"(.+?)\", )?(.{0,200})") output) 3) in
  if String.length result = 200 then
    return (Printf.sprintf "%s... (%s)" result loc)
  else
    return (Printf.sprintf "%s (%s)" result loc)

let callback ~lang ~connection ~result =
  let open Irc_message in
  match result with
  | Message {prefix=Some sender; command="PRIVMSG";
             params=[channel]; trail=Some text; } ->
    if String.starts_with text ">> " then
      let nick, _ = String.split sender "!" in
      let code    = String.strip (String.slice ~first:2 text) in
      catch
        (fun () ->
          evaluate ~lang ~code >>= fun result ->
          Irc_client_lwt.Client.send_privmsg ~connection ~target:channel
                                             ~message:(nick ^ ":`" ^ result))
        (fun exn ->
          Lwt_io.eprintl (Printexc.to_string exn) >>= fun () ->
          Lwt_io.eprintl (Printexc.get_backtrace ()) >>= fun () ->
          Irc_client_lwt.Client.send_privmsg ~connection ~target:channel
                                             ~message:(nick ^ ":`sorry,`I`broke`-_-'"))
    else
      return_unit
  | _ ->
    return_unit

let main server port realname username password nick channels lang =
  Lwt_main.run (
    Irc_client_lwt.Client.connect_by_name ~server ~port ~username ~mode:0
                           ~realname ~nick ?password () >>=
    fun result ->
    match result with
    | None -> Lwt_io.eprintlf "Cannot connect to %s" server
    | Some connection ->
      channels |> Lwt_list.iter_p (fun channel ->
        Irc_client_lwt.Client.send_join ~connection ~channel) >>= fun () ->
      Irc_client_lwt.Client.listen ~connection ~callback:(callback ~lang) >>= fun () ->
      Irc_client_lwt.Client.send_quit ~connection)

open Cmdliner

let server =
  Arg.(required & opt (some string) None & info ["h"; "server"] ~docv:"HOST"
          ~doc:"Server hostname")

let port =
  Arg.(value & opt int 6667 & info ["p"; "port"] ~docv:"PORT"
          ~doc:"Server port")

let realname =
  Arg.(value & opt string "ocaml-eval_in" & info ["realname"] ~docv:"REALNAME"
          ~doc:"Real name")

let username =
  Arg.(required & opt (some string) None & info ["U"; "username"] ~docv:"USERNAME"
          ~doc:"Nickserv username")

let password =
  Arg.(value & opt (some string) None & info ["P"; "password"] ~docv:"PASSWORD"
          ~doc:"Nickserv password")

let nickname =
  Arg.(required & opt (some string) None & info ["n"; "nickname"] ~docv:"NICK"
          ~doc:"Nickname")

let channels =
  Arg.(value & opt_all string [] & info ["c"; "channel"] ~docv:"CHANNEL"
          ~doc:"Channel to join")

let language =
  Arg.(value & opt string "ocaml/ocaml-4.00.1" & info ["l"; "language"] ~docv:"LANG"
          ~doc:"Language to evaluate in")

let eval_in_t = Term.(pure main $ server $ port $ realname $ username
                                $ password $ nickname $ channels $ language)

let info =
  let doc = "execute code snippets from IRC on http://eval.in" in
  let man = [
    `S "BUGS";
    `P "Email bug reports to Peter Zotov <whitequark@whitequark.org>.";
  ] in
  Term.info "eval_in" ~version:"1.0" ~doc ~man

let () = match Term.eval (eval_in_t, info) with `Error _ -> exit 1 | _ -> exit 0
