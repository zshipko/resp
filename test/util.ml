module Make (Server : Resp_server.S) = struct
  open Server.IO

  let commands =
    [ ( "set"
      , fun ht client _cmd nargs ->
          if nargs <> 2 then Server.invalid_arguments client
          else
            Server.recv client
            >>= fun key ->
            Server.recv client
            >>= fun value ->
            Hashtbl.replace ht (Resp.to_value_exn key)
              (Resp.to_value_exn value);
            Server.ok client )
    ; ( "get"
      , fun ht client _cmd nargs ->
          if nargs <> 1 then Server.invalid_arguments client
          else
            Server.recv client
            >>= fun key ->
            try
              let value = Hashtbl.find ht (Resp.to_value_exn key) in
              Server.send client (`Bulk (`Value value))
            with Not_found -> Server.error client "Not found" ) ]
end
