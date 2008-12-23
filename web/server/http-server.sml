structure HTTPServer :> WEB_SERVER where type opts = INetSock.sock_addr = struct

  exception BadRequest

  fun readline sock = let
        fun getc () = case SockUtil.recvStr (sock, 1) of
                        "" => NONE
                      | s => SOME (String.sub (s, 0))

        fun readline' acc = case getc () of
                              NONE => acc
                            | SOME #"\n" => (case acc of #"\r"::rest => rest
                                                      | _ => acc) 
                            | SOME c => readline' (c::acc)
      in
        String.implode (rev (readline' nil))
      end

  val bad_request_msg = Byte.stringToBytes (
                          "400 Bad Request\r\nContent-type: text/plain\r\n\r\n"
                        ^ "400 Bad Request")

  fun bad_request conn = (
        ignore (SockUtil.sendVec (conn, bad_request_msg));
        Socket.close conn
      )

  val server_name = "Stilts-HTTPd/0.1"

  fun serveConn (sname, sbind, shdrs) application (conn, conn_addr) = let

        val reqline = String.tokens (fn c => c = #" ") (readline conn)

        val (request_method, url, version) = case reqline of
                                               (r::u::v::nil) => (r, u, v)
                                             | _ => raise BadRequest

        fun read_headers acc = case readline conn of
                                      "" => acc
                                    | line => read_headers (line::acc)

        val headers = rev (read_headers nil)

        (* Convert client addr to the string and int that Web.request wants *)
        val (client_host, client_port) = INetSock.fromAddr conn_addr
        val cbind = (NetHostDB.toString client_host, client_port)

        (* Separate out path and query string. *)
        val (spath, squery) = Substring.splitl (fn c => c <> #"?")
                                               (Substring.full url)

        val pathsections = case map (Form.unquote o Substring.string)
                                    (Substring.tokens (fn c => c = #"/") spath)
                           of nil => [ "" ] | sections => sections

        (* Build the request *)
        val req : Web.request = { client = cbind,
                                  method = request_method,
                                  path = (nil, pathsections),
                                  query_string = Substring.string squery,
                                  http_headers = nil,
                                  content_length = 0,
                                  content = (fn () => raise Fail "no content"),
                                  doc_root = "",
                                  server_name = sname,
                                  server_bind = sbind,
                                  server_headers = shdrs }

        val (r_hdrs, r_body) = application req

        val length = Int.toString (Word8Vector.length r_body)

        val server_seen = ref false 
        val status = ref "200 OK"

        fun proc_hdr (k, v) = (case String.map Char.toUpper k of
                                 "CONTENT-LENGTH" => NONE
                               | "STATUS" => (status := v; NONE)
                               | "SERVER" => (server_seen := true; SOME (k, v))
                               | _ => SOME (k, v))

        val rh_n = ("Content-Length", Int.toString (Word8Vector.length r_body))
                   :: List.mapPartial proc_hdr r_hdrs

        val rh_n = case !server_seen of false => ("Server", server_name) :: rh_n
                                      | true => rh_n 

        val response = Word8Vector.concat [
                        Byte.stringToBytes ("HTTP/1.0 " ^ (!status) ^ "\r\n"),
                        CGI.make_response (rh_n, r_body)
                      ]
      in
        ignore (SockUtil.sendVec (conn, response));
        Socket.close conn
      end

(*
      val content_cache : Word8Vector.vector option ref = ref NONE

      fun reader () =
            case !content_cache of
              SOME c => c
            | NONE => let
                        val c = SockUtil.recvVec (conn, content_length)
                      in
                        content_cache := SOME c;
                        c
                      end
*)

  type opts = INetSock.sock_addr

  fun serve addr application =
    let
      val listener = INetSock.TCP.socket ()

      val (server_host, server_port) = INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      fun accept () = (
        serveConn (server_name, sbind, nil)
                  application
                  (Socket.accept listener);
        accept ()
      )
    in
      (
        Socket.Ctl.setREUSEADDR (listener, true);
        Socket.bind (listener, addr);
        Socket.listen (listener, 9);
        accept ()
      ) handle x => (Socket.close listener; raise x)
    end

end
