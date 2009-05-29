structure HTTPServer :> WEB_SERVER where type opts = INetSock.sock_addr = struct

  exception BadRequest

  structure LR = LineReader(Socket)

  val server_name = "Stilts-HTTPd/0.1"

  val callbacks : (unit -> unit) list ref = ref nil

  fun addCleanupCallback f = callbacks := (f :: !callbacks)

  val bad_request_msg = Byte.stringToBytes (
                          "400 Bad Request\r\nContent-type: text/plain\r\n"
                        ^ "Server: " ^ server_name ^ "\r\n\r\n"
                        ^ "400 Bad Request")

  fun bad_request conn = (
        ignore (SockUtil.sendVec (conn, bad_request_msg));
        Socket.close conn
      )

  fun serveConn (sname, sbind, shdrs) application (conn, conn_addr) = (let

        val reader = LR.new (conn, { increment = 8192, stripCR = true })

        val reqline = String.tokens (fn c => c = #" ")
                                    (Byte.bytesToString (LR.readline reader))

        val (request_method, url, version) = case reqline of
                                               (r::u::v::nil) => (r, u, v)
                                             | _ => raise BadRequest

        fun read_headers acc = (
              case Byte.bytesToString (LR.readline reader) of
                "" => acc
              | line => let
                          val (sk, sv) = Substring.splitl (fn c => c <> #":")
                                                          (Substring.full line)
                          fun dropf #":" = true
                            | dropf c = Char.isSpace c
                          val v = Substring.dropl dropf sv
                        in
                          read_headers ((Substring.string sk,
                                         Substring.string v) :: acc)
                        end)

        val headers = rev (read_headers nil)

        val content_len = ref 0

        fun i_or_zero s = case Int.fromString s of SOME i => i | NONE => 0

        fun proc_header (k, v) = let
              val k' = String.map (Char.toUpper o (fn #"-" => #"_" | c => c)) k
              val () = case k' of "CONTENT_LENGTH" => content_len := i_or_zero v
                                | _ => ()
            in
              ("HTTP_" ^ k', v)
            end

        val headers = map proc_header headers
        val content_length = !content_len

        (* Convert client addr to the string and int that Web.request wants *)
        val (client_host, client_port) = INetSock.fromAddr conn_addr
        val cbind = (NetHostDB.toString client_host, client_port)

        (* Separate out path and query string. *)
        val (spath, squery) = Substring.splitl (fn c => c <> #"?")
                                               (Substring.full url)

        val pathsections = case map (Form.unquote o Substring.string)
                                    (Substring.fields (fn c => c = #"/") spath)
                           of nil => [ "" ]
                            | ""::sections => sections
                            | sections => sections

      val content_cache : Word8Vector.vector option ref = ref NONE

      fun contentreader () =
            case !content_cache of
              SOME c => c
            | NONE => let
                        val c = LR.readbytes reader content_length
                      in
                        content_cache := SOME c;
                        c
                      end

        val squery = if Substring.isEmpty squery
                     then squery 
                     else Substring.slice (squery, 1, NONE)

        (* Build the request *)
        val req : Web.request = { client = cbind,
                                  method = request_method,
                                  path = (nil, pathsections),
                                  query_string = Substring.string squery,
                                  http_headers = headers,
                                  content_length = content_length,
                                  content = contentreader,
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

        val () = SockUtil.sendVec (conn, response)
        val () = Socket.close conn

        val () = List.app (fn f => f ()) (!callbacks)
      in
        ()
      end
      handle BadRequest => bad_request conn)


  type opts = INetSock.sock_addr

  fun serve addr application =
    let
      val listener = INetSock.TCP.socket ()

      val (server_host, server_port) = INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      fun accept () = ((
        serveConn (server_name, sbind, nil)
                  application
                  (Socket.accept listener);
        accept ()
      ) handle _ => accept ())
    in
      (
        Socket.Ctl.setREUSEADDR (listener, true);
        Socket.bind (listener, addr);
        Socket.listen (listener, 9);
        accept ()
      ) handle x => (Socket.close listener; raise x)
    end

end
