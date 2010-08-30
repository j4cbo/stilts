(* functor HTTPHandlerFn 
 *
 * Generalized HTTP request handling. This functor is designed to be
 * suitable for both Chiral and non-Chiral environments: it is functorized
 * over the socket libraries as well as a bool indicating whether it is
 * permitted to handle multiple keep-alive requests in a connection.
 *)
functor HTTPHandlerFn (
  structure S : SOCKET
  val can_keep_alive : bool
  val server_name : string
) : sig
  val serve_conn:
    string * Web.hostport * Web.header list
    -> Web.app
    -> (INetSock.inet, S.active S.stream) S.sock * INetSock.sock_addr
    -> unit -> unit

  val addCleanupCallback : (unit -> unit) -> unit
end =
struct

  structure LR = LineReader(S)
  structure W8V = Word8Vector

  val max_request_size = 16384

  exception BadRequest
  exception QuietBailout

  (* Cleanup callbacks to be executed at the end of every request.
  *)
  val callbacks : (unit -> unit) list ref = ref nil
  fun addCleanupCallback f = callbacks := (f :: !callbacks)

  (* Helpers for string splitting.
   *)
  fun == (c: char) c' = c = c'
  fun != (c: char) c' = c <> c'

  (* Split an HTTP header line ("key: value") into key and value.
   *)
  fun split_header line = let
        val (sk, sv) = Substring.splitl (!= #":") (Substring.full line)

        fun dropf #":" = true
          | dropf c = Char.isSpace c

        val v = Substring.dropl dropf sv
      in
        (Substring.string sk, Substring.string v)
      end

  (* Read request headers until there aren't any more.
   *)
  fun read_headers reader = let
        fun read_headers (acc, bytes) =
            if bytes > max_request_size
            then raise BadRequest
            else (
              case Byte.bytesToString (LR.readline reader) of
                "" => rev acc
              | line => read_headers (split_header line :: acc,
                                       size line + bytes)
            )
      in
        read_headers (nil, 0)
      end

  (* Utility function to send the complete contents of an array; based on
   * the SML/NJ distribution.
   *)
  fun send sock vec = let
        val len = W8V.length vec
        fun write i = S.sendVec (sock,
                                 Word8VectorSlice.slice (vec, i, NONE))
        fun put i = if i < len
                    then put (i + write i)
                    else ()
      in
        put 0
      end

  (* If we don't get a well-formed request, we have to send something back.
   * This provides a last-resort fallback. 
   *)
  val bad_request_msg = "400 Bad Request\r\nContent-type: text/plain\r\n"
                      ^ "Server: " ^ server_name ^ "\r\n\r\n"
                      ^ "400 Bad Request"

  fun bad_request conn = (
        ignore (send conn (Byte.stringToBytes bad_request_msg));
        S.close conn
      )

  (* Serve a connection.
   *
   * This function does the bulk of the work of the server. It is designed
   * to be able to run in its own thread under Chiral; once partially
   * applied with server info, an application, and a connection, it is a
   * thunk suitable to be passed to Thread.new. Alternately, it can be
   * evaluated fully; it wil return once one request has been completed if
   * keep-alive is not enabled.
   *
   * If the connection can be used for additional requests with keep-alive,
   * this function will recurse in order to try another request.
   *)
  fun serve_conn_once serverinfo application (conn, conn_addr) = (let

        val (sname, sbind, shdrs) = serverinfo

        val reader = LR.new (conn, { increment = 2048, stripCR = true })

        (* Get the first line from the request *)
        val reqline = Byte.bytesToString (LR.readline reader)
                      handle SysErr => (print "qb\n"; raise QuietBailout)

        (* Parse out the request: "GET / HTTP/1.0" *)
        val (request_method, url, version, req_can_keep_alive) =
          case String.tokens (== #" ") reqline of 
            [ r, u, v as "HTTP/1.1" ] => (r, u, v, true)
          | [ r, u, v ] => (r, u, v, false)
          | [ r, u ] => (r, u, "HTTP/0.9", false)
          | _ => raise BadRequest

        (* Read the request headers *)
        val headers = read_headers reader

        (* Run through the request headers looking for any we care about *)
        val content_len = ref 0
        val connection = ref (
              if can_keep_alive andalso req_can_keep_alive
              then "keep-alive"
              else "close")

        fun proc_header (k, v) = let
              val k' = String.map (fn #"-" => #"_" | c => Char.toUpper c) k
              val () = case k' of
                  "CONTENT_LENGTH" => (
                    content_len := valOf (Int.fromString v)
                    handle Option => ())
                | "CONNECTION" => connection := v
                | _ => ()
            in
              ("HTTP_" ^ k', v)
            end

        val headers = map proc_header headers
        val content_length = !content_len

        (* Convert client addr to a string and int for the Web.request *)
        val (client_host, client_port) = INetSock.fromAddr conn_addr
        val cbind = (NetHostDB.toString client_host, client_port)

        (* Separate out path and query string. *)
        val (spath, squery) = Substring.splitl (!= #"?")
                                               (Substring.full url)

        val pathsections = case map (WebUtil.urldecode o Substring.string)
                                    (Substring.fields (== #"/") spath)
                           of nil => [ "" ]
                            | ""::sections => sections
                            | sections => sections

        val content_cache = ref (if content_length = 0
                                 then SOME (W8V.fromList nil)
                                 else NONE)

        fun contentreader () =
              case !content_cache of
                SOME c => c
              | NONE => let val c = LR.readbytes reader content_length
                         in content_cache := SOME c; c end

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

        (* Now, prepare a response *)

        val length = Int.toString (W8V.length r_body)

        val server_seen = ref false 
        val conn_seen = ref false 
        val status = ref "200 OK"

        (* If the user offered data that we didn't accept, don't bother with
         * keep-alive; we'd have to read the data, and we don't want to do
         * that! *)
        val () = case !content_cache of
              SOME _ => ()
            | NONE => connection := "close"

        (* If the client asked for a keep-alive connection, but we can't
         * support it, then update the header. *)
        val () = if can_keep_alive then ()
                                   else connection := "close"

        (* Process the headers we get back from the application; strip out
         * headers that have special meaning.
         *   Content-Length: we decide that; it's not the app's business.
         *   Status: turn into an HTTP status code
         *   Server: if they provided a Server:, don't add our own
         *   Connection: let them override.
         *)
        fun proc_hdr (k, v) = (case String.map Char.toUpper k of
               "CONTENT-LENGTH" => NONE
            | "STATUS" => (status := v; NONE)
            | "SERVER" => (server_seen := true; SOME (k, v))
            | "CONNECTION" => (conn_seen := true; SOME (k, v))
            | _ => SOME (k, v))

        val rh_n = ("Content-Length", Int.toString (W8V.length r_body))
                   :: List.mapPartial proc_hdr r_hdrs

        val rh_n = if !server_seen
                   then rh_n
                   else ("Server", server_name) :: rh_n

        val rh_n = if !conn_seen
                   then rh_n
                   else ("Connection", !connection) :: rh_n

        (* Which version do we tell them we're responding with? *)
        val respVersion = case version of "HTTP/1.1" => version
                                        | _ => "HTTP/1.0"

        val response = W8V.concat [
              Byte.stringToBytes (respVersion ^ " " ^ !status ^ "\r\n"),
              CGI.make_response (rh_n, r_body)
            ]

        val () = send conn response

        val () = List.app (fn f => f ()) (!callbacks)

        val () = print ("Final Connection value: " ^ !connection ^ "\n")
      in
        case String.map Char.toUpper (!connection) of
          "KEEP-ALIVE" => true
        | _ => (S.close conn; false)
      end
      handle BadRequest => (bad_request conn; false)
           | QuietBailout => (((S.close conn) handle e => ()); false)
           | e => raise e
    )

  fun serve_conn serverinfo app (conn, conn_addr) () =
    if serve_conn_once serverinfo app (conn, conn_addr)
    then serve_conn serverinfo app (conn, conn_addr) ()
    else ()

end
