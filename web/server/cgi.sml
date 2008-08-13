structure CGI = struct

  exception ProtocolError

  (* val make_request: (string * string) list * int * (unit -> string)
   *                                                             -> Web.request
   *
   * Assemble CGI headers and a body reader function into a Web.request record.
   *)
  fun make_request (headers, content_length, content_reader) : Web.request =
    let
      (* Loop through all the headers and sort them out: HTTP, server, or
       * specially-handled. *)

      val remote_addr = ref ""
      val remote_port = ref ""
      val request_method = ref ""
      val script_name = ref ""
      val path_info = ref ""
      val query_string = ref ""
      val document_root = ref ""
      val server_addr = ref ""
      val server_name = ref ""
      val server_port = ref ""

      val process_key = (fn
            (("REMOTE_ADDR", v),    acc) => (remote_addr := v; acc)
          | (("REMOTE_PORT", v),    acc) => (remote_port := v; acc)
          | (("REQUEST_METHOD", v), acc) => (request_method := v; acc)
          | (("SCRIPT_NAME", v),    acc) => (script_name := v; acc)
          | (("PATH_INFO", v),      acc) => (path_info := v; acc)
          | (("QUERY_STRING", v),   acc) => (query_string := v; acc)
          | (("DOCUMENT_ROOT", v),  acc) => (document_root := v; acc)
          | (("SERVER_ADDR", v),    acc) => (server_addr := v; acc)
          | (("SERVER_NAME", v),    acc) => (server_name := v; acc)
          | (("SERVER_PORT", v),    acc) => (server_port := v; acc)
          | ((k, v), (http_headers, other_headers)) =>
            if String.isPrefix "HTTP_" k
            then (((k, v)::http_headers), other_headers)
            else (http_headers, ((k, v)::other_headers))
          )

      val (http_headers, server_headers) = foldl process_key (nil, nil) headers

      (* Parse out client and server port numbers *)
      val client = case Int.fromString (!remote_port) of
                     SOME i => (!remote_addr, i)
                   | NONE => raise ProtocolError
                   handle Overflow => raise ProtocolError

      val server = case Int.fromString (!server_port) of
                     SOME i => (!server_addr, i)
                   | NONE => raise ProtocolError
                   handle Overflow => raise ProtocolError

      (* Split the path, dropping leading / if necessary *)
      val splitSlash = String.fields (fn c => c = #"/")
      val pre = case splitSlash (!script_name) of (""::p) => p | p => p
      val post = case splitSlash (!path_info) of (""::p) => p | p => p

      (* If no postpath is provided, the prepath should probably be there... *)
      val (pre, post) = case (pre, post) of (x, nil) => (nil, x) | x => x
    in
      {
        client = client, method = !request_method, path = (pre, post),
        query_string = !query_string, content_length = content_length,
        content = content_reader, doc_root = !document_root,
        server_name = !server_name, server_bind = server,
        http_headers = http_headers, server_headers = server_headers
      }
    end


  (* val make_response: Web.header * string -> string
   *
   * Concatenate together output headers and content into a CGI-style response.
   *)
  fun make_response (headers, body) = let
        fun headerLine (k, v) = k ^ ": " ^ v ^ "\r\n"
      in
        Word8Vector.concat [
          Byte.stringToBytes (String.concat (map headerLine headers)),
          Byte.stringToBytes "\r\n",
          body ]
      end

end
