structure WebUtil :> WEB_UTIL = struct

  (* val dumpRequest: Web.request -> string
   *
   * Return a string with all available request information. This produces
   * many lines of output; it is suitable for debugging, not general logging.
   *)
  fun dumpRequest (req: Web.request) = 
    let
      val (remote_addr, remote_port) = #client req
      val (server_addr, server_port) = #server_bind req
      val (prepath, postpath) = #path req
      val prepath = map (fn s => "\"" ^ s ^ "\"") prepath
      val postpath = map (fn s => "\"" ^ s ^ "\"") postpath
      fun formatHeader (k, v) = String.concat [ "    - ", k, ": ", v, "\n" ]
    in
      String.concat [
        "Request dump:\n  Remote address: ", remote_addr, ":",
        Int.toString remote_port, "\n  Request: ", #method req, " ",
        String.concatWith "/" prepath, " / ", String.concatWith "/" postpath,
        "\n  Query string: ", #query_string req, "\n  HTTP headers: \n",
        String.concat (map formatHeader (#http_headers req)),
        "  Request content: ", Int.toString (#content_length req), " bytes\n",
        "  Server: \"", #server_name req, "\" at ", server_addr,
          ":", Int.toString server_port, ", docroot ", #doc_root req, "\n",
        "  Server headers:\n",
        String.concat (map formatHeader (#server_headers req)),
        "Running application...\n"
      ]
    end


  (* val dumpRequestWrapper: (string -> unit) -> Web.app -> Web.app
   *
   * Apply the given printing function (simply "print" for stdout) to each
   * request handled by the given app.
   *)
  fun dumpRequestWrapper printer app req = (printer (dumpRequest req); app req)


  (* val withPath: Web.pathsec * Web.pathsec -> Web.request -> Web.request
   *
   * Replace a request's path sections with the given values.
   *)
  fun withPath newPath (req as {
                                 client, method, path, query_string,
                                 http_headers, content_length, content,
                                 doc_root, server_name, server_bind,
                                 server_headers
                               } : Web.request) = { 
        client = client, method = method, path = newPath,
        query_string = query_string, http_headers = http_headers,
        content_length = content_length, content = content,
        doc_root = doc_root, server_name = server_name,
        server_bind = server_bind, server_headers = server_headers
      }


  (* val prepath: Web.request -> Web.pathsec
   * val postpath: Web.request -> Web.pathsec
   *
   * Return the prepath or postpath for the given request.
   *)
  fun prepath (req: Web.request) = #1 (#path req)
  fun postpath (req: Web.request) = #2 (#path req)


  (* val flattenPath: Web.pathsec * Web.pathsec -> string
   *
   * Flatten a path in (prepath, postpath) form to an abolute path string.
   *)
  fun flattenPath (pre, post) = "/" ^ (String.concatWith "/" (pre @ post))


  (* val http_header: string -> Web.request -> string
   * val server_header: string -> Web.request -> string
   *
   * Find the first HTTP or server header in the request of the given name.
   *)
  fun http_header hdr (req: Web.request) =
        case List.find (fn (k, v) => k = hdr) (#http_headers req) of
               SOME (k, v) => SOME v | NONE => NONE

  fun server_header hdr (req: Web.request) =
        case List.find (fn (k, v) => k = hdr) (#server_headers req) of
               SOME (k, v) => SOME v | NONE => NONE


  (* val httpExnCode: Web.http_exn -> string
   *
   * Return the correct code and string for the given HTTP exception.
   *)
  val httpExnCode = let open Web in fn
        HTTP300MultipleChoices        => "300 Multiple Choices"
      | HTTP301Moved _                => "301 Moved"
      | HTTP302Found _                => "302 Found"
      | HTTP303SeeOther _             => "303 See Other"
      | HTTP304NotModified            => "304 Not Modified"
      | HTTP305UseProxy _             => "305 Use Proxy"
      | HTTP307TemporaryRedirect _    => "307 Temporary Redirect"
      | HTTP400BadRequest             => "400 Bad Request"
      | HTTP401Unauthorized           => "401 Unauthorized"
      | HTTP403Forbidden              => "403 Forbidden"
      | HTTP404NotFound               => "404 Not Found"
      | HTTP405MethodNotAllowed       => "405 Method Not Allowed"
      | HTTP406NotAcceptable          => "406 Not Acceptable"
      | HTTP407ProxAuthReq            => "407 Proxy Authentication Required"
      | HTTP408RequestTimeout         => "408 Request Timeout"
      | HTTP409Conflict               => "409 Conflict"
      | HTTP410Gone                   => "410 Gone"
      | HTTP411LengthRequired         => "411 Length Required"
      | HTTP412PreconditionFailed     => "412 Precondition Failed"
      | HTTP413ReqEntityTooLarge      => "413 Request Entity Too Large"
      | HTTP414ReqURITooLong          => "414 Request-URI Too Long"
      | HTTP415UnsuppMediaType        => "415 Unsupported Media Type"
      | HTTP416RangeNotSatisfiable    => "416 Requested Range Not Satisfiable"
      | HTTP417ExpectationFailed      => "417 Expectation Failed"
      | HTTP500InternalServerError _  => "500 Internal Server Error"
      | HTTP501NotImplemented         => "501 Not Implemented"
      | HTTP502BadGateway             => "502 Bad Gateway"
      | HTTP503ServiceUnavailable     => "503 Service Unavailable"
      | HTTP504GatewayTimeout         => "504 Gateway Timeout"
      | HTTP505VersionNotSupported    => "505 HTTP Version Not Supported" end


  (* val httpExnHandler: http_exn -> Web.response
   *
   * Generate a suitable response for the given exception.
   *)
  val httpExnHandler = let
      fun redirContent (name, target) = ( [ ("Status", httpExnCode name),
                                            ("Content-type", "text/plain"),
                                            ("Location", target) ],
                                          Byte.stringToBytes (
                                            httpExnCode name ^ ": " ^ target) )
    in
      fn e as (Web.HTTP301Moved t) => redirContent (e, t)
       | e as (Web.HTTP302Found t) => redirContent (e, t)
       | e as (Web.HTTP303SeeOther t) => redirContent (e, t)
       | e as (Web.HTTP305UseProxy t) => redirContent (e, t)
       | e as (Web.HTTP307TemporaryRedirect t) => redirContent (e, t)
       | e as (Web.HTTP500InternalServerError t) => (
            [ ("Status", "500 Internal Server Error"),
              ("Content-type", "text/plain") ],
            Byte.stringToBytes ("Internal Server Error: " ^ t) )
        | e => (
            [ ("Status", httpExnCode e), ("Content-type", "text/plain") ],
            Byte.stringToBytes (httpExnCode e))
    end


  (* val exnWrapper: Web.app -> Web.app
   *
   * Run the application; if an exception is raised, handle / display it with
   * httpExnHandler.
   *)
  fun exnWrapper app req = (app req) handle e => (case e of
        (Web.HTTPExn e') => httpExnHandler e'
      | e => ( [ ("Status", "500 Internal Server Error"),
                 ("Content-type", "text/plain") ],
               Byte.stringToBytes (
                 "Internal Server Error: " ^ (General.exnMessage e))))


  (* val notFound: exn
   * val redirect: string -> exn
   *
   * Helpers for commonly-used exceptions. The redirect function produces an
   * HTTP 301 Moved.
   *)
  val notFound = Web.HTTPExn Web.HTTP404NotFound
  fun redirect addr = Web.HTTPExn (Web.HTTP301Moved addr)


  (* val redirectPostpath: Web.request -> Web.pathsec -> exn
   *
   * Redirect to a path relative to the given request's prepath.
   *
   * For example: if (#path req) is ( ["my", "app"], ["page"] ), then
   * redirectPostpath req ["otherpage"] will redirect to /my/app/otherpage.
   *)
  fun redirectPostpath req p = redirect (flattenPath (prepath req, p))


  (* val resp: string -> string -> Web.response
   * val htmlResp: Web.html -> Web.response
   *
   * Helpers to build a Web.response. "resp" takes a Content-type followed by
   * output data.
   *)
  fun resp contentType data = (
        [ ("Status", "200 OK"), ("Content-type", contentType) ],
        Byte.stringToBytes data
      )
  fun htmlResp (Web.HTML data) = resp "text/html;charset=utf-8" data
  fun xhtmlResp (Web.HTML data) = resp "application/xhtml+xml;charset=utf-8" data


  (* val escapeStr: string -> string
   * val escape: string -> Web.html
   *
   * Perform HTML / XML escaping on the input string.
   *)
  fun escapeNormalChar c = if Char.< (c, #" ")
                           then "&#" ^ Int.toString (Char.ord c) ^ ";"
                           else String.str c

  val escapeStr = String.translate (fn #"<" => "&lt;"
                                     | #"&" => "&amp;"
                                     | #"\"" => "&quot;"
                                     | c => escapeNormalChar c)
  val escape = Web.HTML o escapeStr
  val escapeForJS = Web.HTML o String.translate (fn #"<" => "&lt;"
                                                  | #"&" => "&amp;"
                                                  | c => escapeNormalChar c)


  (* fun dispatch: (Web.pathseq * dispatchmode * Web.app) list -> Web.app
   *
   * General-purpose directory dispatcher.
   *
   * The dispatcher app takes a list of tuples specifying a path segment,
   * match mode, and target app. For each item, it checks whether the given
   * segment is a prefix of the incoming request's postpath. If so, the match
   * mode provides further criteria:
   * - EXACT: the match path and request's postpath must be identical
   * - PREFIX: there may be additional components in the request's postpath
   * - SLASH: the request's postpath must contain a single "" (trailing slash)
   *          after the match path. If not present, a redirect will be raised.
   *          Any other trailing components will result in the match failing.
   *
   * If no item in the list matches, the dispatcher will raise a 404.
   *
   * Once a match is found, the target app is invoked with a rewritten path.
   * The portions of the path matched by the entry in the list are removed from
   * prepath and concatenated to the end of postpath. Note that this means that
   * an EXACT entry will always be invoked with a nil postpath; a SLASH entry
   * will always be invoked with a postpath of [""].
   *
   * The forceSlash function may be useful for PREFIX matches.
   *)
  datatype dispatchmode = EXACT | PREFIX | SLASH
  fun dispatch nil req = raise (Web.HTTPExn Web.HTTP404NotFound)
    | dispatch ((matchpath, matchmode, app)::rest) req = let
        fun splitPrefix (nil: string list, p: string list) = SOME p
          | splitPrefix (_, nil) = NONE
          | splitPrefix ((h1::t1),(h2::t2)) = if h1=h2 then splitPrefix (t1,t2)
                                                       else NONE
      in
        case splitPrefix (matchpath, postpath req) of
          NONE => dispatch rest req
        | SOME postpath' => let
            val req' = withPath (prepath req @ matchpath, postpath') req
          in
            case (matchmode, postpath') of
              (EXACT, nil) => app req'
            | (EXACT, _) => dispatch rest req
            | (PREFIX, _) => app req'
            | (SLASH, [""]) => app req'
            | (SLASH, nil) => raise (redirect (flattenPath (#path req) ^ "/"))
            | (SLASH, _) => dispatch rest req 
          end
      end


  (* val forceSlash: Web.app -> Web.app
   *
   * Ensure that if postpath is nil, a redirect will be raised to add the
   * missing slash. Thus, if /my/app is invoked with (prepath, postpath) =
   * (["my", "app"], nil), the redirect will be to /my/app/, which should
   * correspond to (["my", "app"], [""]).
   *)
  fun forceSlash app (req: Web.request) = case postpath req of
        nil => raise (redirect (flattenPath (#path req) ^ "/"))
      | _ => (app req)


  (* val urlencode: string -> string
   *
   * Perform URL encoding.
   *)
  val urlencode = String.translate (
        fn c => if Char.isAlphaNum c
                then String.str c
                else "%" ^ StringCvt.padLeft #"0" 2
                           (Int.fmt StringCvt.HEX (Char.ord c))
      )

end
