signature WEB_UTIL = sig

  (* Debugging *)
  val dumpRequest: Web.request -> string
  val dumpRequestWrapper: (string -> unit) -> Web.app -> Web.app

  (* Path extraction and manipulation *)
  val withPath: Web.pathsec * Web.pathsec -> Web.request -> Web.request
  val prepath: Web.request -> Web.pathsec
  val postpath: Web.request -> Web.pathsec
  val flattenPath: Web.pathsec * Web.pathsec -> string

  (* Other request accessors *)
  val http_header: string -> Web.request -> string option
  val server_header: string -> Web.request -> string option

  (* Exception handling, and some shortcuts for common exceptions *)
  val httpExnCode: Web.http_exn -> string
  val exnWrapper: Web.app -> Web.app
  val notFound: exn
  val redirect: string -> exn
  val redirectPostpath: Web.request -> Web.pathsec -> exn

  (* Wrappers to build a Web.response *)
  val resp: string -> string -> Web.response
  val htmlResp: Web.html -> Web.response
  val xhtmlResp: Web.html -> Web.response

  (* HTML escaping *)
  val escapeStr: string -> string
  val escape: string -> Web.html
  val escapeForJS: string -> Web.html

  (* Dispatching and automatic redirection *)
  datatype dispatchmode = EXACT | PREFIX | SLASH
  val dispatch: (Web.pathsec * dispatchmode * Web.app) list -> Web.app
  val forceSlash: Web.app -> Web.app

  (* URL-encoding *)
  val urlencode: string -> string

end
