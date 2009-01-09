structure Web = struct

  type header = string * string
  type hostport = string * int
  type pathsec = string list

  type request = {
                    (* Client connection *)
                    client: hostport,

                    (* GET, POST, etc. *)
                    method: string,

                    (* The path is divied into two parts: the prepath is
                       the portion of the URL hierarchy leading up to the
                       application's root, and the postpath is everything
                       after. Paths with a trailing slash are represented with
                       an empty string at the end of postpath.

                       Invariants:
                       - An empty path component may only occur as the last
                         item in postpath; never elsewhere in postpath or
                         anywhere in prepath.
                       - Either prepath or postpath may be nil, but not both.
                         The absolute root must therefore be represented as:
                           (nil, [""])
                    *)       
                    path: pathsec * pathsec,
                    query_string: string,

                    (* HTTP headers are passed as in CGI/SCGI: upper case,
                       with an HTTP_ prefix. The order is not specified.
                    *)
                    http_headers: header list,

                    (* Request content is read on demand, to allow applications
                       to reject excessive amounts of data. The interface does
                       not yet allow incremental reading of data.
                    *)
                    content_length: int, 
                    content: unit -> Word8Vector.vector,

                    (* Server *)
                    doc_root: string,
                    server_name: string,
                    server_bind: hostport,
                    server_headers: header list
  }

  type response = header list * Word8Vector.vector

  type app = request -> response

  type 'a server = 'a -> app -> unit

  datatype html = HTML of string

  (* HTTP error and redirect codes *)
  datatype http_exn = HTTP300MultipleChoices
                    | HTTP301Moved of string
                    | HTTP302Found of string
                    | HTTP303SeeOther of string
                    | HTTP304NotModified
                    | HTTP305UseProxy of string
                    | HTTP307TemporaryRedirect of string
                    | HTTP400BadRequest
                    | HTTP401Unauthorized
                    | HTTP403Forbidden
                    | HTTP404NotFound
                    | HTTP405MethodNotAllowed
                    | HTTP406NotAcceptable
                    | HTTP407ProxAuthReq 
                    | HTTP408RequestTimeout 
                    | HTTP409Conflict
                    | HTTP410Gone
                    | HTTP411LengthRequired
                    | HTTP412PreconditionFailed
                    | HTTP413ReqEntityTooLarge
                    | HTTP414ReqURITooLong
                    | HTTP415UnsuppMediaType
                    | HTTP416RangeNotSatisfiable
                    | HTTP417ExpectationFailed
                    | HTTP500InternalServerError of string
                    | HTTP501NotImplemented
                    | HTTP502BadGateway
                    | HTTP503ServiceUnavailable
                    | HTTP504GatewayTimeout
                    | HTTP505VersionNotSupported

  exception HTTPExn of http_exn

end

signature WEB_SERVER = sig

  type opts

  val serve : opts Web.server

  val addCleanupCallback: (unit -> unit) -> unit

end
