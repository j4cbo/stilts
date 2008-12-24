structure StaticServer :> sig

  val server: { basepath: string,
                expires: LargeInt.int option,
                headers: Web.header list } -> Web.app

end = struct

  structure U = WebUtil

  val content_type = fn
        "png" => "image/png"
      | "gif" => "image/gif"
      | "jpg" => "image/jpeg"
      | "css" => "text/css"
      | "js" => "text/javascript"
      | "html" => "text/html"
      | _ => "text/plain" 

  fun server { basepath, expires, headers } (req: Web.request) = let

        val (_, reqPath) = #path req 

        fun isBad ".." = true
          | isBad _ = false

        val () = if List.exists isBad reqPath then raise U.notFound else ()

        val reqPathStr = OS.Path.toString { isAbs = false, vol = "",
                                            arcs = basepath :: reqPath }

        val stream = BinIO.openIn reqPathStr
        val data = BinIO.inputAll stream
        val () = BinIO.closeIn stream

        val { ext, ... } = OS.Path.splitBaseExt reqPathStr

        val ct = content_type (case ext of SOME e => e | NONE => "")

        val formatTime = HTTPDate.format o Date.fromTimeUniv
        val now = Time.now ()

        val headers = ("Content-Type", ct)
                   :: ("Date", formatTime now)
                   :: headers

        val headers = case expires of
              NONE => headers
            | SOME secs => ("Expires",
                            formatTime (Time.+ (now, Time.fromSeconds secs)))
                           :: headers
      in
        (headers, data)
      end 
      handle Io => raise U.notFound

end
