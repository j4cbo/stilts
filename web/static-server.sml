structure StaticServer :> sig

  val server: string -> Web.app

end = struct

  structure U = WebUtil

  val content_type = fn
        ".png" => "image/png"
      | ".gif" => "image/gif"
      | ".jpg" => "image/jpeg"
      | ".css" => "text/css"
      | ".js" => "text/javascript"
      | ".html" => "text/html"
      | _ => "text/plain" 

  fun server basePath (req: Web.request) = let

        val (_, reqPath) = #path req 

        fun isBad ".." = true
          | isBad _ = false

        val () = if List.exists isBad reqPath then raise U.notFound else ()

        val reqPathStr = OS.Path.toString { isAbs = false, vol = "",
                                            arcs = basePath :: reqPath }

        val stream = BinIO.openIn reqPathStr
        val data = BinIO.inputAll stream
        val () = BinIO.closeIn stream

        val { ext, ... } = OS.Path.splitBaseExt reqPathStr
        val ct = content_type (case ext of SOME e => e | NONE => "")
      in
        ( [("Content-Type", ct)], data)
      end 
      handle Io => raise U.notFound

end
