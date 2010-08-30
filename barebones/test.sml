structure Test = struct

  structure U = WebUtil

  structure I = Introspector(structure T = TH)

  fun handler (req: Web.request) = (case U.postpath req of

        nil => U.resp "text/plain" "lol"
      | [ "" ] => U.resp "text/plain" "lol"

      | [ "introspector", "" ] => I.app req

      | _ => raise U.notFound
    )

  val app = U.dumpRequestWrapper print (U.exnWrapper handler)

  fun main _ = let
      val () = print "Listening...\n"
      val serverthread = CHTTPServer.spawn_server (INetSock.any 5124) app
    in
      TH.run ();
      0
    end

end


structure FHandler = HTTPHandlerFn(structure S = CS.Socket val can_keep_alive = true val server_name = "f")

val s = HTTPServer.serve
