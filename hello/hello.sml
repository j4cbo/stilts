structure Hello = struct

  structure U = WebUtil

  fun handler (req: Web.request) = (case U.postpath req of

        nil =>
          raise U.redirectPostpath req [ "hello" ]

      | [ "" ] =>
          raise U.redirectPostpath req [ "hello" ]

      | [ "hello" ] => U.htmlResp (
              THello.render { blort = "world" }
          )

      | _ => raise U.notFound
    )

  val app = U.dumpRequestWrapper print (U.exnWrapper handler)

  fun main _ = let
      val () = print "Listening...\n"
      val () = HTTPServer.serve (INetSock.any 8888) app
    in
      0
    end

end
