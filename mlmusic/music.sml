structure Wiki = struct

  structure U = WebUtil

  fun players () = let
        fun process p = { id = valOf (Command.Map.find (p, "playerid")),
                          name = valOf (Command.Map.find (p, "name")) }
        val (_, players) = Command.players ()
      in
        map process players
      end

  fun rootHandler (req: Web.request) = (case U.postpath req of
        nil => U.htmlResp (TIndex.render { players = players () })
      | [ "" ] => U.htmlResp (TIndex.render { players = players () })
      | _ => raise U.notFound
    )

  val app = U.dispatch [ ( [ "browse" ], U.PREFIX, Browser.browseApp ),
                         ( [ "search", "" ], U.EXACT, SearchApp.searchApp ),
                         ( nil, U.PREFIX, rootHandler ) ]

  val conn_info : MySQLClient.connect_info = {
        host = SOME "127.0.0.1", port = 0w3306, unix_socket = NONE,
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }
(*
  val conn_info : MySQLClient.connect_info = {
        host = SOME "localhost", port = 0w0, unix_socket = NONE,
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }
*)
  val () = DB.connect ()
  val () = SearchFile.init "index/db.idx"

  fun timer app req = let
        val realTimer = Timer.startRealTimer ()
        val cpuTimer = Timer.startCPUTimer ()
        val resp = app req
        val realTime = Timer.checkRealTimer realTimer
        val { usr, sys } = Timer.checkCPUTimer cpuTimer
        val () = print ("Request time: " 
               ^ Real.toString ((Time.toReal realTime) * 1000.0) ^ " ms total, "
               ^ Real.toString ((Time.toReal usr) * 1000.0) ^ " ms user, "
               ^ Real.toString ((Time.toReal sys) * 1000.0) ^ " ms system\n")
      in
        resp
       end

  val app = timer (U.dumpRequestWrapper print (U.exnWrapper app))

 

  fun main _ = let
      val () = print "Listening...\n"
      val () = FastCGIServer.serve (INetSock.any 5124) app
    in
      0
    end

end
