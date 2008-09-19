structure Music = struct

  structure U = WebUtil

  val escapeNQ = String.translate (fn #"<" => "&lt;"
                                    | #"&" => "&amp;"
                                    | c => String.str c)

  fun getCookie req cname = let
        val cookieStr = case U.http_header "HTTP_COOKIE" req of SOME s => s
                                                              | NONE => ""
        fun isSep c = c = #" " orelse c = #";"
        val cookies = String.fields isSep cookieStr

        val cookie = List.find (String.isPrefix (cname ^ "=")) cookies
      in
        case cookie of
          SOME c => SOME (String.extract (c, size cname + 1, NONE))
        | NONE => NONE
      end

  fun index req = let
        val () = print "index\n"
        fun process p = { id = valOf (Command.Map.find (p, "playerid")),
                          name = valOf (Command.Map.find (p, "name")) }
        val () = print "index done\n"

        val (_, playerMap) = Command.players ()
        val players = map process playerMap

        val defaultPlayer = case players of nil => NONE
                                          | ({ id, ... }::_) => SOME id        

        (* Find the player we're using *)
        val player = case (getCookie req "SqueezeCenter-player") of
              SOME s => if List.exists (fn { id, ... } => id = s) players
                        then SOME s
                        else defaultPlayer
            | NONE => defaultPlayer

        val () = print ("Player: " ^ (case player of SOME s => s
                                                   | NONE => "NONE") ^ "\n")

        val initialPlaylist = case player of
              SOME s => TPlaylist.render (Command.playlist s 0 9999)
            | NONE => Web.HTML ""

        val initialStatus = case player of SOME s => Command.status s
                                         | NONE => "null" 
      in
        U.htmlResp (TIndex.render {
                      players = players,
                      initialPlaylist = initialPlaylist,
                      initialStatus = Web.HTML (escapeNQ initialStatus) })
      end

  fun rootHandler (req: Web.request) = (case U.postpath req of
        nil => index req
      | [ "" ] => index req
      | _ => raise U.notFound
    )

  val app = U.dispatch [ ( [ "browse" ], U.PREFIX, Browser.browseApp ),
                         ( [ "search", "" ], U.EXACT, SearchApp.searchApp ),
                         ( [ "player" ], U.PREFIX, PlayerApp.playerApp ),
                         ( nil, U.PREFIX, rootHandler ) ]

  val () = DB.connect ()
  val () = SearchFile.init "searchdb.idx"

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
