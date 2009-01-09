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
      | [ "exit" ] => OS.Process.exit OS.Process.success
      | _ => raise U.notFound
    )

  val staticApp = StaticServer.server { basepath = "static",
                                        expires = SOME (60*60*24*365),
                                        headers = nil }

  val app = U.dispatch [ ( [ "browse" ], U.PREFIX, CLIBrowser.browseApp ),
                         ( [ "search", "" ], U.EXACT, SearchApp.searchApp ),
                         ( [ "player" ], U.PREFIX, PlayerApp.playerApp ),
                         ( [ "static" ], U.PREFIX, staticApp ),
                         ( nil, U.PREFIX, rootHandler ) ]

  val () = DB.connect ()
  val () = SearchFile.init "searchdb.idx"

  fun timer app req = let
        val t = PrettyTimer.start ()
        val resp = app req
        val () = print ("Request time: " ^ PrettyTimer.print t ^ "\n")
        val () = MLton.GC.collect ()
      in
        resp
      end

  val app = timer (U.dumpRequestWrapper print (U.exnWrapper app))

  fun main _ = let
      val () = print "Listening...\n"
      val () = HTTPServer.serve (INetSock.any 8888) app
    in
      0
    end

end
