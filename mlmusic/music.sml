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

  fun get_player_info req = let
        val desiredPlayer = Option.map U.urldecode (getCookie req "SqueezeCenter-player")

        (* Go through the cached player list doing two things:
         * - Find the player that the user wants; if they didn't specify one,
         *   take the first one we find
         * - Assemble a list of { id, name } for all players
         *)
        fun plFolder (item : PlayerCache.player_item, (foundPlayer, acc)) = (
              case (desiredPlayer, foundPlayer) of
                  (SOME desired, SOME prev) => if (#id item = desired) then SOME item else SOME prev
                | (NONE, SOME prev) => SOME prev
                | _ => SOME item,
              { id = #id item, name = #name (!(#info item)) } :: acc
            )

        val (player, playerList) = Command.Map.foldl plFolder (NONE, nil) (!PlayerCache.player_cache)

        (* Add a 'cur' field to indicate whether the player is selected *)
        val players = map (fn { id, name } => {
              id = id, name = name,
              cur = case player of SOME p => (id = #id p) | NONE => false
            }) playerList

      in
        (players, player)
      end

  fun index req = let

        val (players, player) = get_player_info req
        val () = print ("Player: " ^ (case player of SOME p => #id p
                                                   | NONE => "NONE") ^ "\n")

        val initialPlaylist = case player of
              SOME p => TPlaylist.render (Command.playlist (#id p) 0 9999)
            | NONE => Web.HTML ""

        val initialStatus = case player of
                  SOME { info = ref info, ... } => SOME (#status info)
                | NONE => NONE
      in
        U.xhtmlResp (TIndex.render {
                      players = players,
                      initialPlaylist = initialPlaylist,
                      initialStatus = initialStatus
                   })
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

  val app = U.dispatch [ ( [ "browse" ], U.PREFIX, Browser.browseApp ),
                         ( [ "search", "" ], U.EXACT, SearchApp.searchApp ),
                         ( [ "player" ], U.PREFIX, PlayerApp.playerApp ),
                         ( [ "musicfolder" ], U.PREFIX, MFBrowser.browseApp ),
                         ( [ "static" ], U.PREFIX, staticApp ),
                         ( nil, U.PREFIX, rootHandler ) ]

  fun timer app req = let
        val t = PrettyTimer.start ()
        val resp = app req
        val () = print ("Request time: " ^ PrettyTimer.print t ^ "\n")
      in
        resp
      end

(*  val app = timer (U.dumpRequestWrapper print (U.exnWrapper app))
*)
  val app = timer (U.exnWrapper app)

  val () = HTTPServer.addCleanupCallback GC.collectAll;
  val httpd = HTTPServer.spawn_server (INetSock.any 8888) app;

  fun main _ = (
        print "Starting up...\n";
        Startup.startup ();
        print "Listening...\n";
        T.run ();
        0
      )

end
