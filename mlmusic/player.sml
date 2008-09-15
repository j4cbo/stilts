structure PlayerApp :> sig val playerApp: Web.app end = struct

  structure U = WebUtil

  fun playerApp (req: Web.request) = let
        val path = U.postpath req
      in
        (case path of

          [ player, "status" ] => let
              val cmdStr = Byte.bytesToString (#content req ())
              val () = print ("Command: \"" ^ cmdStr ^ "\"\n")

              (* SML/NJ v110.54's Byte.bytesToString appears to be buggy;
               * it occasionaly returns strings on which pattern matching
               * doesn't work properly. Running the string through
               * String.translate String.str is an ugly workaround... *)

              val cmd = case (String.translate String.str cmdStr) of
                  "repeat" => SOME [ player, "playlist", "repeat" ]
                | "shuffle" => SOME [ player, "playlist", "shuffle" ]
                | "pause" => SOME [ player, "pause" ]
                | "prev" => SOME [ player, "playlist", "index", "-1" ]
                | "next" => SOME [ player, "playlist", "index", "+1" ]
                | "" => NONE
                | s => (print "Command: unknown command!\n"; NONE)

              val _ = case cmd of SOME cmd => CLI.command Command.c cmd
                                | NONE => nil
            in
              U.resp "text/plain" (Command.status player)
            end 

        | [ player, "playlist" ] => let
              val (prologue, tracks) = Command.playlist player 0 9999
            in
              U.htmlResp (TPlaylist.render (prologue, tracks))
            end

        | _ => raise U.notFound
    )
    end


end
