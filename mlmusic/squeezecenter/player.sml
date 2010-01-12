structure PlayerApp :> sig val playerApp: Web.app end = struct

  structure U = WebUtil

  fun playerApp (req: Web.request) = let
        val path = U.postpath req
      in
        (case path of

          [ player, "status" ] => let
              val cmdStr = Byte.bytesToString (#content req ())
              val () = print ("Command: \"" ^ cmdStr ^ "\"\n")

              val commands = String.fields (fn c => c = #" ") cmdStr

              val cmd = case commands of
                  [ "repeat" ] => SOME [ player, "playlist", "repeat" ]
                | [ "shuffle" ] => SOME [ player, "playlist", "shuffle" ]
                | [ "pause" ] => SOME [ player, "pause" ]
                | [ "prev" ] => SOME [ player, "playlist", "index", "-1" ]
                | [ "next" ] => SOME [ player, "playlist", "index", "+1" ]
                | [ "volup" ] => SOME [ player, "mixer", "volume", "+10" ]
                | [ "voldown" ] => SOME [ player, "mixer", "volume", "-10" ]
                | "add" :: cmds => SOME (player :: "playlist" :: "addtracks" :: cmds)
                | "play" :: cmds => SOME (player :: "playlist" :: "loadtracks" :: cmds)
                | "pljump" :: cmds => SOME (player :: "playlist" :: "index" :: cmds)
                | "pldel" :: cmds => SOME (player :: "playlist" :: "delete" :: cmds)
                | "plmove" :: cmds => SOME (player :: "playlist" :: "move" :: cmds)
                | [ "" ] => NONE
                | _ => (print "Command: unknown command!\n"; NONE)

              val _ = case cmd of SOME cmd => Command.command cmd
                                | NONE => nil
            in
              U.resp "text/plain" (Command.statusJSON (SOME (Command.statusRaw player)))
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
