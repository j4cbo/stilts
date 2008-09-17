structure SearchApp :> sig val searchApp: Web.app end = struct

  structure U = WebUtil

  fun renderTrack { id, tracknum, title } = {
        id = id,
        name = case tracknum of NONE => title
                              | SOME t => (Int.toString t) ^ ". " ^ title
      }

  fun search q = let
        val start = Timer.startRealTimer ()

        val { artists as artistIds,
              albums as albumIds,
              tracks as songIds, time }= Search.search q

        val artists = case artistIds of nil => nil
                                      | _ => SQL.artistMulti artistIds
        val albums = case albumIds of nil => nil
                                    | _ => SQL.albumMulti albumIds
        val songs = case songIds of nil => nil
                                  | _ => SQL.trackMulti songIds

        val ttime = Timer.checkRealTimer start

        val () = print ("Search: done; index " ^ Real.toString ((Time.toReal time) * 1000.0)
                        ^ " ms, total " ^ Real.toString ((Time.toReal ttime) * 1000.0)
                        ^ " ms\n");

        val opts = { artists = artists,
                     albums = albums,
                     songs = DB.fold_tracks songs,
                     itime = time,
                     ttime = ttime }
      in
        U.htmlResp (TSearch.render (q, SOME opts))
      end

  fun searchApp (req: Web.request) = let
          val form = Form.load req
        in
          case Form.get form "q" of
            SOME q => search q
          | NONE => U.htmlResp (TSearch.render ("", NONE))
        end

end
