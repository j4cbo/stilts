structure Browser :> sig val browseApp: Web.app end = struct

  structure U = WebUtil

  fun renderTrack { id, tracknum, title } = {
        id = id,
        name = case tracknum of NONE => title
                              | SOME t => (Int.toString t) ^ ". " ^ title
      }


  fun albumInfo albumId = let
        val albumId' = valOf (Int.fromString albumId)
        val albumTitle = case #title (SQL.albumDetail albumId') of
                           NONE => "Album"
                         | SOME t => t
      in
        (albumId', albumTitle)
      end

  fun artistInfo artistId = let
        val artistId' = valOf (Int.fromString artistId)
        val artist = #name (SQL.artistDetail artistId')
      in
        (artistId', artist)
      end


  fun pbSubApp (req, ns, sublist) = let
        val pb = PageBar.pagebar (U.postpath req, ns)
        val form = Form.load req

        val startlen = case (Form.get form "start", Form.get form "len") of
              (SOME st, SOME len) => (Int.fromString st, Int.fromString len)
            | _ => (NONE, NONE)

        val (start, len) = case startlen of
              (SOME st, SOME len) => (st, len)
            | _ => (case pb of SOME ((fpStart, fpLen, _, _) :: _) =>
                                        (fpStart, fpLen)
                             | _ => (0, 9999999))
      in
        (pb, sublist (len, start), start)
      end


  fun browseApp (req: Web.request) = let
        val path = U.postpath req
      in
        U.htmlResp (case path of

          [ "home" ] => THome.render () 

        | [ "artists", "" ] => let
            val (pb, list, start) = pbSubApp (req, SQL.allArtistsNamesort,
                                                   SQL.allArtistsRange)
          in
            TList.render {
              path = [ ( "Artists", "/browse/artists/" ) ],
              nextPrefix = SOME "",
              title = "All Artists",
              cmdId = "artist",
              list = list,
              pb = pb,
              start = start
            }
          end

        | [ "artists", id, "" ] => let
            val id' = valOf (Int.fromString id)
            val title = #name (SQL.artistDetail id')
            val pb = PageBar.pagebar (path, fn () => map #name (SQL.albumsByArtist id'))
          in TList.render {
            nextPrefix = SOME "",
            path = [ ( "Artists", "/browse/artists/" ),
                     ( title, "/browse/artists/" ^ id ^ "/") ],
            list = SQL.albumsByArtist id',
            title = title,
            cmdId = "album",
            pb = pb,
            start = 0
        }
        end

      | [ "artists", artistId, albumId, "" ] => let
	    val (artistId', artistName) = artistInfo artistId
            val (albumId', albumTitle) = albumInfo albumId
          in TList.render {
            nextPrefix = SOME "/browse/song/",
            path = [ ( "Artists", "/browse/artists/" ),
                     ( artistName, "/browse/artists/" ^ artistId ^ "/"),
                     ( albumTitle, "/browse/artists/" ^ artistId ^ "/" ^ albumId ^ "/") ],
            list = map renderTrack (SQL.albumTracks albumId'),
            title = albumTitle,
            cmdId = "song",
            pb = NONE, start = 0
        }
        end

      | [ "albums", "" ] => let
          val (pb, list, start) = pbSubApp (req, SQL.allAlbumsNamesort,
                                                 SQL.allAlbumsRange)
        in
          TList.render {
            nextPrefix = SOME "",
            path = [ ( "Albums", "/browse/albums/" ) ],
            list = list,
            title = "All Albums",
            cmdId = "album",
            pb = pb,
            start = start
          }
        end

      | [ "albums", albumId, "" ] => let
            val (albumId', albumTitle) = albumInfo albumId
          in TList.render {
            nextPrefix = SOME "/browse/song/",
            path = [ ( "Albums", "/browse/albums/" ),
                     ( albumTitle, "/browse/artists/" ^ albumId ^ "/") ],
            list = map renderTrack (SQL.albumTracks albumId'),
            title = albumTitle,
            cmdId = "song",
            pb = NONE, start = 0
        }
        end

      | [ "song", songId, "" ] => let
            val songInfo = SQL.songInfo (valOf (Int.fromString songId))
          in
            TSong.render songInfo
          end

      | _ => raise U.notFound

    )
    handle Option => raise U.notFound 
    end

end
