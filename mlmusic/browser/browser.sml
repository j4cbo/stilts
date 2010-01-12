structure Browser :> sig val browseApp: Web.app end = struct

  structure U = WebUtil

  fun renderTrack { id, tracknum, title } = {
        id = id,
        name = case tracknum of NONE => title
                              | SOME t => (Int.toString t) ^ ". " ^ title
      }


  val getInt = valOf o Int.fromString

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


  fun browse_list_params req [ "artists", "" ] = let
            val (pb, list, start) = pbSubApp (req, SQL.allArtistsNamesort,
                                                   SQL.allArtistsRange)
          in TList.render {
              path = [ ( "Artists", "/browse/artists/" ) ],
              title = "All Artists",
              list = list,
              pb = pb,
              start = start,
              perItem = TArtistItem.render true,
              all = NONE
          } end

    | browse_list_params req [ "artists", id, title, "" ] = let
            val id' = getInt id
            val pb = PageBar.pagebar (U.postpath req, fn () => map #name (SQL.albumsByArtist id'))
          in TList.render {
              title = title,
              path = [ ( "Artists", "/browse/artists/" ),
                       ( title, "/browse/artists/" ^ id ^ "/" ^ U.urlencode title ^ "/") ],
              pb = pb, list = SQL.albumsByArtist id', start = 0,
              perItem = TAlbumItem.render false,
              all = SOME (fn () => TListItem.render ("All Songs", "contributor.id=" ^ id))
          } end

    | browse_list_params req [ "artists", artistId, artistName, albumId, albumTitle, "" ] = let
	    val _ = getInt artistId
            val albumId' = getInt albumId
          in TList.render {
              title = albumTitle,
              path = [ ( "Artists", "/browse/artists/" ),
                       ( artistName, "/browse/artists/" ^ artistId ^ "/" ^ U.urlencode artistName ^ "/"),
                       ( albumTitle, "/browse/artists/" ^ artistId ^ "/" ^ U.urlencode artistName
                                   ^ "/" ^ albumId ^ "/" ^ U.urlencode albumTitle ^ "/") ],
              list = DB.fold_tracks (SQL.albumTracks albumId'),
              pb = NONE, start = 0,
              perItem = TTrackItem.render NONE,
              all = SOME (fn () => TListItem.render ("All Songs", "album.id=" ^ albumId ^ " contributor.id=" ^ artistId))
          } end

    | browse_list_params req [ "albums", "" ] = let
          val (pb, list, start) = pbSubApp (req, SQL.allAlbumsNamesort,
                                                 SQL.allAlbumsRange)
        in TList.render {
            title = "All Albums",
            path = [ ( "Albums", "/browse/albums/" ) ],
            pb = pb, list = list, start = start,
            perItem = TAlbumItem.render false, all = NONE
        } end

    | browse_list_params req [ "albums", albumId, albumTitle, "" ] = let
            val albumId' = getInt albumId
        in TList.render {
            path = [ ( "Albums", "/browse/albums/" ),
                     ( albumTitle, "/browse/albums/" ^ albumId ^ "/" ^ U.urlencode albumTitle ^ "/") ],
            list = DB.fold_tracks (SQL.albumTracks albumId'),
            title = albumTitle,
            all = SOME (fn () => TListItem.render ("All Songs", "album.id=" ^ albumId)),
 
            pb = NONE, start = 0,
	    perItem = TTrackItem.render NONE
        } end

     | browse_list_params req _ = raise U.notFound


  fun browse_list req = let
        val renderer = (browse_list_params req (U.postpath req))
                       handle Option => raise U.notFound 
        val (players, status) = Request.get_header req
      in 
        U.xhtmlResp (renderer (players, status))
      end

  fun browseApp (req: Web.request) = (case U.postpath req of
       ( "artists" :: _ ) => browse_list req
     | ( "albums" :: _ ) => browse_list req
     | ( "song" :: songId :: _ ) => let
            val songInfo = SQL.songInfo (getInt songId)
          in
            U.htmlResp (TSong.render songInfo)
          end
     | _ => raise U.notFound
    )

end
