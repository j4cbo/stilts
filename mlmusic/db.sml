structure DB = struct

(*
  val conn_info_root : MySQLClient.connect_info = {
        host = SOME "localhost", port = 0w0, unix_socket = NONE,
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }

  val conn_info_sock : MySQLClient.connect_info = {
        host = NONE, port = 0w0, unix_socket = SOME "/var/lib/squeezecenter/cache/squeezecenter-mysql.sock",
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }
*)

  fun fold_tracks res = let
        fun processSingle { id: string, tracknum, title, lossless, albumId, albumTitle,
                                                      artistId, artistName, bitrate, ct } =
              { id = id, tracknum = tracknum, title = title, lossless = lossless,
			bitrate = bitrate, ct = ct,
                album = case (albumId, albumTitle) of
                          (SOME i, SOME t) => SOME { id = i, name = t }
                        | _ => NONE,
                artists = case (artistId, artistName) of
                            (SOME i, SOME n) => [ { id = i, name = n } ]
                          | _ => nil }

        fun process (item, nil) = [ processSingle item ]
          | process (item as { id, artistId, artistName, ... },
                     (prev as { id = pId, tracknum = pNum, title = pTitle, lossless = pLossless, ct = pct, bitrate = pBitrate,
                       album = pAlbum, artists = pArtists }) :: rest) =
              if id = pId
              then { id = pId, tracknum = pNum, title = pTitle, lossless = pLossless, ct = pct, bitrate = pBitrate, album = pAlbum,
                     artists = case (artistId, artistName) of
                         (SOME i, SOME n) => { id = i, name = n } :: pArtists
                       | _ => pArtists } :: rest
              else (processSingle item) :: prev :: rest
      in
        rev (foldl process nil res)
      end
        

end
