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
        fun processSingle { id: int, tracknum, title, albumId, albumTitle,
                                                      artistId, artistName } =
              { id = id, tracknum = tracknum, title = title,
                album = case (albumId, albumTitle) of
                          (SOME i, SOME t) => SOME { id = i, title = t }
                        | _ => NONE,
                artists = case (artistId, artistName) of
                            (SOME i, SOME n) => [ { id = i, name = n } ]
                          | _ => nil }

        fun process (item, nil) = [ processSingle item ]
          | process (item as { id, artistId, artistName, ... },
                     (prev as { id = pId, tracknum = pNum, title = pTitle,
                       album = pAlbum, artists = pArtists }) :: rest) =
              if id = pId
              then { id = pId, tracknum = pNum, title = pTitle, album = pAlbum,
                     artists = case (artistId, artistName) of
                         (SOME i, SOME n) => { id = i, name = n } :: pArtists
                       | _ => pArtists } :: rest
              else (processSingle item) :: prev :: rest
      in
        foldl process nil res
      end
        

end
