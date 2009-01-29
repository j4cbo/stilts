structure CLISongInfo :> sig

  type track = {
         id: int, tracknum: int option, title: string,
         albumId: int option, albumTitle: string option,
         artistId: int option, artistName: string option }

  val songInfo: int -> track option
  val songInfoMulti: int list -> track list

end
= struct

  type track = {
         id: int, tracknum: int option, title: string,
         albumId: int option, albumTitle: string option,
         artistId: int option, artistName: string option }

  structure J = JSON

  fun songInfo song = let
        val res = JSONRPC.request [
              J.String "songinfo",
              J.Number 0,
              J.Number 99999,
              J.String "tags:elast",
              J.String ("track_id:" ^ Int.toString song)
            ]

        val objArr = case res of
            J.Object obj => (case J.Map.find (obj, "songinfo_loop") of
                               SOME (J.Array arr) => arr 
                             | _ => nil)
          | _ => nil

        fun processObj (J.Object obj, acc) =
              J.Map.foldli (fn (k, v, acc) => J.Map.insert (acc, k, v)) acc obj
          | processObj (_, acc) = acc

        val objs = foldl processObj J.Map.empty objArr

        fun getString key = case J.Map.find (objs, key) of
              SOME (J.String s) => SOME s
            | SOME (J.Number n) => SOME (IntInf.toString n)
            | _ => NONE

        fun getNumber key = case J.Map.find (objs, key) of
              SOME (J.String s) => Int.fromString s
            | SOME (J.Number n) => SOME (IntInf.toInt n)
            | _ => NONE
      in 
        case (getNumber "id", getString "title") of
          (SOME id, SOME title) => SOME {
            id = id,
            title = title,
            tracknum = getNumber "tracknum",
            albumId = getNumber "album_id",
            albumTitle = getString "album",
            artistId = getNumber "artist_id",
            artistName = getString "artist"
          }
        | _ => NONE
      end

  val songInfoMulti = List.mapPartial songInfo

end
