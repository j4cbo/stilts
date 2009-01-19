structure CLIBrowser :> sig val browseApp: Web.app end = struct

  structure U = WebUtil
  structure J = JSON

  val pageLen = 200

  (* val pairs: 'a list -> ('a * 'a) list
   *
   * Combine a list of even length into pairs of adjacent elements:
   *   [ a, b, c, d, e, f ] ==> [ (a, b), (c, d), (e, f) ]
   *
   * A trailing element, if present, will be ignored.
   *)
  fun pairs nil = nil
    | pairs (_::nil) = nil
    | pairs (a::(b::r)) = (a, b)::(pairs r)

  val hierarchy = [
        ("genre", "Genres", "genre.id="),
        ("artist", "Artists", "contributor.id="),
        ("album", "Albums", "album.id=")
      ]

  fun browseApp (req: Web.request) = let
        val path = U.postpath req
        val form = Form.load req
        val base = String.concatWith "/" (U.prepath req)
        val (start, rest) = case path of a::b => (a, b)
                                       | _ => raise U.notFound

        val () = case rev path of ""::_ => ()
                                | _ => raise U.notFound
  
        val pathPairs = pairs rest

        (* Find where in the browse hierarchy we're starting *)
        fun find nil = raise U.notFound
          | find ((i as (key, _, _))::rest) =
              if (key ^ "s") = start then (i, rest) else find rest
  
        val (startInfo, qHierarchy) = find hierarchy
        val (_, startName, _) = startInfo

        (* Advance up the hierarchy given path info *)
        fun advance ((id, name), (_, _, _, nil)) = raise U.notFound
          | advance ((id, name), (pathAcc, filterAcc, tagAcc, h::hierAcc)) = let
              val (hkey, _, hfprefix) = h
              val () = if List.all Char.isDigit (String.explode id)
                       then ()
                       else raise U.notFound
            in
              ( (id, name) :: pathAcc,
                (hkey ^ "_id:" ^ id) :: filterAcc,
                " " ^ hfprefix ^ id ^ tagAcc,
                hierAcc)
            end

        val (pathRev, filters, tags, hierRemainder) =
               foldl advance (nil, nil, "", startInfo :: qHierarchy)
                             pathPairs 

        (* Generate breadcrumb links *)
        fun makeLinks (nil, acc) = acc
          | makeLinks (p as ((id, name)::rest), acc) = let
                val url = "/" ^ base ^ "/" ^ start ^ "/"
                        ^ String.concatWith "/" (map
                            (fn (i, n) => i ^ "/" ^ n)
                             (rev p))
                        ^ "/"
              in
                makeLinks (rest, (name, url)::acc)
              end

        val backlinks = (startName, "/" ^ base ^ "/" ^ start ^ "/")
                        :: makeLinks (pathRev, nil)

        (* Item render function *)
        fun idName { id, name, map } = { id = id, name = name }
        fun idNameTracknum { id, name, map } = (
              case J.Map.find (map, "tracknum") of
                SOME (J.String s) => { id = id, name = s ^ ". " ^ name }
              | SOME (J.Number n) => { id = id, name = IntInf.toString n
                                                     ^ ". " ^ name }
              | _ => { id = id, name = name }
            )        

        (* Produce our actual query *)
        val (queryBase, cmdPrefix, cmdSuffix, nextPrefix, params, lmap) =
          case hierRemainder of
                nil => ("title", "track.id=", "", SOME "/browse/tracks",
                        [ "sort:tracknum" ], idNameTracknum)
             | (qb, _, cp)::_ => (qb, cp, tags, SOME "", nil, idName)

        val start = case (Form.get form "start") of
              NONE => 0
            | SOME st => case IntInf.fromString st of
                  NONE => 0
                | SOME i => i

        val req = J.String (queryBase ^ "s")
                  :: J.Number start
                  :: J.Number (IntInf.fromInt pageLen)
                  :: map J.String (params @ filters)

        val () = print ("Query: " ^ J.fmt (J.Array req) ^ "\n")

        val resp = JSONRPC.request req
        val list = JSONRPC.unpack (queryBase ^ "s_loop") queryBase resp

        fun page i = (i * pageLen, pageLen, 0, Int.toString (i + 1))

        fun makePagebar count =
              if count < pageLen then NONE
              else SOME (List.tabulate ((count - 1) div pageLen + 1, page))

        val pb = case resp of
              J.Object m => (
                case J.Map.find (m, "count") of
                  SOME (J.String s) => (
                    case Int.fromString s of SOME i => makePagebar i
                                              | NONE => NONE)
                | SOME (J.Number i) => makePagebar (Int.fromLarge i)
                | _ => NONE)
            | _ => NONE
      
      in
         U.htmlResp (TList.render {
           path = backlinks,
           title = #1 (hd (rev backlinks)),
           cmdPrefix = cmdPrefix,
           cmdSuffix = cmdSuffix,
           list = map lmap list,
           pb = pb,
           start = Int.fromLarge start,
           allCmd = NONE,
           nextPrefix = nextPrefix
         })
      end
end
