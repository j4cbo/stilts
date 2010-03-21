structure MFBrowser :> sig val browseApp: Web.app end = struct

  structure U = WebUtil
  structure J = JSON

  val pageLen = 50

  (* val pairs: 'a list -> ('a * 'a) list
   *
   * Combine a list of even length into pairs of adjacent elements:
   *   [ a, b, c, d, e, f ] ==> [ (a, b), (c, d), (e, f) ]
   *
   * A trailing element, if present, will be ignored.
   *)
  fun pairs [] = nil
    | pairs [_] = nil
    | pairs (a::b::r) = (a, b)::(pairs r)


  fun getString obj key = case J.Map.find (obj, key) of
                              SOME (J.String s) => SOME s
                            | _ => NONE


  fun browseApp (req: Web.request) = let
        val path = U.postpath req
        val form = Form.load req
        val header_info = Request.get_header req
        val base = String.concatWith "/" ("" :: U.prepath req)

        (* Assert that the path ends with a trailing / *)
        val () = case rev path of ""::_ => () | _ => raise U.notFound
  
        val pathPairsRev = rev (pairs path)

        (* Generate breadcrumb links *)
        fun makeLinks (nil, acc) = acc
          | makeLinks (p as ((id, name)::rest), acc) = let
                val () = case Int.fromString id of NONE => raise U.notFound
                                               | _ => ()
                val url = base ^ "/"
                        ^ String.concatWith "/" (map
                            (fn (i, n) => i ^ "/" ^ n)
                             (rev p))
                        ^ "/"
              in
                makeLinks (rest, (name, url)::acc)
              end

        val backlinks = ("Music Folder", base ^ "/")
                        :: makeLinks (pathPairsRev, nil)

        val start = Option.getOpt
              (Option.mapPartial IntInf.fromString (Form.get form "start"), 0)

        (* Produce the actual request *)
        val req = J.String "musicfolder"
                  :: J.Number start
                  :: J.Number (IntInf.fromInt pageLen)
                  :: (case pathPairsRev of
                        ((id, _)::_) => [ J.String ("folder_id:" ^ id) ]
                      | _ => nil)

        val resp = JSONRPC.request req
        val list = JSONRPC.unpack "folder_loop" "filename" resp

        fun page i = (i * pageLen, pageLen, 0, Int.toString (i + 1))
        fun makePagebar count =
              if count < pageLen then NONE
              else SOME (List.tabulate ((count - 1) div pageLen + 1, page))

        val pb = case resp of
              J.Object m => (
                case J.Map.find (m, "count") of
                  SOME (J.String s) =>
                    Option.mapPartial makePagebar (Int.fromString s)
                | SOME (J.Number i) => makePagebar (Int.fromLarge i)
                | _ => NONE)
            | _ => NONE

         fun renderItem { id, map, name } = (case getString map "type" of
               SOME "folder" =>
                 TFolderItem.render { id = id, filename = name }
             | SOME "track" =>
                 TTrackItem.render NONE {
                   id = id,
                   tracknum = NONE,
                   title = Option.getOpt (getString map "title", name),
                   artists = nil, album = NONE, lossless = NONE, bitrate = NONE, ct = NONE
                 }
             | _ => Web.HTML ""
           )
      in
         U.xhtmlResp (TList.render {
           all = NONE,
           path = backlinks,
           title = #1 (hd (rev backlinks)),
           list = list,
           pb = pb,
           start = Int.fromLarge start,
           perItem = renderItem
         } header_info)
      end
end
