structure CLIBrowser :> sig val browseApp: Web.app end = struct

  structure U = WebUtil
  structure J = JSON
  structure HC = SimpleHTTPConnection(structure Socket = Socket
                                      structure INetSock = INetSock)

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

  val id = ref 1 : IntInf.int ref

  fun browseReq req = 
        J.Object (foldl J.Map.insert' J.Map.empty [
            ("id", J.Number (!id)),
            ("method", J.String "slim.request"),
            ("params", J.Array [ J.String "", J.Array req ])
          ])
        before id := (!id + 1)

  fun rpc_request conn req = let
        val timer = PrettyTimer.start ()
        val body = HC.request conn ("/jsonrpc.js",
                                    [("Content-Type", "text/json")],
                                    SOME (J.fmt req))
        val () = print ("JSONRPC: request " ^ PrettyTimer.print timer ^ "; "
                        ^ Int.toString (size body) ^ " bytes\n")
        val timer = PrettyTimer.start ()
        val rjson = case JSON.fromString body of
                       NONE => (print body; raise Fail "response not json")
                     | SOME json => json
        val () = print ("JSONRPC: JSON parse " ^ PrettyTimer.print timer ^ "\n")
        val robj = case rjson of J.Object m => m
                               | _ => raise Fail "response not a json object"
      in
        case J.Map.find (robj, "result") of SOME res => res
                                          | _ => raise Fail "no result"
    end

  fun request rlist = let
        val conn = HC.new "blackbox.res.cmu.edu:9000"
      in
        rpc_request conn (browseReq rlist)
      end

  fun unpack loopkey namekey (J.Object m) = (
        case J.Map.find (m, loopkey) of
          SOME (J.Array items) => List.mapPartial (
            fn (J.Object om) => (
              case (J.Map.find (om, "id"), J.Map.find (om, namekey)) of
                  (SOME (J.String id), SOME (J.String name)) =>
                      SOME { id=id, name=name }
                | (SOME (J.Number id), SOME (J.String name)) =>
                      SOME { id=IntInf.toString id, name=name }
                | _ => NONE)
             | _ => NONE) items
        | _ => nil)
  | unpack _ _ _ = nil

  val hierarchy = [ ("genre", "Genres"),
                    ("artist", "Artists"),
                    ("album", "Albums") ]

  fun browseApp (req: Web.request) = let
        val path = U.postpath req
        val base = String.concatWith "/" (U.prepath req)
        val (start, rest) = case path of a::b => (a, b)
                                       | _ => raise U.notFound

        val () = case rev path of ""::_ => ()
                                | _ => raise U.notFound
  
        val pathPairs = pairs rest

        (* Find where in the browse hierarchy we're starting *)
        fun find nil = raise U.notFound
          | find ((i as (key, _))::rest) =
              if (key ^ "s") = start then (i, rest) else find rest
  
        val ((startKey, startName), qHierarchy) = find hierarchy

        (* Advance up the hierarchy given path info *)
        fun advance ((id, name), (pathAcc, filterAcc, nil)) = raise U.notFound
          | advance ((id, name), (pathAcc, filterAcc, h::hierAcc)) = let
              val (hkey, _) = h
              val () = if List.all Char.isDigit (String.explode id)
                       then ()
                       else raise U.notFound
            in
              ( (id, name) :: pathAcc,
                (hkey ^ "_id:" ^ id) :: filterAcc,
                hierAcc)
            end

        val (pathRev, filters, hierRemainder) =
               foldl advance (nil, nil, (startKey, startName) :: qHierarchy)
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

        (* Produce our actual query *)
        val queryBase = case hierRemainder of nil => "title" | (k, _)::_ => k

        val req = J.String (queryBase ^ "s")
                  :: J.Number 0
                  :: J.Number 100
                  :: map J.String filters

        val () = print ("Query: " ^ J.fmt (J.Array req) ^ "\n")

        val resp = request req
        val list = unpack (queryBase ^ "s_loop") queryBase resp
      in
         U.htmlResp (TList.render {
           path = backlinks,
           nextPrefix = SOME "",
           title = #1 (hd (rev backlinks)),
           cmdPrefix = "contributor.id=",
           cmdSuffix = "",
           allCmd = NONE,
           list = list,
           pb = NONE,
           start = 0
         })
      end
end
