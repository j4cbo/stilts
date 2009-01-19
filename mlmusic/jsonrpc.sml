structure JSONRPC :> sig
  val request: JSON.json list -> JSON.json
  val unpack: string -> string -> JSON.json
              -> { id: string, name: string, map: JSON.json JSON.Map.map } list
end
= struct

  structure J = JSON
  structure HC = SimpleHTTPConnection(structure Socket = Socket
                                      structure INetSock = INetSock)

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
                      SOME { id=id, name=name, map=om }
                | (SOME (J.Number id), SOME (J.String name)) =>
                      SOME { id=IntInf.toString id, name=name, map=om }
                | _ => NONE)
             | _ => NONE) items
        | _ => nil)
  | unpack _ _ _ = nil

end
