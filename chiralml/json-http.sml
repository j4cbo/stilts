structure J = JSON

val test = J.Object (foldl J.Map.insert' J.Map.empty [
             ("channel", J.String "/meta/handshake"),
             ("version", J.String "1.0"),
             ("supportedConnectionTypes", J.Array [ J.String "streaming" ])
           ])


val test2 = J.Object (foldl J.Map.insert' J.Map.empty [
              ("id", J.Number 1),
              ("method", J.String "slim.request"),
              ("params", J.Array [
                J.String "00:04:20:1e:00:50",
                J.Array [
                  J.String "status",
                  J.String "-",
                  J.Number 1,
                  J.String "tags:uB",
                  J.String "menu:menu"
                ]
              ])
            ])

val test2 = J.Object (foldl J.Map.insert' J.Map.empty [
              ("id", J.Number 1),
              ("method", J.String "slim.request"),
              ("params", J.Array [
                J.String "00:04:20:1e:00:50",
                J.Array [
                  J.String "artists",
                  J.Number 0,
                  J.Number 1000,
                  J.String "menu:album"
                ]
              ])
            ])

(*
structure T = Thread (structure T = ThreadBase
                      structure RC = SelectReactorCore)
structure CS = ChiralSocketFn(T)
*)
structure HC = SimpleHTTPConnection(structure Socket = Socket structure INetSock = INetSock)

val conn = HC.new "blackbox.res.cmu.edu:9000"


fun cometd_request conn reqs = let
      val body = HC.request conn ("/cometd",
                                  [("Content-Type", "text/json")],
                                  SOME (J.fmt (J.Array reqs)))
      val rjson = case JSON.fromString body of
                     NONE => raise Fail "response not json"
                   | SOME json => json
    in
      case rjson of JSON.Array arr => arr
                  | _ => raise Fail "expected json array result"
    end


(* val res = cometd_request conn [ test ]
*)

val response : string ref = ref ""
fun rpc_request conn req = let
      val timer = PrettyTimer.start ()
      val body = HC.request conn ("/jsonrpc.js",
                                  [("Content-Type", "text/json")],
                                  SOME (J.fmt req))
      val () = print ("JSONRPC: request " ^ PrettyTimer.print timer ^ "; "
                      ^ Int.toString (size body) ^ " bytes\n")
      val timer = PrettyTimer.start ()
      val () = response := body
      val rjson = case JSON.fromString body of
                     NONE => raise Fail "response not json"
                   | SOME json => json
      val () = print ("JSONRPC: JSON parse " ^ PrettyTimer.print timer ^ "\n")
    in
      rjson
    end
val () = print (J.fmt test2)
val res2 = rpc_request conn test2
(*

val i = case res2 of J.Object m => J.Map.listItemsi m

val res = case res2 of J.Object m => J.Map.find (m, "result")

val resitems = case res of SOME (J.Object m) => J.Map.listItemsi m
*)
