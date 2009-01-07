functor SimpleHTTPConnection (CS : CHIRAL_SOCKET) :> sig
  type t
  val new: string -> t
  val new': INetSock.inet Socket.sock_addr -> t
  val request: t -> string * (string * string) list * string option -> string
end
= struct
  structure S = CS.Socket
  structure SU = ChiralSockUtil(CS)
  structure INS = CS.INetSock
  structure LR = LineReader(S)

  type t = S.active INS.stream_sock * LR.reader

  fun new' target = let
        val sock = INS.TCP.socket ()
        val () = S.connect (sock, target)
      in
        (sock, LR.new (sock, { increment = 8192, stripCR = true }))
      end

  fun new target = let
        val hp = case SockUtil.addrFromString target of
                     NONE => raise Fail "unable to parse address" 
                   | SOME hp => hp

        val { addr, port, ... } = SockUtil.resolveAddr hp
        val port = case port of SOME p => p
                              | NONE => raise Fail "port required" 
      in
        new' (INS.toAddr (addr, port))
      end


  fun format_header (k, v) = k ^ ": " ^ v ^ "\r\n"

  fun format_get headers path =
        "GET " ^ path ^ " HTTP/1.0\r\nUser-Agent: ChiralML/0.0\r\n"
        ^ headers ^ "\r\n"

  fun format_post headers path pdata =
        "POST " ^ path ^ " HTTP/1.0\r\nUser-Agent: ChiralML/0.0\r\n"
      ^ "Content-Length: " ^ Int.toString (size pdata) ^ "\r\n" ^ headers
      ^ "\r\n" ^ pdata

  fun request (conn, lr) (path, headers, postdata) = let
        val headers' = String.concat (map format_header headers)

        val req = case postdata of NONE => format_get headers' path
                                 | SOME pd => format_post headers' path pd

        val () = SU.sendVec (conn, Byte.stringToBytes req)

        val content_length : string option ref = ref NONE

        fun read_headers acc = (
              case Byte.bytesToString (LR.readline lr) of
                "" => acc
              | line => let
                          val (sk, sv) = Substring.splitl (fn c => c <> #":")
                                                          (Substring.full line)
                          fun dropf #":" = true
                            | dropf c = Char.isSpace c
                          val k = Substring.string sk
                          val v = Substring.string (Substring.dropl dropf sv)
                        in
                          ( case String.map Char.toUpper k of
                              "CONTENT-LENGTH" => content_length := SOME v
                            | _ => ());
                          read_headers ((k, v) :: acc)
                        end)

        val headers = rev (read_headers nil)

        val content = case !content_length of
              NONE => raise Fail "expected content-length"
            | SOME lengthstr => case Int.fromString lengthstr of
                 NONE => raise Fail "invalid content-length"
               | SOME length => Byte.bytesToString (LR.readbytes lr length)

      in
        content
      end
end
