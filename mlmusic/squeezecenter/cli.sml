signature CLI = sig

  type conn

  val connect: string * int -> conn

  val command: conn -> string list -> string list

end

structure CLI :> CLI = struct

  structure LR = LineReader(Socket)
  structure S = Socket

  type conn = (INetSock.inet, S.active S.stream) S.sock * LR.reader

  fun connect (hostname, port) = let
        val host = case NetHostDB.getByName hostname of
                     SOME host => host
                   | NONE => raise Fail ("error looking up " ^ hostname)
        val target = INetSock.toAddr (NetHostDB.addr host, port)
        val s = INetSock.TCP.socket ()
        val () = S.connect (s, target)
      in
        (s, LR.new (s, { increment = 8192, stripCR = true }))
      end

  fun unquote v = let
      fun process (s, acc) = (let
          val vchars = Substring.string (Substring.slice (s, 0, SOME 2))
          val v = Word8.fromString vchars
        in
             Substring.full (String.str (Byte.byteToChar (valOf v)))
          :: Substring.slice (s, 2, NONE)
          :: acc
        end
        handle Overflow => Substring.full "%" :: s :: acc
             | Subscript => Substring.full "%" :: s :: acc
             | Option => Substring.full "%" :: s :: acc)
    in
      case Substring.fields (fn c => c = #"%") v of
        nil => ""
      | x::rest => Substring.concat (x :: foldr process nil rest)
    end

  fun command (s, r) ins = let

        val timer = PrettyTimer.start ()

        val () = print ("CLI: sending: " ^ String.concatWith " " ins ^ "\n")

        fun hex2 i = StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX i)
        fun quote #"-" = "-"
          | quote #"_" = "_"
          | quote #"~" = "~"
          | quote #"." = "."
          | quote c = if Char.isAlphaNum c
                      then String.str c
                      else "%" ^ (hex2 (Char.ord c))

        val out = String.concatWith " " (map (String.translate quote) ins)

        val () = print ("CLI: out computed: " ^ PrettyTimer.print timer ^ "\n")

        val () = SockUtil.sendVec (s, Byte.stringToBytes (out ^ "\n"))
        val () = print ("CLI: sent: " ^ PrettyTimer.print timer ^ "\n")
        val resp = Byte.bytesToString (LR.readline r)
        val () = print ("CLI: read response: " ^ Int.toString (size resp) ^ " bytes, " ^ PrettyTimer.print timer ^ "\n")
        val fields = Substring.fields (fn c => c = #" ") (Substring.full resp)
        val () = print ("CLI: parsed fields: " ^ PrettyTimer.print timer ^ "\n")
        val resFields = map unquote fields

        val () = print ("CLI: command took " ^ PrettyTimer.print timer ^ "\n")

      in
        resFields
      end
end
