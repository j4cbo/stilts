signature CLI = sig

  type conn

  val connect: string * int -> conn

  val command: conn -> string list -> string list

end

structure CLI :> CLI = struct

  structure S = Socket

  type conn = (INetSock.inet, S.active S.stream) S.sock

  fun connect (hostname, port) = let
        val host = case NetHostDB.getByName hostname of
                     SOME host => host
                   | NONE => raise Fail ("error looking up " ^ hostname)
        val target = INetSock.toAddr (NetHostDB.addr host, port)
        val s = INetSock.TCP.socket ()
        val () = S.connect (s, target)
      in
        s
      end

  fun readline sock = let
        fun getc () = case SockUtil.recvStr (sock, 1) of
                        "" => NONE
                      | s => SOME (String.sub (s, 0))

        fun readline' acc = case getc () of NONE => acc
                                          | SOME #"\n" => acc
                                          | SOME c => readline' (c::acc)
      in
        String.implode (rev (readline' nil))
      end

  fun unquote v = let
      val v = String.translate (fn #"+" => " " | c => String.str c) v
      fun process s = let
          val v = Word8.fromString (String.extract (s, 0, SOME 2))
        in
          String.concat [ String.str (Byte.byteToChar (valOf v)),
                          String.extract (s, 2, NONE) ]
        end
        handle Overflow => "%" ^ s
             | Subscript => "%" ^ s
             | Option => "%" ^ s
    in
      String.concat (case String.fields (fn c => c = #"%") v of
                       nil => nil
                     | x::rest => x::(map process rest))
    end

  fun command c ins = let
        fun hex2 i = StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX i)
        fun quote #"-" = "-"
          | quote #"_" = "_"
          | quote #"~" = "~"
          | quote #"." = "."
          | quote c = if Char.isAlphaNum c
                      then String.str c
                      else "%" ^ (hex2 (Char.ord c))

        val out = String.concatWith " " (map (String.translate quote) ins)
        val () = SockUtil.sendVec (c, Byte.stringToBytes (out ^ "\n"))
        val fields = String.fields (fn c => c = #" ") (readline c)
      in
        map unquote fields
      end
end
