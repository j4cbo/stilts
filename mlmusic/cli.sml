signature CLI = sig

  type conn

  val connect: string * int -> conn

  val command: conn -> string list -> string list

end

structure CLI :> CLI = struct

  structure S = Socket

  type conn = (INetSock.inet, S.active S.stream) S.sock
              * LineReader.reader

  fun connect (hostname, port) = let
        val host = case NetHostDB.getByName hostname of
                     SOME host => host
                   | NONE => raise Fail ("error looking up " ^ hostname)
        val target = INetSock.toAddr (NetHostDB.addr host, port)
        val s = INetSock.TCP.socket ()
        val () = S.connect (s, target)
      in
        (s, LineReader.new (s, 8192))
      end

  fun unquote v = let
      val v = Substring.translate (fn #"+" => " " | c => String.str c) v
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

  fun command (s, r) ins = let
        val timer = Timer.startRealTimer ()
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
        val () = SockUtil.sendVec (s, Byte.stringToBytes (out ^ "\n"))
        val resp = Byte.unpackStringVec (LineReader.readline r)
        val fields = Substring.fields (fn c => c = #" ") (Substring.full resp)
        val cmdMS = (Time.toReal (Timer.checkRealTimer timer)) * 1000.0
        val () = print ("CLI: command took " ^ Real.toString cmdMS ^ " ms\n")
      in
        map unquote fields
      end
end
