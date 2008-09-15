structure Command = struct

  structure Map = RedBlackMapFn (type ord_key = string
                                 val compare = String.compare)

  fun foldMap (str, map) = let
        val str' = Substring.full str
        val (ls, rs) = Substring.splitl (fn c => c <> #":") str'
      in
        Map.insert (map, Substring.string ls,
                         Substring.string (Substring.triml 1 rs))
      end

  val mapResp = foldl foldMap Map.empty

  fun mapMulti prefix list = let

        val prefix' = prefix ^ ":"

        fun split (nil, curChunk, chunks) = curChunk :: chunks
          | split (s::rest, curChunk, chunks) =
              if String.isPrefix prefix' s
              then split (rest, nil, (s :: curChunk) :: chunks)
              else split (rest, s :: curChunk, chunks)

        val (prologue, items) = case split (rev list, nil, nil) of
              (prologue :: items) => (prologue, items)
            | _ => raise Fail "unexpected result"

 
      in
        (prologue, map (foldl foldMap Map.empty) items)
      end

  val c = CLI.connect ("localhost", 9090)

  fun players () = let
        val resp = case CLI.command c [ "players", "0", "9999" ] of
                     ("players"::"0"::"9999"::rest) => rest
                   | _ => raise Fail "unexpected result" 
      in
        mapMulti "playerindex" resp
      end

  structure JSON = struct

    fun string s = "\"" ^ String.toString s ^ "\""

    fun list l = "[" ^ String.concatWith "," (map string l) ^ "]"

    fun object map = let
          fun process (k, v, acc) = (string k ^ ":" ^ string v) :: acc
        in
          "{" ^ String.concatWith "," (Map.foldli process nil map) ^ "}"
        end
  end

  fun status p = let
        val resp = case CLI.command c [ p, "status", "-", "1", "tags:asledity" ] of 
                     (_::"status"::"-"::"1"::"tags:asledity"::rest) => rest
                   | _ => raise Fail "unexpected result" 
        val (prologue, tracks) = mapMulti "playlist index" resp
      in
        "[" ^ JSON.object (mapResp prologue)
            ^ ",[" ^ String.concatWith "," (map JSON.object tracks) ^ "]]"
      end

  fun playlist p start len = let
        val resp = case CLI.command c [ p, "status", Int.toString start,
                                        Int.toString len, "tags:asledity" ] of 
                     (_::"status"::_::_::"tags:asledity"::rest) => rest
                   | _ => raise Fail "unexpected result" 
        val (prologue, tracks) = mapMulti "playlist index" resp
      in
        (prologue, tracks)
      end
end
