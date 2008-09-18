structure Index = struct

  structure Map = RedBlackMapFn (type ord_key = string
                                 val compare = String.compare)

  fun addToMap ((id, key), idx) =
      let
        fun add_key (tok, idx) = Map.insert (idx, tok,
              id :: (case Map.find (idx, tok) of NONE => nil | SOME vs => vs))

        val keySplit = String.tokens (fn c => c = #" ") key
      in
        foldl add_key idx keySplit
      end

  structure PW = PackWord32Little
  structure W8A = Word8Array

  fun mkPrologue (jumpTableLength, dataLength, numIndexes) = let
         val prologue = Word8Array.array (24, 0w0)
       in
         W8A.copyVec { src = Byte.stringToBytes "SearchDB",
                       dst = prologue, di = 0 };
         W8A.update (prologue, 8, 0w3);
         W8A.update (prologue, 9, if PW.isBigEndian then 0w1 else 0w0);
         W8A.update (prologue, 10, Word8.fromInt numIndexes);
         PW.update (prologue, 3, LargeWord.fromInt jumpTableLength);
         PW.update (prologue, 4, LargeWord.fromInt dataLength);
         W8A.vector prologue
       end

  fun mkJumpTableEntry (offset, key) = let
        val key' = if (size key) <= 4 then key
                                      else String.extract (key, 0, SOME 4)
        val record = Word8Array.array (8, 0w0)
      in
        W8A.copyVec { src = Byte.stringToBytes key', dst = record, di = 0 };
        PW.update (record, 1, LargeWord.fromInt offset);
        W8A.vector record
      end


  fun indexFolder (sql, (acc, level)) = let
        val rows = sql ()

        val () = print "Building index...\n"
        val idx = foldl addToMap Map.empty rows

        val () = print "Merging...\n"

        val acc' = Map.map (fn k => (SOME k, NONE)) acc
        val idx' = Map.map (fn k => (NONE, SOME k)) idx

        fun merge ((SOME k1, NONE), (NONE, SOME k2)) = (SOME k1, SOME k2)
          | merge _ = raise Fail "merge failure"

        fun mergeMap (SOME old, SOME new) = new :: old
          | mergeMap (SOME old, NONE) = nil :: old
          | mergeMap (NONE, SOME new) = new :: List.tabulate (level, fn _ =>nil)
          | mergeMap (NONE, NONE) = raise Fail "mergeMap failure"

        val acc' = Map.map mergeMap (Map.unionWith merge (acc', idx'))
      in
        (acc', level + 1)
      end


  fun index indexes file = let

        val (outMap, _) = foldl indexFolder (Map.empty, 0) indexes

        val () = print "Generating output...\n"

        val jtMod = Real.floor (Math.sqrt (Real.fromInt (Map.numItems outMap)))

        fun outputFold (key, idss, (index, outBytes, outRecords, outJT)) = let

              val keyLen = (size key) + 1
              val keyPaddedLen = if keyLen mod 4 = 0
                                 then keyLen
                                 else keyLen - (keyLen mod 4) + 4

              val idssCount = foldl (fn (l, acc) => acc + length l + 1) 0 idss

              val recordLength = 8 + (4 * idssCount) + keyPaddedLen

              val record = W8A.array (recordLength, 0w0)

              val () = PW.update (record, 0, LargeWord.fromInt recordLength)
              val () = PW.update (record, 1, LargeWord.fromInt (size key))

              val () = W8A.copyVec { src = Byte.stringToBytes key,
                                     dst = record, di = 8 }

              fun setIds (ids, index) = let
                    fun setId (id, index) = (
                          PW.update (record, index, LargeWord.fromInt id);
                          index + 1)
                  in
                    PW.update (record, index, LargeWord.fromInt (length ids));
                    foldr setId (index + 1) ids
                  end

              val _ = foldr setIds (keyPaddedLen div 4 + 2) idss

              val outJT' = if index mod jtMod = 0
                           then (mkJumpTableEntry (outBytes, key)) :: outJT
                           else outJT
            in
              (index + 1, outBytes + recordLength,
               (W8A.vector record) :: outRecords, outJT')
            end

          val (_, outBytes, outRecords, outJT) =
                Map.foldli outputFold (0, 0, nil, nil) outMap

          val () = print ("Done. " ^ Int.toString (length outRecords)
                        ^ " records, " ^ Int.toString outBytes
                        ^ " bytes.\nWriting output...\n"); 

          val outFile = BinIO.openOut file

          val prologue = mkPrologue (8 * length outJT, outBytes, length indexes)

          val () = BinIO.output (outFile, prologue)
          val () = app (fn r => BinIO.output (outFile, r)) (rev outJT)
          val () = app (fn r => BinIO.output (outFile, r)) (rev outRecords)
          val () = BinIO.closeOut outFile
      in
        ()
      end
  
 fun run () = let
       val () = DB.connect ()
       val out = index [ SQL.tracksTitle,
                         SQL.tracksTitleAlbum,
                         SQL.tracksTitleArtist,
                         SQL.tracksTitleAlbumArtist,
                         SQL.artists,
                         SQL.albums,
                         SQL.albumsArtist ] "db.idx"
    in
      ()
    end 

end
