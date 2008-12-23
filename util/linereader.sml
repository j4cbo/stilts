structure LineReader :> sig

  type reader
  val new: Socket.active INetSock.stream_sock * int -> reader

  val readline: reader -> Word8VectorSlice.slice

end = struct

  structure W8V = Word8Vector
  structure W8VS = Word8VectorSlice

  val emptySlice = W8VS.full (W8V.fromList nil)

  type reader = Socket.active INetSock.stream_sock
              * int
              * W8VS.slice ref

  fun new (sock, increment) = (sock, increment, ref emptySlice)

  fun isNL (_, 0wx0A: Word8.word) = true
    | isNL (_, _) = false

  fun readline (sock, increment, slice) = (
        case W8VS.findi isNL (!slice) of
          NONE =>
            let
              val vec' = Socket.recvVec (sock, increment)
            in
              if W8V.length vec' = 0
              then emptySlice
              else (slice := W8VS.full (W8VS.concat [ !slice, W8VS.full vec' ]);
                    readline (sock, increment, slice))
            end
        | SOME (pos, _) => (
            W8VS.subslice (!slice, 0, SOME pos)
            before slice := W8VS.subslice (!slice, pos + 1, NONE)
          ))

end
