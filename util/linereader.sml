structure LineReader :> sig

  type reader
  val new: Socket.active INetSock.stream_sock * int -> reader

  val readline: reader -> Word8Vector.vector

end = struct

  structure W8V = Word8Vector
  structure W8VS = Word8VectorSlice

  val emptyVec = W8V.fromList nil

  type reader = Socket.active INetSock.stream_sock
              * int
              * (W8VS.slice list) ref

  fun new (sock, increment) = (sock, increment, ref nil)

  fun isNL (_, 0wx0A: Word8.word) = true
    | isNL (_, _) = false

  fun readline (reader as (sock, increment, ref nil)) = getMore reader
    | readline (reader as (sock, increment, buf as (ref (head::backlog)))) = (
        case W8VS.findi isNL head of
          NONE => getMore reader
        | SOME (pos, _) => (
              W8VS.concat (rev (W8VS.subslice (head, 0, SOME pos) :: backlog))
              before buf := [ W8VS.subslice (head, pos + 1, NONE) ]
            )
      )

  and getMore (sock, increment, slices) = let
        val vec' = Socket.recvVec (sock, increment)
      in
        if W8V.length vec' = 0 then emptyVec
        else (slices := (W8VS.full vec' :: !slices);
              readline (sock, increment, slices))
      end 

end
