functor LineReader (S: SOCKET) :> sig

  type reader
  val new: (INetSock.inet, S.active S.stream) S.sock * { increment: int,
                                                  stripCR: bool } -> reader

  val readline: reader -> Word8Vector.vector

  val readbytes: reader -> int -> Word8Vector.vector

end = struct

  structure W8V = Word8Vector
  structure W8VS = Word8VectorSlice

  val emptyVec = W8V.fromList nil

  type config = { increment: int,
                  stripCR: bool }

  type reader = (INetSock.inet, S.active S.stream) S.sock
              * config
              * (W8VS.slice list * int) ref

  fun new (sock, config) = (sock, config, ref (nil, 0))

  val cr = 0wx0D: Word8.word

  fun isNL (_, 0wx0A: Word8.word) = true
    | isNL (_, _) = false

  fun readline (reader as (_, _, ref (nil, _))) = getMore reader readline
    | readline (reader as (sock, { increment, stripCR },
                                 buf as (ref (head::backlog, len)))) = (
        case W8VS.findi isNL head of
          NONE => getMore reader readline
        | SOME (pos, _) => let
              val pos' = if stripCR andalso pos > 0
                         then (case W8VS.sub (head, pos - 1) of
                                 0wx0D => pos - 1
                               | _ => pos)
                         else pos

              val headslice = W8VS.subslice (head, 0, SOME pos')

              val rest = case (backlog, pos, stripCR) of
                (blh::rest, 0, true) => let
                    val blhl = W8VS.length blh
                    val blh' = if W8VS.sub (blh, blhl - 1) = cr
                               then W8VS.subslice (blh, 0, SOME (blhl - 1))
                               else blh 
                  in 
                    blh' :: rest
                  end
              | _ => backlog

              val newData = W8VS.subslice (head, pos + 1, NONE)
              val () = buf := (newData :: nil, W8VS.length newData)
            in
              W8VS.concat (rev (headslice :: rest))
            end
      )

  and readbytes (reader as (sock, _, buf as (ref (slices, dlen)))) len =
        if len > dlen then getMore reader (fn r => readbytes r len)
        else case slices of
               nil => emptyVec
             | head :: rest => let
                   val brkpt = W8VS.length head - (dlen - len)
                   val newData = W8VS.subslice (head, brkpt, NONE)
                   val () = buf := (newData :: nil, W8VS.length newData)
                 in
                   W8VS.concat (rev (W8VS.subslice (head, 0, SOME brkpt)::rest))
                 end

  and getMore (sock, config as { increment, stripCR }, sliceref) cont = let
        val (slices, len) = !sliceref
        val vec' = S.recvVec (sock, increment)
        val newLen = W8V.length vec'
      in
        if newLen = 0 then emptyVec
        else (sliceref := (W8VS.full vec' :: slices, len + newLen);
              cont (sock, config, sliceref))
      end 

end
