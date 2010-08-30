functor ChiralSocketFn (T: THREAD) :> CHIRAL_SOCKET
= struct

  structure Socket = struct

    structure S = Socket
    structure C = ChiralCommon

    exception Unimplemented

    fun block (sock, typ, cont) = (T.block (sock, typ); cont ())

    fun wrap (f, args, sock, typ) = let
          val res = f args
                    handle e => (print ("well that didn't go so well\n"
                                       ^ General.exnMessage e ^ "\n"); raise e)
        in
          case res of
            SOME res => res
          | NONE => block (sock, typ, fn () => wrap (f, args, sock, typ))
        end

    fun wrap_b (f, args, sock, typ) = case f args of
            true => ()
          | false => block (sock, typ, fn () => wrap_b (f, args, sock, typ))

    (* ----------- *)

    type ('af, 'sock_type) sock = ('af, 'sock_type) S.sock
    type 'af sock_addr = 'af S.sock_addr
    type dgram = S.dgram
    type 'mode stream = 'mode S.stream
    type passive = S.passive
    type active = S.active

    structure AF = S.AF
    structure SOCK = S.SOCK
    structure Ctl = S.Ctl

    val sameAddr = S.sameAddr
    val familyOfAddr = S.familyOfAddr

    val bind = S.bind
    val listen = S.listen
    val acceptNB = S.acceptNB
    val connectNB = S.connectNB

    fun accept s = wrap (S.acceptNB, s, s, C.BLOCK_RD)

    (* Connect doesn't use wrap, because unlike all the other functions, we
     * don't want to retry the connect once it's writeable; we just want to
     * return. *)

    fun connect (sock, dest) = case S.connectNB (sock, dest) of
            true => ()
          | false => block (sock, C.BLOCK_WR, fn () => ())

    val close = S.close

    datatype shutdown_mode = datatype S.shutdown_mode
    val shutdown = S.shutdown

    type sock_desc = S.sock_desc
    val sockDesc = S.sockDesc
    val sameDesc = S.sameDesc
    val select = S.select
    val ioDesc = S.ioDesc

    type out_flags = S.out_flags
    type in_flags = S.in_flags

    val sendVecNB = S.sendVecNB
    val sendArrNB = S.sendArrNB
    val sendVecNB' = S.sendVecNB'
    val sendArrNB' = S.sendArrNB'

    val recvVecNB = S.recvVecNB
    val recvArrNB = S.recvArrNB
    val recvVecNB' = S.recvVecNB'
    val recvArrNB' = S.recvArrNB'

    val sendVecToNB = S.sendVecToNB
    val sendArrToNB = S.sendArrToNB
    val sendVecToNB' = S.sendVecToNB'
    val sendArrToNB' = S.sendArrToNB'

    val recvVecFromNB = S.recvVecFromNB
    val recvArrFromNB = S.recvArrFromNB
    val recvVecFromNB' = S.recvVecFromNB'
    val recvArrFromNB' = S.recvArrFromNB'

    fun sendVec (a as (s,_)) = wrap (S.sendVecNB, a, s, C.BLOCK_WR)
    fun sendArr (a as (s,_)) = wrap (S.sendArrNB, a, s, C.BLOCK_WR)
    fun sendVec' (a as (s,_,_)) = wrap (S.sendVecNB', a, s, C.BLOCK_WR)
    fun sendArr' (a as (s,_,_)) = wrap (S.sendArrNB', a, s, C.BLOCK_WR)

    fun recvVec (a as (s,_)) = wrap (S.recvVecNB, a, s, C.BLOCK_RD)
    fun recvArr (a as (s,_)) = wrap (S.recvArrNB, a, s, C.BLOCK_RD)
    fun recvVec' (a as (s,_,_)) = wrap (S.recvVecNB', a, s, C.BLOCK_RD)
    fun recvArr' (a as (s,_,_)) = wrap (S.recvArrNB', a, s, C.BLOCK_RD)

    fun sendVecTo (a as (s,_,_)) = wrap_b (S.sendVecToNB, a, s, C.BLOCK_WR)
    fun sendArrTo (a as (s,_,_)) = wrap_b (S.sendArrToNB, a, s, C.BLOCK_WR)
    fun sendVecTo' (a as (s,_,_,_)) = wrap_b (S.sendVecToNB', a, s, C.BLOCK_WR)
    fun sendArrTo' (a as (s,_,_,_)) = wrap_b (S.sendArrToNB', a, s, C.BLOCK_WR)

    fun recvVecFrom (a as (s,_)) = wrap (S.recvVecFromNB, a, s, C.BLOCK_RD)
    fun recvArrFrom (a as (s,_)) = wrap (S.recvArrFromNB, a, s, C.BLOCK_RD)
    fun recvVecFrom' (a as (s,_,_)) = wrap (S.recvVecFromNB', a, s, C.BLOCK_RD)
    fun recvArrFrom' (a as (s,_,_)) = wrap (S.recvArrFromNB', a, s, C.BLOCK_RD)

  end

  structure INetSock = struct
    type inet = INetSock.inet
    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type dgram_sock = Socket.dgram sock
    type 'mode stream_sock = 'mode Socket.stream sock
    type sock_addr = inet Socket.sock_addr

    val inetAF = INetSock.inetAF
    val toAddr = INetSock.toAddr
    val fromAddr = INetSock.fromAddr
    val any = INetSock.any

    structure UDP = struct
      val socket = INetSock.UDP.socket
      val socket' = INetSock.UDP.socket'
    end

    structure TCP = struct
      val socket = INetSock.TCP.socket
      val socket' = INetSock.TCP.socket'
      val getNODELAY = INetSock.TCP.getNODELAY
      val setNODELAY = INetSock.TCP.setNODELAY
    end
  end
end

