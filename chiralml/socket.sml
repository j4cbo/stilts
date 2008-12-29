functor ChiralSocket (R: REACTOR) :> SOCKET
                            where type ('af, 'st) sock = ('af, 'st) Socket.sock
                              and type 'mode stream = 'mode Socket.stream
                              and type dgram = Socket.dgram
                              and type passive = Socket.passive
                              and type active = Socket.active
                              and type 'af sock_addr = 'af Socket.sock_addr
= struct

  structure S = Socket
  structure C = ChiralCommon

  exception Unimplemented

  fun block (sock, typ, cont) = (R.block (sock, typ); cont ())

  fun wrapper (f, args, sock, typ) = case f args of
          SOME res => res
        | NONE => block (sock, typ, fn () => wrapper (f, args, sock, typ))

  fun wrapper_b (f, args, sock, typ) = case f args of
          true => ()
        | false => block (sock, typ, fn () => wrapper_b (f, args, sock, typ))

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

  fun accept s = wrapper (S.acceptNB, s, s, C.BLOCK_RD)

  (* Connect doesn't use wrapper, because unlike all the other functions, we
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

  fun sendVec (a as (s,_)) = wrapper (S.sendVecNB, a, s, C.BLOCK_WR)
  fun sendArr (a as (s,_)) = wrapper (S.sendArrNB, a, s, C.BLOCK_WR)
  fun sendVec' (a as (s,_,_)) = wrapper (S.sendVecNB', a, s, C.BLOCK_WR)
  fun sendArr' (a as (s,_,_)) = wrapper (S.sendArrNB', a, s, C.BLOCK_WR)

  fun recvVec (a as (s,_)) = wrapper (S.recvVecNB, a, s, C.BLOCK_RD)
  fun recvArr (a as (s,_)) = wrapper (S.recvArrNB, a, s, C.BLOCK_RD)
  fun recvVec' (a as (s,_,_)) = wrapper (S.recvVecNB', a, s, C.BLOCK_RD)
  fun recvArr' (a as (s,_,_)) = wrapper (S.recvArrNB', a, s, C.BLOCK_RD)

  fun sendVecTo (a as (s,_,_)) = wrapper_b (S.sendVecToNB, a, s, C.BLOCK_WR)
  fun sendArrTo (a as (s,_,_)) = wrapper_b (S.sendArrToNB, a, s, C.BLOCK_WR)
  fun sendVecTo' (a as (s,_,_,_)) = wrapper_b (S.sendVecToNB', a, s, C.BLOCK_WR)
  fun sendArrTo' (a as (s,_,_,_)) = wrapper_b (S.sendArrToNB', a, s, C.BLOCK_WR)

  fun recvVecFrom (a as (s,_)) = wrapper (S.recvVecFromNB, a, s, C.BLOCK_RD)
  fun recvArrFrom (a as (s,_)) = wrapper (S.recvArrFromNB, a, s, C.BLOCK_RD)
  fun recvVecFrom' (a as (s,_,_)) = wrapper (S.recvVecFromNB', a, s, C.BLOCK_RD)
  fun recvArrFrom' (a as (s,_,_)) = wrapper (S.recvArrFromNB', a, s, C.BLOCK_RD)

end
