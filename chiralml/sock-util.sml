(* ChiralSockUtil
 *
 * Based on sock-util.sml from SML/NJ distribution.
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Various utility functions for programming with sockets.
 *)

functor ChiralSockUtil (S: SOCKET
                            where type ('af, 'st) sock = ('af, 'st) Socket.sock
                              and type 'mode stream = 'mode Socket.stream
                              and type dgram = Socket.dgram
                              and type passive = Socket.passive
                              and type active = Socket.active
                              and type 'af sock_addr = 'af Socket.sock_addr
) :> SOCK_UTIL =
  struct

    datatype port = datatype SockUtil.port
    datatype hostname = datatype SockUtil.hostname
    val scanAddr = SockUtil.scanAddr
    val addrFromString = SockUtil.addrFromString
    exception BadAddr = SockUtil.BadAddr
    val resolveAddr = SockUtil.resolveAddr
    type 'a stream_sock = ('a, S.active S.stream) S.sock

    (* establish a client-side connection to a INET domain stream socket *)
    fun connectINetStrm {addr, port} = let
	  val sock = INetSock.TCP.socket ()
	  in
	    S.connect (sock, INetSock.toAddr(addr, port));
	    sock
	  end

   (** If the server closes the connection, do we get 0 bytes or an error??? **)
    (* read exactly n bytes from a stream socket *)
    fun recvVec (sock, n) = let
	  fun get (0, data) = Word8Vector.concat(rev data)
	    | get (n, data) = let
		val v = S.recvVec (sock, n)
		in
		  if (Word8Vector.length v = 0)
		    then raise OS.SysErr("closed socket", NONE)
		    else get (n - Word8Vector.length v, v::data)
		end
	  in
	    if (n < 0) then raise Size else get (n, [])
	  end

    fun recvStr arg = Byte.bytesToString (recvVec arg)

  (* send the complete contents of a vector *)
    fun sendVec (sock, vec) = let
	  val len = Word8Vector.length vec
	  fun send i = S.sendVec (sock,
				       Word8VectorSlice.slice (vec, i, NONE))
	  fun put i = if (i < len)
		then put(i + send i)
		else ()
	  in
	    put 0
	  end

    fun sendStr (sock, str) = sendVec (sock, Byte.stringToBytes str)

  (* send the complete contents of an array *)
    fun sendArr (sock, arr) = let
	  val len = Word8Array.length arr
	  fun send i = S.sendArr (sock,
				       Word8ArraySlice.slice (arr, i, NONE))
	  fun put i = if (i < len)
		then put(i + send i)
		else ()
	  in
	    put 0
	  end

  end;
