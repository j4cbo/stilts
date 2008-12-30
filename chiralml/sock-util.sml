(* ChiralSockUtil
 *
 * Based on sock-util.sml from SML/NJ distribution.
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Various utility functions for programming with sockets.
 *)

functor ChiralSockUtil (S: CHIRAL_SOCKET) :> sig

    datatype port = PortNumber of int | ServName of string
    datatype hostname = HostName of string | HostAddr of NetHostDB.in_addr
    val scanAddr : (char, 'a) StringCvt.reader
          -> ({host : hostname, port : port option}, 'a) StringCvt.reader
    val addrFromString : string -> {host : hostname, port : port option} option
    exception BadAddr of string
    val resolveAddr : {host : hostname, port : port option}
          -> {host : string, addr : NetHostDB.in_addr, port : int option}
    type 'a stream_sock = ('a, S.Socket.active S.Socket.stream) S.Socket.sock
    val connectINetStrm : {addr : NetHostDB.in_addr, port : int}
          -> S.INetSock.inet stream_sock
    val recvVec : ('a stream_sock * int) -> Word8Vector.vector
    val recvStr : ('a stream_sock * int) -> string
    val sendVec : ('a stream_sock * Word8Vector.vector) -> unit
    val sendStr : ('a stream_sock * string) -> unit
    val sendArr : ('a stream_sock * Word8Array.array) -> unit
 
end = struct

    datatype port = datatype SockUtil.port
    datatype hostname = datatype SockUtil.hostname
    val scanAddr = SockUtil.scanAddr
    val addrFromString = SockUtil.addrFromString
    exception BadAddr = SockUtil.BadAddr
    val resolveAddr = SockUtil.resolveAddr
    type 'a stream_sock = ('a, S.Socket.active S.Socket.stream) S.Socket.sock

    (* establish a client-side connection to a INET domain stream socket *)
    fun connectINetStrm {addr, port} = let
	  val sock = S.INetSock.TCP.socket ()
	  in
	    S.Socket.connect (sock, S.INetSock.toAddr (addr, port));
	    sock
	  end

   (** If the server closes the connection, do we get 0 bytes or an error??? **)
    (* read exactly n bytes from a stream socket *)
    fun recvVec (sock, n) = let
	  fun get (0, data) = Word8Vector.concat(rev data)
	    | get (n, data) = let
		val v = S.Socket.recvVec (sock, n)
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
	  fun send i = S.Socket.sendVec (sock,
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
	  fun send i = S.Socket.sendArr (sock,
				       Word8ArraySlice.slice (arr, i, NONE))
	  fun put i = if (i < len)
		then put(i + send i)
		else ()
	  in
	    put 0
	  end

end
