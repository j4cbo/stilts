structure SCGIServer :> WEB_SERVER where type opts = INetSock.sock_addr = struct

  exception ProtocolError

  val callbacks : (unit -> unit) list ref = ref nil

  fun addCleanupCallback f = callbacks := (f :: !callbacks)

  (* val pairs: 'a list -> ('a * 'a) list 

     Combine a list of even length into pairs of adjacent elements:
       [ a, b, c, d, e, f ] ==> [ (a, b), (c, d), (e, f) ]

     A trailing element, if present, will be ignored.
  *)
  fun pairs nil = nil
    | pairs (_::nil) = nil
    | pairs (a::(b::r)) = (a, b)::(pairs r)


  fun serveConn application (conn, conn_addr) =
    let

      fun read_length (sock, max) = let
        val delim = Byte.charToByte #":"
        fun loop (acc, n) = let
              val c = Word8Vector.sub (Socket.recvVec (sock, 1), 0)
            in
              if c = delim
                 then rev acc
              else if n < max
                 then loop (c :: acc, n + 1)
              else raise ProtocolError 
            end
      in
        Byte.bytesToString (Word8Vector.fromList (loop (nil, 0)))
      end

      (* Parse request netstring *)
      val request_len = case Int.fromString (read_length (conn, 10)) of
                           SOME i => i
                         | NONE => raise ProtocolError
                         handle Overflow => raise ProtocolError

      (* Read the request and split fields *)
      val req_data = Word8VectorSlice.slice (
                       SockUtil.recvVec (conn, request_len + 1),
                       0,
                       SOME request_len
                     )
      val req_headers = pairs (String.fields (fn c => c = #"\000")
                                             (Byte.unpackStringVec req_data))

      (* Get the content length and prepare to read the body *)
      val (content_length, req_headers) = case req_headers of
                  (("CONTENT_LENGTH", n)::r) => (case (Int.fromString n) of
                                                   SOME i => (i, r)
                                                 | NONE => raise ProtocolError) 
                | _ => raise ProtocolError

      val content_cache : Word8Vector.vector option ref = ref NONE

      fun reader () =
            case !content_cache of
              SOME c => c
            | NONE => let
                        val c = SockUtil.recvVec (conn, content_length)
                      in
                        content_cache := SOME c;
                        c
                      end

      val request = CGI.make_request (req_headers, content_length, reader)

      (* GO GO GO! *)
      val response = CGI.make_response (application request)

      val () = SockUtil.sendVec (conn, response)
      val () = Socket.close conn

      val () = List.app (fn f => f ()) (!callbacks)
    in
      ()
    end
    handle ProtocolError => Socket.close conn
    handle x => (Socket.close conn; raise x)


  type opts = INetSock.sock_addr

  fun serve addr application =
    let
      val listener = INetSock.TCP.socket ()

      fun accept () = (
        serveConn application (Socket.accept listener);
        accept ()
      )
    in
      (
        Socket.Ctl.setREUSEADDR (listener, true);
        Socket.bind (listener, addr);
        Socket.listen (listener, 9);
        accept ()
      ) handle x => (Socket.close listener; raise x)
    end

end
