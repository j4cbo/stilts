structure FastCGICommon :> sig
  val serveConn: Web.app
                 -> (('af, Socket.active Socket.stream) Socket.sock
                     * 'af Socket.sock_addr)
                 -> unit
end =
struct
  exception ProtocolError

  structure W8VS = Word8VectorSlice
  structure W8V = Word8Vector
  structure W32 = Word32

  type rectype = int

  structure R = struct
    val FCGI_BEGIN_REQUEST = 1
    val FCGI_ABORT_REQUEST = 2
    val FCGI_END_REQUEST = 3
    val FCGI_PARAMS = 4
    val FCGI_STDIN = 5
    val FCGI_STDOUT = 6
    val FCGI_STDERR = 7
    val FCGI_DATA = 8
    val FCGI_GET_VALUES = 9
    val FCGI_GET_VALUES_RESULT = 10
    val FCGI_UNKNOWN_TYPE = 11

    val FCGI_RESPONDER = 1

    val FCGI_REQUEST_COMPLETE = 0
    val FCGI_CANT_MPX_CONN = 1
    val FCGI_OVERLOADED = 2
    val FCGI_UNKNOWN_ROLE = 3

    val FCGI_KEEP_CONN : Word8.word = 0w1 
  end
  open R

  type fcgirec = { version: int,
                   rectype: rectype,
                   reqId: int,
                   content: W8V.vector }


  (* val recvRecord: conn -> fcgirec
   *
   * Receive a single record from the connection.
   *)
  fun recvRecord conn =
    let
      val base = SockUtil.recvVec (conn, 8)
      fun b i = Word8.toInt (Word8Vector.sub (base, i))
    in
      { version = b 0,
        rectype = b 1,
        reqId = 256 * (b 2) + (b 3),
        content = SockUtil.recvVec (conn, 256 * (b 4) + (b 5)) }
      before
        ignore (SockUtil.recvVec (conn, b 6))
    end


  (* val parseNum: W8VS.slice -> int * W8VS.slice
   *
   * Parse out a number in FastCGI's encoding, and return the remainder of the
   * input data.
   *) 
  fun parseNum slice =
    let
      val first = W8VS.sub (slice, 0)
      fun subVec v = Word32.fromLargeWord (PackWord32Big.subVec v)
    in
      if (Word8.andb (first, 0wx80)) = 0w0
      then (Word8.toInt first,
            W8VS.subslice (slice, 1, NONE))
      else (W32.toInt (W32.andb (0wx7FFFFFFF, subVec (W8VS.vector slice, 0))),
            W8VS.subslice (slice, 4, NONE))
    end


  (* val parsePairs: W8VS.slice -> (string * string) list
   *
   * Parse all name/value pairs from the input stream.
   *)
  fun parsePairs slice =
    let
      val (nameLength, s') = parseNum slice
      val (valueLength, s') = parseNum s'
    in
      (Byte.unpackStringVec (W8VS.subslice (s', 0, SOME nameLength)),
       Byte.unpackStringVec (W8VS.subslice (s', nameLength, SOME valueLength)))
     :: (parsePairs (W8VS.subslice (s', nameLength + valueLength, NONE)))
    end
    handle Subscript => nil


  (* val sendRecord: conn -> fcgirec -> unit
   *
   * Send the given record.
   *)
  fun sendRecord conn { version, rectype, reqId, content } = let
      val contentLen = W8V.length content
      val padding = case (contentLen mod 8) of 0 => 0 | i => 8 - i
      val dest = Word8Array.array (W8V.length content + padding + 8, 0w0)
    in
      Word8Array.update (dest, 0, Word8.fromInt version);
      Word8Array.update (dest, 1, Word8.fromInt rectype);
(*
      PackWord16Big.update (dest, 1, LargeWord.fromInt reqId);
      PackWord16Big.update (dest, 2, LargeWord.fromInt contentLen);
*)
      Word8Array.update (dest, 2, Word8.fromInt (reqId div 256));
      Word8Array.update (dest, 3, Word8.fromInt reqId);
      Word8Array.update (dest, 4, Word8.fromInt (contentLen div 256));
      Word8Array.update (dest, 5, Word8.fromInt contentLen);
      Word8Array.update (dest, 6, Word8.fromInt padding);
      Word8Array.copyVec { src = content, dst = dest, di = 8 };
      SockUtil.sendArr (conn, dest)
    end


  (* val encodePairs: (string * string) list -> Word8Vector.vector
   *
   * Encode name/value pairs.
   *)
  fun encodePairs pairs = let
          fun encodeNum i = if i < 128
                then W8V.tabulate (1, fn _ => Word8.fromInt i)
                else let
                    val a = Word8Array.array (4, 0w0)
                    val i' = LargeWord.orb (0wx80000000, LargeWord.fromInt i)
                  in
                    PackWord32Big.update (a, 0, i'); Word8Array.vector a
                  end

         fun encodePair (k, v) = [ encodeNum (size k),
                                   encodeNum (size v),
                                   Byte.stringToBytes k,
                                   Byte.stringToBytes v ]
      in
        W8V.concat (List.concat (map encodePair pairs))
      end


  (* val parseBeginRequest: Word8Vector.vector -> int * Word8.word
   *
   * Parse the 'role' and 'flags' fields from a FCGI_BEGIN_REQUEST record.
   *)
  fun parseBeginRequest v = ( 256 * Word8.toInt (W8V.sub (v, 0))
                               +    Word8.toInt (W8V.sub (v, 1)),
                              W8V.sub (v, 2) )
                            handle Subscript => raise ProtocolError 


  (* val encodeEndRequest: int * int * int -> fcgirec
   *
   * Build a FCGI_END_REQUEST record for the given request ID, app status,
   * and protocol status.
   *)
  fun encodeEndRequest (reqId, appStatus, protocolStatus) = let
      val arr = Word8Array.array (8, 0w0)
    in
      PackWord32Big.update (arr, 0, LargeWord.fromInt appStatus);
      Word8Array.update (arr, 4, Word8.fromInt protocolStatus);
      { version = 1,
        rectype = FCGI_END_REQUEST,
        reqId = reqId,
        content = Word8Array.vector arr }
    end


  (* val getRecord: conn -> int -> fcgirec
   *
   * Read a record of the given type from the stream.
   *
   * This will transparetly handle the FCGI_GET_VALUES record type. If the
   * received record is not of the correct type, then it will be ignored and
   * an error message printed. Additionally, this will generate the proper
   * rejection for additional FCGI_BEGIN_REQUEST records. 
   *)
  fun getRecord conn expected = let
      val record = recvRecord conn
      val rtype = #rectype record
    in
      if rtype = FCGI_GET_VALUES then
        let
           val responsePairs = List.mapPartial (fn
                 (k as "FCGI_MAX_CONNS", _) => SOME (k, "1")
               | (k as "FCGI_MAX_REQS", _) => SOME (k, "1")
               | (k as "FCGI_MPXS_CONNS", _) => SOME (k, "0")
               | _ => NONE
             ) (parsePairs (W8VS.full (#content record)))
         in
           sendRecord conn { version = 1,
                             rectype = FCGI_GET_VALUES_RESULT,
                             reqId = 0,
                             content = encodePairs responsePairs };
           getRecord conn expected
         end
      else if rtype = expected then
        record
      else if rtype = FCGI_BEGIN_REQUEST then
        (sendRecord conn (encodeEndRequest (#reqId record, 0,
                                            FCGI_CANT_MPX_CONN));
         getRecord conn expected )
      else
        (print ("FastCGI: Unexpected record type "
                    ^ (Int.toString rtype) ^ "\n");
         getRecord conn expected )
    end


  fun processRequest (app, conn, reqId) = let
      fun readStream conn expected acc = let
          val { content, ... } = getRecord conn expected
        in
          if W8V.length content = 0
          then W8V.concat (rev acc)
          else readStream conn expected (content::acc)
        end

      val params = readStream conn FCGI_PARAMS []
      val pairs = parsePairs (W8VS.full params)

      val stdin = readStream conn FCGI_STDIN []

      val request = CGI.make_request (pairs, W8V.length stdin, fn () => stdin)

      val response = CGI.make_response (app request)

      val _ = print ("sending: " ^ (Int.toString (W8V.length response)) ^ "\n");
      val _ = print (Byte.bytesToString response ^ "\n")

      val limit = 65535

      fun sendChunks start =
            if (start + limit) < (W8V.length response)
            then (sendRecord conn { version = 1,
                                     rectype = FCGI_STDOUT,
                                     reqId = reqId,
                                     content = W8VS.vector (W8VS.slice
                                              (response, start, SOME limit)) };
                  sendChunks (start + limit))
            else sendRecord conn { version = 1,
                                   rectype = FCGI_STDOUT,
                                   reqId = reqId,
                                   content = W8VS.vector (W8VS.slice
                                                     (response, start, NONE)) }

    in
      sendChunks 0;
      sendRecord conn { version = 1,
                        rectype = FCGI_STDOUT,
                        reqId = reqId,
                        content = Byte.stringToBytes "" };
      sendRecord conn (encodeEndRequest (reqId, 0, FCGI_REQUEST_COMPLETE));
      print "SFCGI: done\n"
    end

  fun serveConn app (conn, conn_addr) =
    let
      val { version, reqId, content, ... } = getRecord conn FCGI_BEGIN_REQUEST
      val (role, flags) = parseBeginRequest content
    in
      if role = FCGI_RESPONDER
      then (processRequest (app, conn, reqId);
            if Word8.andb (flags, FCGI_KEEP_CONN) = FCGI_KEEP_CONN
            then serveConn app (conn, conn_addr)
            else Socket.close conn)
      else (sendRecord conn (encodeEndRequest (reqId, 0, FCGI_UNKNOWN_ROLE));
            serveConn app (conn, conn_addr)
           )

    end
    handle ProtocolError => Socket.close conn
    handle x => (Socket.close conn; raise x)

end
