structure FastCGIServer :> WEB_SERVER where type opts = INetSock.sock_addr = struct

  structure FS = Posix.FileSys

  type opts = INetSock.sock_addr

  fun serve addr application =
    let
      val sock = case FS.ST.isSock (FS.fstat FS.stdin) of
            true => MLton.Socket.fdToSock FS.stdin
          | false => let
                       val listener = INetSock.TCP.socket ()
                       val () = Socket.Ctl.setREUSEADDR (listener, true);
                       val () = Socket.bind (listener, addr);
                     in
                       listener
                     end
      val () = Socket.listen (sock, 10);
      fun acceptLoop () = let
            val conn = Socket.accept sock
            val () = FastCGICommon.serveConn application conn
          in
            acceptLoop ()
          end
    in
      acceptLoop ()
      handle x => (Socket.close sock; raise x)
    end

end
