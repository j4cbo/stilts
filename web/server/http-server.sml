structure HTTPServer :> WEB_SERVER where type opts = INetSock.sock_addr = struct

  val server_name = "Stilts-HTTPd/0.1"

  structure Handler = HTTPHandlerFn(
    structure S = Socket
    val can_keep_alive = true
    val server_name = server_name
  )

  val addCleanupCallback = Handler.addCleanupCallback
  type opts = INetSock.sock_addr

  fun serve addr application =
    let
      val listener = INetSock.TCP.socket ()

      val (server_host, server_port) = INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      fun accept () = ((
        Handler.serve_conn (server_name, sbind, nil)
                           application
                           (Socket.accept listener);
        accept ()
      ) handle _ => accept ())
    in
      (
        Socket.Ctl.setREUSEADDR (listener, true);
        Socket.bind (listener, addr);
        Socket.listen (listener, 9);
        accept ()
      ) handle x => (Socket.close listener; raise x)
    end

end
