(* functor HTTPServerFn
 *
 * Chiral version of the HTTP server. This is distinct from the non-Chiral
 * version in supporting keep-alive; the single-threaed version can only handle
 * one request per connection.
 *)

functor HTTPServerFn (
    structure CS : CHIRAL_SOCKET
    structure T : THREAD
)
=
struct

  val server_name = "Stilts-HTTPd/0.1"

  structure Handler = HTTPHandlerFn(
    structure S = CS.Socket
    val can_keep_alive = true
    val server_name = server_name
  )

  type opts = CS.INetSock.sock_addr

  fun spawn_server addr application =
    let
      val listener = CS.INetSock.TCP.socket ()

      val (server_host, server_port) = CS.INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      val connServer = Handler.serve_conn (server_name, sbind, nil) application

      fun accept () = let
val () = print "accept\n"
            val conn = CS.Socket.accept listener
            val t = T.new (connServer conn)
          in
            accept ()
          end

      fun app () = (
        CS.Socket.Ctl.setREUSEADDR (listener, true);
        CS.Socket.bind (listener, addr);
        CS.Socket.listen (listener, 9);
        accept ()
      ) handle x => (CS.Socket.close listener; raise x)

    in
      T.new app
    end

end
