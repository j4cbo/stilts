structure R = Reactor(structure T = ThreadBase
                      structure RC = SelectReactorCore)

fun blarg str () = (print str; R.sleep (Time.fromMilliseconds 1000); blarg str ())
(*
val mudt = R.new (blarg "Mud\n")
val kipt = R.new (fn () => (R.sleep (Time.fromMilliseconds 500); blarg "Kip\n" ()))
*)
(*
val _ = R.run ()

*)

structure S = ChiralSocket(R)
structure SU = ChiralSockUtil(S)
structure LR = LineReader(S)


  fun serveConn (conn, conn_addr) () = let
        val r = LR.new (conn, { increment = 1024, stripCR = true })
        fun lineLoop () = let
              val line = LR.readline r
              val () = R.sleep (Time.fromMilliseconds 200)
              val () = SU.sendVec (conn, line)
              val () = SU.sendVec (conn, Byte.stringToBytes "\n")
            in
              lineLoop ()
            end
      in
        lineLoop ()
      end

  fun serve addr () =
    let
      val listener = INetSock.TCP.socket ()

      val (server_host, server_port) = INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      fun accept () = let
          val conn = S.accept listener
        in
          R.new (serveConn conn);
          accept ()
        end
    in
      (
        S.Ctl.setREUSEADDR (listener, true);
        S.bind (listener, addr);
        S.listen (listener, 9);
        accept ()
      ) handle x => (S.close listener; raise x)
    end

val listent = R.new (serve (INetSock.any 1234))

