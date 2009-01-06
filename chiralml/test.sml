structure T = Thread (structure T = ThreadBase
                      structure RC = SelectReactorCore)

fun blarg str () = (print str; T.sleep (Time.fromMilliseconds 1000); blarg str ())
(*
val mudt = T.new (blarg "Mud\n")
val kipt = T.new (fn () => (T.sleep (Time.fromMilliseconds 500); blarg "Kip\n" ()))

*)
(*
val bthr : T.thread option ref = ref NONE

fun afun () = (print "Arg\n";
               T.make_runnable (Option.valOf (!bthr))
                 handle T.AlreadyRunnable => (print "Already Blarg?\n"; ());
               T.deschedule ();
               afun ())

val athr = T.new afun

fun bfun () = (print "Blarg\n";
               T.make_runnable athr
                 handle T.AlreadyRunnable => (print "Already Arg?\n"; ());
               T.deschedule ();
               bfun ())

val () = bthr := SOME (T.new bfun);

val _ = T.run ()
*)



structure CS = ChiralSocketFn(T)
structure SU = ChiralSockUtil(CS)
structure LR = LineReader(CS.Socket)

  fun serveConn (conn, conn_addr) () = let
        val r = LR.new (conn, { increment = 1024, stripCR = true })
        fun lineLoop () = let
val () = print "T: reading line\n"
              val line = LR.readline r
(*
val () = print "T: read line; sleeping\n"
              val () = T.sleep (Time.fromMilliseconds 200)
*)
val () = print "T: slept; sending\n"
              val () = SU.sendVec (conn, line)
val () = print "T: slept; sending newline\n"
              val () = SU.sendVec (conn, Byte.stringToBytes "\n")
                       handle e => (print "aw shiiiiiiit\n"; raise e)
val () = print "T: done\n"
            in
              lineLoop ()
            end
      in
        lineLoop ()
      end

  fun serve addr () =
    let
      val listener = CS.INetSock.TCP.socket ()

      val (server_host, server_port) = INetSock.fromAddr addr
      val sbind = (NetHostDB.toString server_host, server_port)

      fun accept () = let
          val conn = CS.Socket.accept listener
        in
          T.new (serveConn conn);
          accept ()
        end
    in
      (
        CS.Socket.Ctl.setREUSEADDR (listener, true);
        CS.Socket.bind (listener, addr);
        CS.Socket.listen (listener, 9);
        accept ()
      ) handle x => (CS.Socket.close listener; raise x)
    end

val listent = T.new (serve (INetSock.any 1234))
val _ = T.run();

