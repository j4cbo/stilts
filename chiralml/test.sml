structure R = Reactor(structure T = ThreadBase
                      structure RC = SelectReactorCore)

fun blarg str () = (print str; R.sleep (Time.fromMilliseconds 1000); blarg str ())
(*
val mudt = R.new (blarg "Mud\n")
val kipt = R.new (fn () => (R.sleep (Time.fromMilliseconds 500); blarg "Kip\n" ()))

*)
val bthr : R.thread option ref = ref NONE

fun afun () = (print "Arg\n";
               R.make_runnable (Option.valOf (!bthr))
                 handle R.AlreadyRunnable => (print "Already Blarg?\n"; ());
               R.deschedule ();
               afun ())

val athr = R.new afun

fun bfun () = (print "Blarg\n";
               R.make_runnable athr
                 handle R.AlreadyRunnable => (print "Already Arg?\n"; ());
               R.deschedule ();
               bfun ())

val () = bthr := SOME (R.new bfun);

(*

val _ = R.run ()



structure CS = ChiralSocketFn(R)
structure SU = ChiralSockUtil(CS)
structure LR = LineReader(CS.Socket)

  fun serveConn (conn, conn_addr) () = let
        val r = LR.new (conn, { increment = 1024, stripCR = true })
        fun lineLoop () = let
val () = print "T: reading line\n"
              val line = LR.readline r
(*
val () = print "T: read line; sleeping\n"
              val () = R.sleep (Time.fromMilliseconds 200)
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
          R.new (serveConn conn);
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

val listent = R.new (serve (INetSock.any 1234))
(*
val _ = R.run();
*)

*)
