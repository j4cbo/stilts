structure PrettyTimer :> sig

  type timer

  val start: unit -> timer
  val print: timer -> string

end = struct

  type timer = Timer.real_timer * Timer.cpu_timer

  fun start () = (Timer.startRealTimer (), Timer.startCPUTimer ())

  fun print (real, cpu) = let
        val realTime = Timer.checkRealTimer real
        val { usr, sys } = Timer.checkCPUTimer cpu
      in
          Real.toString ((Time.toReal realTime) * 1000.0) ^ " ms total, "
        ^ Real.toString ((Time.toReal usr) * 1000.0) ^ " ms user, "
        ^ Real.toString ((Time.toReal sys) * 1000.0) ^ " ms system"
      end

end
