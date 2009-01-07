structure PrettyTimer :> sig

  type timer

  val start: unit -> timer
  val print: timer -> string

end = struct

  type timer = Timer.real_timer * Timer.cpu_timer

  fun start () = (Timer.startRealTimer (), Timer.startCPUTimer ())

  fun print (real, cpu) = let
        val realTime = Timer.checkRealTimer real
        val { nongc, gc } = Timer.checkCPUTimes cpu
        fun ms time = Real.toString ((Time.toReal time) * 1000.0)
      in
        String.concat [
          ms realTime, " ms total, ", ms (#usr nongc), "+", ms (#usr gc),
          " ms user, ", ms (#sys nongc), "+", ms (#sys gc), " ms system"
        ]
      end

end
