structure ThreadBase :> THREAD_COMMON = struct

  type 'a t = 'a SMLofNJ.Cont.cont
  type 'a runnable = 'a
  val new = SMLofNJ.Cont.isolate
  fun prepare (thread, arg) = SMLofNJ.Cont.throw thread arg
  val switch = SMLofNJ.Cont.callcc

end
