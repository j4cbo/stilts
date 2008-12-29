structure ThreadBase :> THREAD_COMMON = struct

  type 'a t = 'a MLton.Thread.t
  type 'a runnable = MLton.Thread.Runnable.t
  val new = MLton.Thread.new
  val prepare = MLton.Thread.prepare
  val switch = MLton.Thread.switch

end
