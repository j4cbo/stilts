signature THREAD_CONFIG = sig
  datatype tracetype = REACTOR | THREAD | SCHEDULE | ERROR
  val trace : tracetype -> (unit -> string) -> unit
end
