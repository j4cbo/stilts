structure ChiralCommon = struct

  datatype block_cond = BLOCK_RD | BLOCK_WR

end

signature THREAD_COMMON = sig

  type 'a t
  type 'a runnable

  val new: ('a -> unit) -> 'a t
  val prepare: 'a t * 'a -> 'a runnable
  val switch: ('a t -> 'a runnable) -> 'a

end

signature REACTOR_CORE = sig

  type 'a state

  val init: unit -> 'a state

  val add_sock: 'a state -> 'a * ChiralCommon.block_cond * Socket.sock_desc
                -> unit

  val wait: 'a state -> Time.time option -> 'a list

end

signature REACTOR = sig

  val block: ('af, 't) Socket.sock * ChiralCommon.block_cond -> unit
  val sleep: Time.time -> unit

  type thread

  val new: (unit -> unit) -> thread
  val kill: thread -> exn -> unit

  val run: unit -> unit

end
