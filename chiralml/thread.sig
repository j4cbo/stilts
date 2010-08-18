structure ChiralCommon = struct

  datatype block_cond = BLOCK_RD | BLOCK_WR

  datatype state = RUNNING
                 | RUNNABLE
                 | DESCHEDULED of string
                 | BLOCKED of block_cond 
                 | SLEEPING of Time.time
                 | FINISHED
                 | FAILED of exn

  fun condToStr BLOCK_RD = "read"
    | condToStr BLOCK_WR = "write"

  fun stateToStr (RUNNABLE) = "runnable"
    | stateToStr (RUNNING) = "running"
    | stateToStr (DESCHEDULED str) = "descheduled on " ^ str
    | stateToStr (BLOCKED cond) = "blocked on socket " ^ condToStr cond
    | stateToStr (SLEEPING t) = "sleeping for " ^ Time.toString (Time.- (t, Time.now ())) ^ " s"
    | stateToStr (FINISHED) = "finished"
    | stateToStr (FAILED e) = "failed with " ^ General.exnMessage e

end

signature THREAD_COMMON = sig

  type 'a t
  type 'a runnable

  val new: ('a -> unit) -> 'a t
  val prepare: 'a t * 'a -> 'a runnable
  val switch: ('a t -> 'a runnable) -> 'a

end

signature THREAD = sig

  exception NotRunning
  exception BadState of ChiralCommon.state

  type thread

  val block: ('af, 't) Socket.sock * ChiralCommon.block_cond -> unit
  val sleep: Time.time -> unit
  val wake: thread -> unit

  val new: (unit -> unit) -> thread
  val kill: thread -> exn -> unit

  val deschedule: unit -> unit
  val make_runnable: thread -> unit
  val self: unit -> thread

  val run: unit -> unit

  val get_threads: unit -> thread list
  val get_thread: int -> thread option
  val get_id: thread -> int
  val get_state: thread -> ChiralCommon.state

end
