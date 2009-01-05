(* Chiral/ML, Copyright (c) 2008 Jacob Potter.
 *
 * signature REACTOR_CORE
 *
 * This is the interface for reactor backends, using a system-specific socket
 * polling primitive such as select(). The Reactor functor receives a suitable
 * reactor core inplementation as input.
 *
 * Since this is largely an OS-interface structure, it is imperative; the 
 * add_sock and wait functions take an 'a state and mutate it as necessary.
 *
 *) 
signature REACTOR_CORE = sig

  type 'a state

  val init: unit -> 'a state
  (*
   * Create a new, empty state with no sockets registered.
   *)

  val add_sock: 'a state -> 'a * ChiralCommon.block_cond * Socket.sock_desc
                -> unit
  (*
   * Register interest in a socket. The next call to 'wait' will include the
   * given socket in the list of sockets to be monitored. The socket stays
   * registered until its block condition has been triggered.
   *
   * A given socket should not be registered more than once with the same
   * wait condition.
   *)

  val wait: 'a state -> Time.time option -> 'a list option
  (*
   * Wait for socket activity or for a timeout.
   *
   * If the timeout is NONE, and no sockets are currently registered for event
   * handling, this will immediately return NONE (rather than waiting forever,
   * which would otherwise occur).
   *
   * Otherwise, the function blocks until either (a) activity occurs on a
   * socket which was registered with add_sock, or (b) the timeout passes (if
   * not NONE). Then, SOME alphalist is returned; the values passed back are
   * those which were originally given to add_sock for the corresponding
   * sockets.
   *
   * If the timeout passes with no socket activity, wait will return SOME nil.
   *)

end

