functor Thread (
  structure T: THREAD_COMMON
  structure RC: REACTOR_CORE
  structure C: THREAD_CONFIG
) :> THREAD =
struct

  val () = SIGPIPE.ignore ()

  (* Threads are stored internally as:
   *
   * - A ref to the THREAD_COMMON value saved last time this thread was
   *   descheduled
   * - A bool indicating whether the thread is currently schedulable
   * - A unique (except in case of wraparound) int for tracing/debugging.
   *)

  datatype state = datatype ChiralCommon.state

  type thread_context = unit T.t
  type thread = (thread_context ref
                 * state ref
                 * int) ref

  fun get_id (ref (_, _, id)) = id
  fun get_state (ref (_, ref state, _)) = state

  exception NotRunning
  exception BadState of state

  val next_id = ref 0

  (* Introspection support
   *
   * For runtime monitoring and debugging, we keep track of all existing
   * threads in a map. Weak references are used to ensure that the map doesn't
   * keep threads alive forever.
   *)

  structure IM = IntBinaryMap

  val threads : thread Weak.t IM.map ref = ref IM.empty

  fun gc () = threads := IM.filter
                           (Option.isSome o Weak.get)
                           (!threads)

  fun get_thread id = (
      gc ();
      Option.mapPartial Weak.get (IM.find (!threads, id))
    )

  fun get_threads () = (
      gc ();
      List.mapPartial Weak.get (IM.listItems (!threads))
    )

  (* Sleeping threads
   *
   * We need to store a set of sleeping threads, and efficiently be able to:
   * - Find the next thread scheduled to wake up
   * - Insert a thread to wake up at a specified time in the future
   *
   * Therefore, we use a priority queue; the basis library provides an
   * implementation for us.
   *)

  val invert_order = fn GREATER => LESS
                      | EQUAL => EQUAL
                      | LESS => GREATER

  structure PQ = LeftPriorityQFn (type item = thread * Time.time
                                  type priority = Time.time
                                  val priority : item -> priority = #2
                                  val compare = invert_order o Time.compare)

  val runnable_threads : thread list ref = ref nil

  val rcstate : thread RC.state = RC.init ()
  val sleepq = ref PQ.empty

  val base_thread : thread_context option ref = ref NONE
  val current_thread : thread option ref = ref NONE

  fun get v = case !v of NONE => raise NotRunning | SOME t => t

  fun bail () = (
        print "Reactor: no runnable or blocked threads; exiting.\n";
        ( T.prepare (get base_thread, ())
          before (base_thread := NONE; current_thread := NONE) )
      )

  fun t2s (_, id) = Int.toString id

  val thtrace = C.trace C.THREAD
  val sctrace = C.trace C.SCHEDULE
  val etrace = C.trace C.ERROR

  (* val schedule: unit -> thread
   *
   * Find the next available thread and prepare to switch to it. This returns
   * a unit T.runnable, suitable for calling with T.switch. 
   *
   * If there are runnable threads, this removes the first thread from the
   * runqueue and updates current_thread to point to it.
   *
   * Otherwise, it checks the sleep queue for any threads whose timeout may
   * have expired; failing that, the reactor core will be invoked to wait on
   * blocked sockets. Then, runnable_threads is updated and schedule recurses,
   * so the only codepath for scheduling a thread is via the first match in the
   * outer case statement.
   *)
  fun schedule () =
        case !runnable_threads of
          (cur as ref (ctx, state, id)) :: rest => (
                sctrace (fn () => "Using next runnable: " ^ Int.toString id);
                runnable_threads := rest;
                current_thread := SOME cur;
                state := RUNNING;
                T.prepare (!ctx, ())
              )
        | nil => let
            val now = Time.now ()
            fun get_delayed (q, acc) = case PQ.next q of
                                         NONE => (q, rev acc)
                                       | SOME ((thr, waketime), q') =>
                                           if Time.< (waketime, now)
                                           then get_delayed (q', thr :: acc)
                                           else (q, rev acc)

            val sleep_time = case PQ.next (!sleepq) of
                                NONE => NONE
                              | SOME ((thr, t), _) => SOME (Time.- (t, now))

             val (new_sleepq, delayed_threads) = get_delayed (!sleepq, nil) 
             val () = sleepq := new_sleepq
           in
             case delayed_threads of
               t :: rest => (
                     sctrace (fn () => Int.toString (length rest + 1)
                                       ^ " delayed threads now runnable");
                     runnable_threads := (t :: rest);
                     schedule ()
                   )
             | nil => let
                   val () = sctrace (fn () => "Invoking reactor core")
                   val nt = RC.wait rcstate sleep_time
                 in
                   case nt of
                     NONE => bail () 
                   | SOME nil => (
                       sctrace (fn () => "Reactor returned early");
                       schedule ())
                   | SOME ts => (
                       sctrace (fn () => Int.toString (length ts)
                                         ^ " threads runnable from reactor");
                       runnable_threads := ts;
                       schedule ())
                 end
           end


  fun block (sock, cond) =
        T.switch (fn curctx => let
            val current as ref (ctx_ref, state_ref, _) = get current_thread
          in
            ctx_ref := curctx;
            state_ref := BLOCKED cond;
            RC.add_sock rcstate (current, cond, Socket.sockDesc sock);
            schedule ()
            handle e => (etrace (fn () => "Block prepare died with "
                                 ^ General.exnMessage e); raise e)
          end)

  fun sleep time = 
        T.switch (fn curctx => let
            val waketime = Time.+ (Time.now (), time)
            val current as ref (ctx_ref, state_ref, _) = get current_thread
          in
            ctx_ref := curctx;
            state_ref := SLEEPING waketime;
            sleepq := PQ.insert ((current, waketime), !sleepq);
            schedule ()
            handle e => (etrace (fn () => "Sleep prepare died with "
                                 ^ General.exnMessage e); raise e)
          end)

  fun new thrfun = let
        val id = !next_id
        val state = ref RUNNABLE
        fun thrfun' () = (
              (
                (thrfun ();
                 state := FINISHED;
                 thtrace (fn () => "Thread " ^ Int.toString id ^ " finished"))
               handle e => (
                 state := FAILED e;
                 thtrace (fn () => "Thread " ^ Int.toString id
                                 ^ " died with " ^ General.exnMessage e))
              );
              T.switch (fn _ => schedule ())
            )
        val thread = ref (ref (T.new thrfun'), state, id)
      in
        gc ();
        threads := IM.insert (!threads, id, Weak.new thread);
        next_id := id + 1;
        runnable_threads := thread :: (!runnable_threads);
        thread
      end

  fun self () = get current_thread

  fun kill _ = raise Fail "unimplementedx"

  fun run () =
        T.switch (fn curctx => (
          base_thread := SOME curctx;
          schedule ()
        ))


  fun deschedule () =
        T.switch (fn curctx => let
            val current as ref (ctx_ref, sch_ref, _) = get current_thread
          in
            ctx_ref := curctx;
            sch_ref := DESCHEDULED "deschedule";
            schedule ()
            handle e => (etrace (fn () => "Deschedule prepare died with "
                                ^ General.exnMessage e ^ "\n"); raise e)
          end)

  fun make_runnable (thread as ref (ctx_ref, sch_ref as ref (BLOCKED _), id)) = (
          thtrace (fn () => "Making thread " ^ Int.toString id ^ " runnable");
          sch_ref := RUNNABLE;
          runnable_threads := thread :: (!runnable_threads);
          ()
        )
    | make_runnable (ref (_, ref state, _)) = raise BadState state

  fun wake (thread as ref (ctx_ref, sch_ref as ref (SLEEPING _), id)) = (
          thtrace (fn () => "Making thread " ^ Int.toString id ^ " runnable");
          sch_ref := RUNNABLE;
          runnable_threads := thread :: (!runnable_threads);
          ()
        )
    | wake (ref (_, ref state, _)) = raise BadState state

end
