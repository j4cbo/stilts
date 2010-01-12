functor Thread (
  structure T: THREAD_COMMON
  structure RC: REACTOR_CORE
  structure C: THREAD_CONFIG
) :> THREAD =
struct

  val () = SIGPIPE.ignore ()

  exception NotRunning
  exception AlreadyRunnable
  exception Asleep
  exception NotAsleep

  (* Threads are stored internally as:
   *
   * - A ref to the THREAD_COMMON value saved last time this thread was
   *   descheduled
   * - A bool indicating whether the thread is currently schedulable
   * - A unique (except in case of wraparound) int for tracing/debugging.
   *)

  datatype scheduling_state = RUNNABLE | BLOCKED | SLEEPING

  type thread_context = unit T.t
  type thread = thread_context ref
              * scheduling_state ref
              * int

  type time_block_info = thread * Time.time

  val next_id = ref 0

  val invert_order = fn GREATER => LESS
                      | EQUAL => EQUAL
                      | LESS => GREATER

  structure PQ = LeftPriorityQFn (type item = time_block_info
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
          (ctx, dsc, id) :: rest => (
                sctrace (fn () => "Using next runnable: " ^ Int.toString id);
                runnable_threads := rest;
                current_thread := SOME (ctx, dsc, id);
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
            val current as (ctx_ref, _, _) = get current_thread
          in
            ctx_ref := curctx;
            RC.add_sock rcstate (current, cond, Socket.sockDesc sock);
            schedule ()
            handle e => (etrace (fn () => "Block prepare died with "
                                 ^ General.exnMessage e); raise e)
          end)

  fun sleep time = 
        T.switch (fn curctx => let
            val waketime = Time.+ (Time.now (), time)
            val current as (ctx_ref, _, _) = get current_thread
          in
            ctx_ref := curctx;
            sleepq := PQ.insert ((current, waketime), !sleepq);
            schedule ()
            handle e => (etrace (fn () => "Sleep prepare died with "
                                 ^ General.exnMessage e); raise e)
          end)

  fun new thrfun = let
        val id = !next_id
        fun thrfun' () = (
              thrfun ()
                handle e => thtrace (fn () => "Thread " ^ Int.toString id
                                     ^ " died with " ^ General.exnMessage e);
                thtrace (fn () => "Thread " ^ Int.toString id ^ " terminated");
              T.switch (fn _ => schedule ())
            )
        val thread = (ref (T.new thrfun'), ref RUNNABLE, id)
      in
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
            val current as (ctx_ref, sch_ref, _) = get current_thread
          in
            ctx_ref := curctx;
            sch_ref := BLOCKED;
            schedule ()
            handle e => (etrace (fn () => "Deschedule prepare died with "
                                ^ General.exnMessage e ^ "\n"); raise e)
          end)

  fun make_runnable (_, ref RUNNABLE, _) = raise AlreadyRunnable
    | make_runnable (_, ref SLEEPING, _) = raise Asleep
    | make_runnable (thread as (ctx_ref, sch_ref as ref BLOCKED, id)) = (
          thtrace (fn () => "Making thread " ^ Int.toString id ^ " runnable");
          sch_ref := RUNNABLE;
          runnable_threads := thread :: (!runnable_threads);
          ()
        )

  fun wake (_, ref BLOCKED, _) = raise NotRunning
    | wake (_, ref RUNNABLE, _) = raise AlreadyRunnable
    | wake (thread as (ctx_ref, sch_ref as ref SLEEPING, id)) = (
          thtrace (fn () => "Making thread " ^ Int.toString id ^ " runnable");
          sch_ref := RUNNABLE;
          runnable_threads := thread :: (!runnable_threads);
          ()
        )

end
