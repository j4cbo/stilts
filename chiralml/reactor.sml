functor Reactor(structure T: THREAD_COMMON
                structure RC: REACTOR_CORE) :> REACTOR = struct

  val () = SIGPIPE.ignore ()

  exception NotRunning
  exception AlreadyRunnable

  (* Threads are stored internally as:
   *
   * - A ref to the THREAD_COMMON value saved last time this thread was
   *   descheduled
   * - A bool indicating whether the thread is currently schedulable
   * - A unique (except in case of wraparound) int for tracing/debugging.
   *)
  type thread_context = unit T.t
  type thread = thread_context ref
              * bool ref
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
        print "Reactor: no runnable or blocked threads; exiting.";
        ( T.prepare (get base_thread, ())
          before (base_thread := NONE; current_thread := NONE) )
      )

  fun t2s (_, id) = Int.toString id

  fun trace f = print (f () ^ "\n")

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
                trace (fn () => "Schedule: using next runnable: "
                                 ^ Int.toString id);
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
                     trace (fn () => "Schedule: "
                                      ^ Int.toString (length rest + 1)
                                      ^ " delayed threads now runnable");
                     runnable_threads := (t :: rest);
                     schedule ()
                   )
             | nil => let
                   val () = trace (fn () => "Schedule: invoking reactor core")
                   val nt = RC.wait rcstate sleep_time
                 in
                   case nt of
                     NONE => bail () 
                   | SOME nil => (
                       trace (fn () => "Schedule: reactor returned early");
                       schedule ())
                   | SOME ts => (
                       trace (fn () => "Schedule: " ^ Int.toString (length ts)
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
            handle e => (print ("Block prepare died with "
                                ^ General.exnMessage e ^ "\n"); raise e)
          end)

  fun sleep time = 
        T.switch (fn curctx => let
            val waketime = Time.+ (Time.now (), time)
            val current as (ctx_ref, _, _) = get current_thread
          in
            ctx_ref := curctx;
            sleepq := PQ.insert ((current, waketime), !sleepq);
            schedule ()
            handle e => (print ("Sleep prepare died with "
                                ^ General.exnMessage e ^ "\n"); raise e)
          end)

  fun new thrfun = let
        fun thrfun' () = (
              thrfun ()
                handle e => print ("Thread died with "
                                   ^ General.exnMessage e ^ "\n");
              T.switch (fn _ => schedule ())
            )
        val id = !next_id
        val thread = (ref (T.new thrfun'), ref true, id)
      in
        next_id := id + 1;
        runnable_threads := thread :: (!runnable_threads);
        thread
      end

  fun self () = get current_thread

  fun kill _ = raise Fail "x"

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
            sch_ref := false;
            schedule ()
            handle e => (print ("Deschedule prepare died with "
                                ^ General.exnMessage e ^ "\n"); raise e)
          end)

  fun make_runnable (_, ref true, _) = raise AlreadyRunnable
    | make_runnable (thread as (ctx_ref, sch_ref as ref false, id)) = (
          trace (fn () => "Making thread " ^ Int.toString id ^ " runnable");
          sch_ref := true;
          runnable_threads := thread :: (!runnable_threads);
          ()
        )

end
