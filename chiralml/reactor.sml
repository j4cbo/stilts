functor Reactor(structure T: THREAD_COMMON
                structure RC: REACTOR_CORE) :> REACTOR = struct

  type thread = unit T.t

  type time_block_info = thread * Time.time

  val invert_order = fn GREATER => LESS
                      | EQUAL => EQUAL
                      | LESS => GREATER

  structure PQ = LeftPriorityQFn (type item = time_block_info
                                  type priority = Time.time
                                  val priority : item -> priority = #2
                                  val compare = invert_order o Time.compare)

  val runnable_threads : thread list ref = ref nil
  val base_thread : thread option ref = ref NONE

  val rcstate : thread RC.state = RC.init ()
  val sleepq = ref PQ.empty

  fun getBase () = case !base_thread of SOME t => t
                                      | NONE => raise Fail "no base thread"

  fun schedule () =
        case !runnable_threads of
          t :: rest => (runnable_threads := rest; t)
         | nil => let
(*
             val () = print "No runnable threads; checking sleep queue\n"
*)
             val now = Time.now ()
             fun get_delayed (sq, acc) =
                   case PQ.next sq of
                     NONE => (sq, rev acc)
                   | SOME ((thr, thrtime), sq') =>
                       if Time.< (thrtime, now)
                       then get_delayed (sq', thr :: acc)
                       else (sq, rev acc)

             val sleep_time = case PQ.next (!sleepq) of
                                 NONE => NONE
                               | SOME ((thr, t), _) => SOME (Time.- (t, now))

             val (new_sleepq, delayed_threads) = get_delayed (!sleepq, nil) 
             val () = sleepq := new_sleepq
           in
             case delayed_threads of
               t::rest => (runnable_threads := rest; t)
             | nil => let
(*
                   val () = print "Invoking reactor core\n"
*)
                   val nt = RC.wait rcstate sleep_time
                 in
                   case nt of nil => schedule ()
                            | t::rest => (runnable_threads := rest; t)
                 end
           end


  fun block (sock, cond) =
        T.switch (fn current => let
            val () = RC.add_sock rcstate (current, cond, Socket.sockDesc sock)
          in
            T.prepare (schedule (), ())
          end)

  fun sleep time = 
        T.switch (fn current => let
            val waketime = Time.+ (Time.now (), time)
            val () = sleepq := PQ.insert ((current, waketime), !sleepq)
          in
            T.prepare (schedule (), ())
          end)

  fun new thrfun = let val t = T.new thrfun
                    in runnable_threads := t :: (!runnable_threads);
                       t 
                    end

  fun kill _ = raise Fail "x"

  fun run () =
        T.switch (fn current => (
          base_thread := SOME current;
          T.prepare (schedule (), ())
        ))

end
