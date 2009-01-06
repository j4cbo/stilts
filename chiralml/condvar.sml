functor CondVar(T: THREAD) :> CONDVAR = struct

  type t = T.thread Fifo.fifo ref

  fun new () = ref Fifo.empty

  fun wait cv = (cv := Fifo.enqueue (!cv, T.self ());
                 T.deschedule ())

  fun signal cv =
        case Fifo.next (!cv) of NONE => false
                              | SOME (t', cv') => (cv := cv'; 
                                                   T.make_runnable t';
                                                   true)

  fun broadcast cv =
        Fifo.foldl (fn (t, num) => (T.make_runnable t; num + 1)) 0 (!cv)
        before cv := Fifo.empty

end
