(* Stream Library *)
(* Author: Frank Pfenning, edited by Michael Erdmann *)



(* BASIC_STREAM defines the visible "core" of streams *)
signature BASIC_STREAM =
sig
  type 'a stream    (* abstract *)
  datatype 'a front = Empty | Cons of 'a * 'a stream

  (* Lazy stream construction and exposure *)
  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front

  (* Eager stream construction *)
  val empty : 'a stream
  val cons : 'a * 'a stream -> 'a stream
end;

structure BasicStream :> BASIC_STREAM =
struct
  datatype 'a stream = Stream of unit -> 'a front
  and 'a front = Empty | Cons of 'a * 'a stream

  fun delay (d) = Stream(d)
  fun expose (Stream(d)) = d ()

  val empty = Stream (fn () => Empty)
  fun cons (x, s) = Stream (fn () => Cons (x, s))
end;




(* STREAM extends BASIC_STREAM by operations         *)
(* definable without reference to the implementation *)
signature STREAM =
sig
  include BASIC_STREAM

  exception EmptyStream
  val null : 'a stream -> bool
  val hd : 'a stream -> 'a
  val tl : 'a stream -> 'a stream

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val filter : ('a -> bool) -> 'a stream -> 'a stream
  val exists : ('a -> bool) -> 'a stream -> bool

  val take : 'a stream * int -> 'a list
  val drop : 'a stream * int -> 'a stream

  val tabulate : (int -> 'a) -> 'a stream

  val append : 'a stream * 'a stream -> 'a stream

end;

functor Stream (structure BasicStream : BASIC_STREAM) :> STREAM =
struct
  open BasicStream

  exception EmptyStream

  (* functions null, hd, tl, map, filter, exists, take, drop     *)
  (* parallel the functions in the List structure                *)
  (* null, hd, and tl are not recommended, since they re-compute *)
  (* unless the stream is memoizing                              *)

  fun null (s) = null' (expose s)
  and null' (Empty) = true
    | null' (Cons _) = false

  fun hd (s) = hd' (expose s)
  and hd' (Empty) = raise EmptyStream
    | hd' (Cons (x,s)) = x

  fun tl (s) = tl' (expose s)
  and tl' (Empty) = raise EmptyStream
    | tl' (Cons (x,s)) = s

  fun map f s = delay (fn () => map' f (expose s))
  and map' f (Empty) = Empty
    | map' f (Cons(x,s)) = Cons (f(x), map f s)

  fun filter p s = delay (fn () => filter' p (expose s))
  and filter' p (Empty) = Empty
    | filter' p (Cons(x,s)) =
        if p(x) then Cons (x, filter p s)
        else filter' p (expose s)

  fun exists p s = exists' p (expose s)
  and exists' p (Empty) = false
    | exists' p (Cons(x,s)) =
        p(x) orelse exists p s

  (* take (s,n) converts the first n elements of n to a list *)
  (* raises Subscript if n < 0 or n >= length(s) *)
  fun takePos (s, 0) = nil
    | takePos (s, n) = take' (expose s, n)
  and take' (Empty, _) = raise Subscript
    | take' (Cons(x,s), n) = x::takePos(s, n-1)

  fun take (s,n) = if n < 0 then raise Subscript else takePos (s,n)

  fun dropPos (s, 0) = s
    | dropPos (s, n) = drop' (expose s, n)
  and drop' (Empty, _) = raise Subscript
    | drop' (Cons(x,s), n) = dropPos (s, n-1)

  fun drop (s,n) = if n < 0 then raise Subscript else dropPos (s,n)

  (*
  fun tabulate f = delay (fn () => tabulate' f)
  and tabulate' f = Cons (f(0), tabulate (fn i => f(i+1)))
  *)
  fun tabulate f =
      let
        fun tab n = delay (fn () => Cons (f(n), tab(n+1)))
      in
        tab 0
      end

  (* "Append" one stream to another.  Of course, if the first stream
      is infinite, we'll never actually get to the second stream. *)
  fun append (s1,s2) = delay (fn () => append' (expose s1, s2))
  and append' (Empty, s2) = expose s2
    | append' (Cons(x,s1), s2) = Cons(x, append (s1, s2))

end;

structure Stream :> STREAM =
  Stream (structure BasicStream = BasicStream);





(* Memoizing Streams *)

structure BasicMemoStream :> BASIC_STREAM =
struct

  datatype 'a stream = Stream of unit -> 'a front
  and 'a front = Empty | Cons of 'a * 'a stream

  fun expose (Stream (d)) = d ()
  fun delay (d) =
      let val memoCell = ref d          (* temporarily... *)
          fun memoFun () =              (* executed only once... *)
              let
                  val r = d ()
               in
                  (memoCell := (fn () => r); r)
              end
              handle exn => (memoCell := (fn () => raise exn) ; raise exn)
          val _ = (memoCell := memoFun)
      in
        Stream (fn () => !memoCell ())
      end

  (* next two don't memoize, since front is already evaluated *)
  val empty = Stream (fn () => Empty)
  fun cons (x, s) = Stream (fn () => Cons (x, s))
end;

structure MStream :> STREAM =
  Stream (structure BasicStream = BasicMemoStream);



