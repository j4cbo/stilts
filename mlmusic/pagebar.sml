structure PageBar :> sig

  type item = int * int * int * char

  val pagebar: Web.pathsec * (unit -> string list) -> item list option

end = struct

  type item = int * int * int * char

  (* val paginate: int -> string list -> (int * int * int * char) list option
   *
   * Given a list of items, calculate page boundaries.
   *
   * The first argument is the maximum size of each page. The function returns
   * a list of tuples of:
   *
   *   (page offset, page size, item offset, character)
   *
   * Each character link should lead to the slice of the input starting at
   * page offset of the given size; the item offset is the position of the
   * desired character within that list.
   *
   * If the entire list can fit in one page, returns NONE; otherwise, return
   * SOME list.
   *
   * XXX: This does not properly deal with Unicode.
   *)
  fun paginate pageMaxSize titles = let
        fun firstChar s = case s of "" => #" " | _ => String.sub (s, 0)

        (* First, aggregate the input into a list of (firstChar, count) *)

        fun aggregate (str, nil) = [ (firstChar str, 1) ]
          | aggregate (str, (char, num) :: rest) = let
                val c' = firstChar str
              in
                if char = c' then (char, num + 1) :: rest
                             else (c', 1) :: (char, num) :: rest
              end

        val startChars : (char * int) list = foldr aggregate nil titles

        (* Then, place the characters into pages no larger than pageMaxSize.
         * Since this uses foldr, the list of pages will be reversed, as will
         * the list of characters within each page.
         *)

        fun buildPages ((c, n), nil) = [ (n, [ (c, n) ]) ]
          | buildPages ((c, n), (pageSize, chars) :: rest) =
              if pageSize + n <= pageMaxSize
              then (pageSize + n, (c, n) :: chars) :: rest
              else (n, [ (c, n) ]) :: (pageSize, chars) :: rest

        val pages : (int * (char * int) list) list =
              (foldl buildPages nil startChars)

      in
 
        (* Only continue if we need more than one page. *)

        case pages of
          nil => NONE
        | _::nil => NONE
        | _::_ => SOME let

          (* For each page, calculate its offset from the beginning; also,
           * for each character, calculate its offset within its page.
           *)

          fun calculateCharacterOffsets ((char, num), (base, rest)) =
                (base + num, (char, base) :: rest)

          fun calculateOffsets ((size, cs), (offset, rest)) = let
                val (_, cs') = foldr calculateCharacterOffsets (0, nil) cs
              in
                (offset + size, (offset, size, cs') :: rest)
              end

          val (_, pages') = foldr calculateOffsets (0, nil) pages

          (* Flatten the list *)

          fun flatten nil = nil
            | flatten ((pageOffset, pageSize, nil)::rest) = flatten rest
            | flatten ((pageOffset, pageSize, (c, charOffset)::cs)::rest) =
                (pageOffset, pageSize, charOffset, c)
                :: (flatten ((pageOffset, pageSize, cs)::rest))
        in
          rev (flatten pages')
        end

      end


  structure Map = RedBlackMapFn (type ord_key = string list
                                 val compare = List.collate String.compare)

  val cache : item list option Map.map ref = ref Map.empty

  fun pagebar (key, f) = case Map.find (!cache, key) of
                           SOME v => v
                         | NONE => let
                                     val v' = paginate 200 (f ())
                                   in 
                                     cache := Map.insert (!cache, key, v');
                                     v'
                                   end

end
