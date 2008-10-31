structure SearchFile :> sig 

  val init: string -> unit

  val search: string -> int list list

end = struct

  structure PIO = Posix.IO
  structure FS = Posix.FileSys

  structure W8V = Word8Vector
  structure W8VS = Word8VectorSlice

  type state = { file: FS.file_desc,
                 jt: Word8Vector.vector,
                 numSubs: int,
                 unpack: Word8Vector.vector * int -> LargeWord.word,
                 dataLength: int }

  val state : state option ref = ref NONE

  fun init filename = let

        val () = case !state of SOME _ => raise Fail "already opened"
                              | NONE => ()

        val fd = FS.openf (filename, FS.O_RDONLY, FS.O.flags nil)

        val prologue = PIO.readVec (fd, 24)

        val prefix = W8VS.vector (W8VS.slice (prologue, 0, SOME 8))
        val () = case Byte.bytesToString prefix of
                   "SearchDB" => () 
                 |  _ => raise Fail "invalid database file"

        val () = case W8V.sub (prologue, 8) of
                   0w3 => ()
                 | _ => raise Fail "invalid version"

        val unpack = case W8V.sub (prologue, 9) of
                       0w0 => PackWord32Little.subVec
                     | 0w1 => PackWord32Big.subVec
                     | _ => raise Fail "invalid database file"

        val numSubs = Word8.toInt (W8V.sub (prologue, 10))

        val jtLength = LargeWord.toInt (unpack (prologue, 3))
        val dataLength = LargeWord.toInt (unpack (prologue, 4))

        val jt = PIO.readVec (fd, jtLength) 

      in
        state := SOME { file = fd, jt = jt, numSubs = numSubs,
                        unpack = unpack, dataLength = dataLength }
      end

  fun trace f = print (f ())

  fun search query = let
        val { file, jt, numSubs, unpack, dataLength } =
                case !state of SOME s => s
                             | NONE => raise Fail "search before init"

        val query' = W8VS.full (Byte.stringToBytes query)

        fun searchJT start = let
              val jtStr = W8VS.slice (jt, start * 4 + 8, SOME 4)
            in
              case W8VS.collate Word8.compare (query', jtStr) of
                EQUAL => start
              | LESS => start
              | GREATER => searchJT (start + 2)
            end
            handle Subscript => start

        val jtOffset = searchJT 0

        val dsStart = LargeWord.toInt (unpack (jt, jtOffset + 1))
        val dsLength = LargeWord.toInt (unpack (jt, jtOffset + 5)) - dsStart
                       handle Subscript => dataLength - dsStart

        val () = trace (fn () => "Scanning " ^ Int.toString dsLength
                               ^ " bytes starting at " ^ Int.toString dsStart
                               ^ "\n")

        val _ = PIO.lseek (file,
                           Position.fromInt (dsStart + W8V.length jt + 24),
                           PIO.SEEK_SET)

        val chunks = PIO.readVec (file, dsLength)

        fun searchChunks start = let
              val chunkLen = LargeWord.toInt (unpack (chunks, start))
              val chStrLen = LargeWord.toInt (unpack (chunks, start + 1))
              val chStr = W8VS.slice (chunks, start * 4 + 8, SOME chStrLen)
            in
              case W8VS.collate Word8.compare (query', chStr) of
                LESS => NONE
              | EQUAL => SOME (start + 3 + chStrLen div 4)
              | _ => searchChunks (start + chunkLen div 4)
            end

        fun getData (0, _) = nil
          | getData (section, chunkOffset) = let
                val n = LargeWord.toInt (unpack (chunks, chunkOffset))
                fun get i = LargeWord.toInt (unpack (chunks, chunkOffset + i + 1))
              in
                List.tabulate (n, get)
                :: getData (section - 1, chunkOffset + n + 1)
              end

        val seg = searchChunks 0

      in
        case seg of NONE => List.tabulate (numSubs, fn _ => nil)
                  | SOME s => getData (numSubs, s)
      end
end

structure Search = struct

  fun union (x: int list, nil: int list) = x
    | union (nil, y) = y
    | union (x::xs, y::ys) = case Int.compare (x, y) of
                                    EQUAL => x::(union (xs, ys))
                                  | GREATER => y::(union (x::xs, ys))
                                  | LESS => x::(union (xs, y::ys))

  fun intersection (_: int list, nil: int list) = nil
    | intersection (nil: int list, _: int list) = nil
    | intersection (x::xs, y::ys) = case Int.compare (x, y) of
                                            EQUAL => x::(intersection (xs, ys))
                                          | GREATER => intersection (x::xs, ys)
                                          | LESS => intersection (xs, y::ys)

  fun unpack [ tr, trTAl, trTAr, trTAlAr, ar, al, alAr ] =
        { tracksTitle = tr, tracksTitleAlbum = trTAl,
          tracksTitleArtist = trTAr, tracksTitleAlbumArtist = trTAlAr,
          artists = ar, albums = al, albumsArtist = alAr }
    | unpack l = raise Fail ("unexpected result section count: "
^ Int.toString (length l))


  val stopwords = [ "THE", "EL", "LA", "LOS", "LAS" ]

  fun searchWords nil = (nil, nil, nil)
    | searchWords words = let 

        val searchResults = map (unpack o SearchFile.search) words

        (* First, search for artists *)
        val artistResults = map #artists searchResults
        val artists = foldl intersection (hd artistResults) (tl artistResults)

        (* Album search depends on artist results: *)
        val albumResults = case artists of
                             nil => map #albumsArtist searchResults
                           | _ => map #albums searchResults
        val albums = foldl intersection (hd albumResults) (tl albumResults)

        (* Track search *)
        val trackFold = case (artists, albums) of
	                  (nil, nil) => #tracksTitleAlbumArtist
                        | (nil, _) => #tracksTitleArtist
                        | (_, nil) => #tracksTitleAlbum
                        | (_, _) => #tracksTitle
        val trackResults = map trackFold searchResults
        val tracks = foldl intersection (hd trackResults) (tl trackResults)             
      in
        (artists, albums, tracks)
      end

  fun search str = let
        val timer = Timer.startRealTimer ()
        val strSplit = String.tokens (fn c => c = #" ") str
        val words = map (String.map Char.toUpper) strSplit
        fun notStop word = not (List.exists (fn w => w = word) stopwords)
        val words = List.filter notStop words
        val (artists, albums, tracks) = searchWords words
      in
        { artists = artists,
          albums = albums,
          tracks = tracks, 
          time = Timer.checkRealTimer timer }
      end

end
