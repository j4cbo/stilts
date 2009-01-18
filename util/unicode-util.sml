(* Unicode manipulation utilities.
 *
 * This implements an algorithm for normalizing UTF8 input to uppercase
 * with no punctuation or multiple spaces.
 *)

structure UnicodeUtil = struct

  val utf8_uppercase = [
    ("a", "A"), ("b", "B"), ("c", "C"), ("d", "D"), ("e", "E"), ("f", "F"),
    ("g", "G"), ("h", "H"), ("i", "I"), ("j", "J"), ("k", "K"), ("l", "L"),
    ("m", "M"), ("n", "N"), ("o", "O"), ("p", "P"), ("q", "Q"), ("r", "R"),
    ("s", "S"), ("t", "T"), ("u", "U"), ("v", "V"), ("w", "W"), ("x", "X"),
    ("y", "Y"), ("z", "Z"),
    ("\195\128", "A"), ("\195\129", "A"), ("\195\130", "A"), ("\195\131", "A"),
    ("\195\132", "A"), ("\195\133", "A"), ("\195\159", "B"), ("\195\158", "B"),
    ("\195\135", "C"), ("\194\162", "C"), ("\195\144", "D"), ("\195\136", "E"),
    ("\195\137", "E"), ("\195\138", "E"), ("\195\139", "E"), ("\195\140", "I"),
    ("\195\141", "I"), ("\195\142", "I"), ("\195\143", "I"), ("\195\145", "N"),
    ("\195\146", "O"), ("\195\147", "O"), ("\195\148", "O"), ("\195\149", "O"),
    ("\195\150", "O"), ("\195\152", "O"), ("\195\153", "U"), ("\195\154", "U"),
    ("\195\155", "U"), ("\195\156", "U"), ("\195\151", "X"), ("\195\157", "Y"),
    ("\195\160", "A"), ("\195\161", "A"), ("\195\162", "A"), ("\195\163", "A"),
    ("\195\164", "A"), ("\195\165", "A"), ("\195\190", "B"), ("\195\167", "C"),
    ("\195\168", "E"), ("\195\169", "E"), ("\195\170", "E"), ("\195\171", "E"),
    ("\195\172", "I"), ("\195\173", "I"), ("\195\174", "I"), ("\195\175", "I"),
    ("\195\177", "N"), ("\195\178", "O"), ("\195\179", "O"), ("\195\180", "O"),
    ("\195\181", "O"), ("\195\182", "O"), ("\195\184", "O"), ("\195\185", "U"),
    ("\195\186", "U"), ("\195\187", "U"), ("\195\188", "U"), ("\195\191", "Y"),
    ("\195\189", "Y"), ("\195\176", "D"), ("\194\161", "!"), ("\194\176", "D"),
    ("\195\134", "AE"), ("\195\166", "AE"), ("\194\181", "MU")
  ]

  val strip_punctuation = [
    ("!", " "), ("\"", " "), ("#", " "), ("$", " "), ("%", " "), ("&", " "),
    ("'", " "), ("(", " "), (")", " "), ("*", " "), ("+", " "), (",", " "), 
    ("-", " "), (".", " "), ("/", " "), (":", " "), (";", " "), ("<", " "), 
    ("=", " "), (">", " "), ("?", " "), ("@", " "), ("[", " "), ("\\", " "), 
    ("]", " "), ("^", " "), ("_", " "), ("`", " "), ("{", " "), ("|", " "), 
    ("}", " "), ("~", " ")
  ] 

  structure CM = BinaryMapFn(type ord_key = char val compare = Char.compare)

  fun translate (mappings, collapse_spaces) = let
      datatype mapstate = MSTRING of string
                        | MREPLACE of mapstate CM.map
                        | MREMOVE

      fun add_mapping ((nil, _), _) = raise Fail "add_mapping nil"
        | add_mapping ((c::nil, ""), m) = CM.insert (m, c, MREMOVE)
        | add_mapping ((c::nil, value), m) = CM.insert (m, c, MSTRING value)
        | add_mapping ((c::rest, value), m) = let
              val repl = case CM.find (m, c) of
                    SOME (MREPLACE m) => m
                  | _ => CM.empty
            in
              CM.insert (m, c, MREPLACE (add_mapping ((rest, value), repl)))
          end

      fun add_string_mapping ((key, value), map) =
            add_mapping ((String.explode key, value), map)

      fun add_map (mapping, acc) = foldl add_string_mapping acc mapping
      val map = foldl add_map CM.empty mappings

      fun tr (char, (curmap, acc)) = (
            case CM.find (curmap, char) of
              NONE => (map, (String.str char) :: acc)
            | SOME MREMOVE => (map, acc)
            | SOME (MSTRING s) => (map, s :: acc)
            | SOME (MREPLACE m) => (m, acc)
          )

      fun rev_collapse (nil, " "::acc) = rev_collapse (nil, acc)
        | rev_collapse (nil, acc) = acc
        | rev_collapse (" "::s, " "::acc) = rev_collapse (s, " "::acc)
        | rev_collapse (" "::s, nil) = rev_collapse (s, nil)
        | rev_collapse (c::s, acc) = rev_collapse (s, c::acc)

      val rev_process = if collapse_spaces
                        then (fn l => rev_collapse (l, nil))
                        else rev
    in
      fn string => let
            val (_, outacc) = foldl tr (map, nil) (String.explode string)
          in
            String.concat (rev_process outacc)
          end
    end

  val uppercase_normalzie =
        translate ([ utf8_uppercase, strip_punctuation ], true)
end
