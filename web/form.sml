structure Form : FORM = struct

  (* Types for FORM signature. *)

  structure Map = RedBlackMapFn (type ord_key = string
                                 val compare = String.compare)

  type form = string list Map.map


  (* val parseVars: string -> (string * string) list
   *
   * Parse out a query string (key=value&key=value&...) into a set of pairs
   * of (key, value). All values are URL-decoded.
   *) 
  fun parseVars qstring =
      List.mapPartial (fn field =>
        let
          val (k, v) = Substring.splitl (fn c => c <> #"=") field
        in
          SOME (Substring.string k,
                WebUtil.urldecode (Substring.string (Substring.slice (v, 1, NONE))))
        end
        handle Subscript => NONE
      ) (Substring.fields (fn c => c = #"&") (Substring.full qstring))


  (* val add_value: (string * string) * form -> form
   *
   * Insert the given value at the beginning of the values for the given key.
   *)
  fun add_value ((k, v), form) = let
        val existing = case Map.find (form, k) of NONE => nil | SOME vs => vs
      in
        Map.insert (form, k, v::existing) 
      end


  (* val import: string -> form
   *
   * Builds a form from a query string.
   *)
  fun import str = foldl add_value Map.empty (parseVars str)


  (* val export: form -> string
   *
   * Exports a form into a query string format suitable for use by the "import"
   * function. May not properly preserve the ordering of keys with multiple
   * values.
   *)
  fun export form = String.concatWith "&" (
                      map (fn (k, vs) =>
                        String.concatWith "&" (
                          map (fn v => k ^ "=" ^ (WebUtil.urlencode v)) vs
                        )
                      ) (Map.listItemsi form)
                    )


  (* val load: Web.request -> form
   *
   * Parse all form variables out of a request.
   *
   * If the request is GET, only the query string is parsed; if it is a POST
   * of application/x-www-form-urlencoded data, that content is loaded as well.
   *)
  fun load (req: Web.request) = let
        val form = import (#query_string req)
        val content_type = case WebUtil.server_header "CONTENT_TYPE" req of
              SOME ct => SOME ct
            | NONE => WebUtil.http_header "HTTP_CONTENT_TYPE" req
        val postVars = case (#method req, content_type) of
                         ("POST", SOME "application/x-www-form-urlencoded") =>
                            parseVars (Byte.bytesToString (#content req ()))
                       | _ => nil
      in
        foldl add_value form postVars
      end


  (* val get: form -> string -> string option
   *
   * Return the most-recenty-specified value for the given key in the form,
   * if any.
   *)
  fun get f k = case Map.find (f, k) of NONE => NONE
                                      | SOME nil => NONE
                                      | SOME (v::vs) => SOME v


  (* val getAll: form -> string -> string list
   *
   * Return all values for the given key in the form, in the order specified.
   *)
  fun getAll f k = case Map.find (f, k) of NONE => []
                                         | SOME vs => rev vs


  (* val dump: form -> string
   *
   * Return a multiline string of all keys and values in the form which is
   * nominally human-readable and useful for debugging.
   *)
  fun dump form = String.concat (
                    map (fn (k, vs) =>
                        "- \"" ^ k ^ "\": "
                      ^ (String.concatWith ", " (map (fn v => "\""^v^"\"") vs))
                      ^ "\n"
                    ) (Map.listItemsi form)
                  ) 
end
