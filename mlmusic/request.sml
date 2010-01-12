structure Request = struct

  structure U = WebUtil

  fun getCookie req cname = let
        val cookieStr = case U.http_header "HTTP_COOKIE" req of SOME s => s
                                                              | NONE => ""
        fun isSep c = c = #" " orelse c = #";"
        val cookies = String.fields isSep cookieStr

        val cookie = List.find (String.isPrefix (cname ^ "=")) cookies
      in
        case cookie of
          SOME c => SOME (String.extract (c, size cname + 1, NONE))
        | NONE => NONE
      end

  fun get_player_info req = let
        val desiredPlayer = Option.map Form.unquote (getCookie req "SqueezeCenter-player")

        (* Go through the cached player list doing two things:
         * - Find the player that the user wants; if they didn't specify one,
         *   take the first one we find
         * - Assemble a list of { id, name } for all players
         *)
        fun plFolder (item : PlayerCache.player_item, (foundPlayer, acc)) = (
              case (desiredPlayer, foundPlayer) of
                  (SOME desired, SOME prev) => if (#id item = desired) then SOME item else SOME prev
                | (NONE, SOME prev) => SOME prev
                | _ => SOME item,
              { id = #id item, name = #name (!(#info item)) } :: acc
            )

        val (player, playerList) = Command.Map.foldl plFolder (NONE, nil) (!PlayerCache.player_cache)

        (* Add a 'cur' field to indicate whether the player is selected *)
        val players = map (fn { id, name } => {
              id = id, name = name,
              cur = case player of SOME p => (id = #id p) | NONE => false
            }) playerList

      in
        (players, player)
      end

  fun get_header req = let
        val (players, player) = get_player_info req
        val status = Option.map (#status o ! o #info) player
      in
        (players, status)
      end

end
