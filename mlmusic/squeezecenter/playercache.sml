structure PlayerCache = struct

  val player_cache_update_interval = Time.fromSeconds 60

  structure M = Command.Map

  type player_info = {
         name: string,
         status: string list * string M.map list
  }

  type player_item = {
         id: string,
         info: player_info ref,
         thread: T.thread
  }

  val player_cache = ref (M.empty : player_item M.map)
  val player_cache_cond = CV.new ()
  val player_cache_updating = ref false

  (* Helper functions *)
  fun make_updater status_ref cond func () = (
        if !status_ref
        then ()
        else ( 
          status_ref := true;
          ignore (func ());
          status_ref := false;
          ignore (CV.signal cond)
        )
      )

  fun continuously updater interval = let
        fun loop () = (ignore (updater ()); T.sleep interval; loop ())
      in
        loop
      end

  (* Per-player cache *)

  fun new_player_thread id info =
      let
        val info = ref {
              name = case M.find (info, "name") of NONE => "?" | SOME name => name,
              status = (nil, nil)
            }

        val player_updating = ref false
        val player_cond = CV.new ()
        val force_update = make_updater player_updating player_cond (fn () => let
              val new_status = Command.statusRaw id
            in
              info := { name = #name (!info), status = new_status }
            end)

        val player_thread = continuously force_update (Time.fromSeconds 30)
      in
        { id = id, info = info, thread = T.new player_thread }
      end

  val player_cache_update = (
        make_updater player_cache_updating player_cache_cond (fn () =>
          let
              (* For each player, see if we know about it; if not, start a new thread. *)
              fun create_if_new (id, info) = (
                    case M.find (!player_cache, id) of
                       SOME t => t
                     | NONE => new_player_thread id info
                  )

              val (_, new_players) = Command.players ()

              (* Command.players gives us a list of players with their id somewhere in
                  the map; we need to turn that into a list of ids. *)
              val players_by_id = foldl (
                    fn (player, acc) => case M.find (player, "playerid") of
                                          SOME id => M.insert (acc, id, player)
                                        | NONE => acc
                  ) M.empty new_players
            in
              player_cache := M.mapi create_if_new players_by_id
            end)
      )

  val _ = T.new (continuously player_cache_update player_cache_update_interval)

  (* val make_cache: Time.time -> (unit -> 'a) -> { get: unit -> 'a,
                                                    background_update: unit -> unit }
 
     Make a cached version of an object. Always updates every max_age seconds. 
  *)

  fun 'a make_cache max_age (updater : unit -> 'a) = let
      val cache = ref (NONE : 'a option)
      val updating = ref false
      val update_cond = CV.new ()

      (* Update. *)
      fun update () =
            if !updating
            then ()
            else (
              updating := true;
              cache := SOME (updater ());
              updating := false;
              ignore (CV.signal update_cond)
            )

      (* Continuously update *)
      fun update_loop () = (
            update ();
            T.sleep max_age;
            update_loop ()
          )

      val update_thread = T.new update_loop

      fun poke () = if !updating then ()
                                 else T.wake update_thread

      fun get () = case !cache of SOME value => value
                                | NONE => (CV.wait update_cond; get ())
    in
      {
        get = get,
        background_update = poke
      }
    end

(*
  val player_cache = make_cache (Time.fromSeconds 60) Command.players
*)
end
