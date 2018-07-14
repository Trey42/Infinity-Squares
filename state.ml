open Models

type player = Models.player
type bullet = Models.bullet

type move = | Left
            | Right
            | Up
            | Down

type state = {
  boss_bullets_list: bullet list;
  player_bullets_list: bullet list;
  player_stats: player;
  boss_stats: player;
  game_status: int; (* -1 for not end, 0 for lose, 1 for win*)
  win_message: string;
  lose_message: string;
}


(* [init_state boss_loc] is the initial setting of the game state with the boss
 * start from boss_loc *)
let init_state boss_loc:state = {
  boss_bullets_list = [];
  player_bullets_list = [];
  player_stats = init_player ();
  boss_stats = init_boss boss_loc;
  game_status = -1;
  win_message = "YOU WIN THE GAME!";
  lose_message = "YOU LOSE THE GAME!";
}

(* [get_player_hp s] is the current hp of the player in state s*)
let get_player_hp (s:state) : int =
  get_hp (s.player_stats)

(* [get_boss_hp s] is the current hp of the boss in state s*)
let get_boss_hp (s:state) : int =
  get_hp (s.boss_stats)

(* [get_player_stats s] is the player stats of this state s*)
let get_player_stats (s:state) : player =
  s.player_stats

(* [set_player_stats s p] is the updated state s with new player stats p*)
let set_player_stats (s: state) (p: player): state =
  {s with player_stats = p}

(* [get_boss_stats s] is the boss stats of this state s*)
let get_boss_stats (s:state) : player =
  s.boss_stats

(* [set_boss_stats s p] is the updated state s with new boss stats b *)
let set_boss_stats (s: state) (b: player): state =
  {s with boss_stats = b}

(* [get_player_bullet_list s] is the player_bullet_list of state s *)
let get_player_bullets_list (s:state) : bullet list =
  s.player_bullets_list

(* [get_boss_bullet_list s] is the boss_bullet_list of state s *)
let get_boss_bullets_list (s:state) : bullet list =
  s.boss_bullets_list

(* [get_game_status s] should return -1, 0 or 1 to tell if the game is still
 * running or if player lost/win
 * -1 means game still running;
 * 0 means player lost; 1 means player win *)
let get_game_status (s:state) : int =
  s.game_status


let win_message st =
  if st.game_status = 1 then st.win_message
  else failwith "Incorrect Game Status to call win message"

let lose_message st =
  if st.game_status = 0 then st.lose_message
  else failwith "Incorrect Game Status to call lose message"

(* [border_control loc safe_loc border_x border_y ] is the position of x,
 * if it's out of border just
 *  keep it in by using safe_loc as alternative value *)
let border_control loc safe_loc border_x border_y =
  let (want_x, want_y) = loc in
  let (safe_x, safe_y) = safe_loc in
  let x =
    if (want_x < 0 || want_x > border_x) then safe_x else want_x in
  let y =
    if (want_y < 0 || want_y > border_y) then safe_y else want_y in
  (x, y)

(* [border_control_left loc safe_loc border_x border_y ] is the position of x,
 * if it's out of border just keep it in by using safe_loc as alternative value;
 * this function keeps the player in the left half of screen and always 1
 * distance from the border*)
let border_control_left loc hitbox safe_loc border_x border_y =
  let (l, w) = hitbox in
  let (want_x, want_y) = loc in
  let (safe_x, safe_y) = safe_loc in
  let x =
    if (want_x < 0) then 1
    else if (want_x > (border_x/2 - w)) then (min (border_x/2 - w - 1) safe_x)
    else want_x in
  let y =
    if (want_y < 0) then 1
    else if (want_y > (border_y - l)) then (min (border_y - l - 1) safe_y)
    else want_y in
  (x, y)

(* [border_control_right loc safe_loc border_x border_y ] is the position of x,
 * if it's out of border just keep it in by using safe_loc as alternative value
 * this function keeps the boss in the right half of screenand always 1
 * distance from the border*)
let border_control_right loc hitbox safe_loc border_x border_y =
  let (l, w) = hitbox in
  let (want_x, want_y) = loc in
  let (safe_x, safe_y) = safe_loc in
  let x =
    if (want_x < (border_x/2)) then border_x/2 + 1
    else if (want_x > border_x - w) then (min (border_x - w - 1) safe_x)
    else want_x in
  let y =
    if (want_y < 0) then 1 else
    if (want_y > border_y - l) then (min (border_y - l - 1) safe_y)
    else want_y
  in (x, y)

(* [boss_reached_border loc hitbox border_x border_y] returns true if boss reaches
 * its middle, upper, bottom or right border.*)
let boss_reached_border boss_loc boss_hitbox border_x border_y =
  let (x, y) = boss_loc in
  let (l, w) = boss_hitbox in
  if x = border_x/2 then true
  else if x + w = border_x then true
  else if y + l = border_y then true
  else if y = 0 then true
  else false


(* Moves the player model appropriately based on the input command
 * and the player's x and y velocity. The player cannot move off the edge of the
 * screen and must stay in left half of the screen. *)
let move_player (m : move) (p : player) border_x border_y=
  let original_loc = get_pl_loc p in
  match m with
  | Left -> set_pl_loc p (border_control_left (fst (get_pl_loc p) + (-1 * get_pl_xv p),
                                               snd (get_pl_loc p)) (get_pl_hitbox p) original_loc border_x border_y)
  | Right -> set_pl_loc p (border_control_left (fst (get_pl_loc p) + get_pl_xv p,
                                                snd (get_pl_loc p)) (get_pl_hitbox p) original_loc border_x border_y)
  | Up -> set_pl_loc p (border_control_left (fst (get_pl_loc p),
                                             snd (get_pl_loc p) + get_pl_yv p) (get_pl_hitbox p) original_loc border_x border_y)
  | Down -> set_pl_loc p (border_control_left (fst (get_pl_loc p),
                                               snd (get_pl_loc p) + (-1 * get_pl_yv p)) (get_pl_hitbox p) original_loc border_x border_y)

(* Moves the player model appropriately based on the input command
 * and the player's x and y velocity. The player cannot move off the edge of the
 * screen and must stay in right half of the screen. *)
let move_boss (m : move) (p : player) border_x border_y=
  let original_loc = get_pl_loc p in
  match m with
  | Left -> set_pl_loc p (border_control_right (fst (get_pl_loc p) + (-1 * get_pl_xv p),
                                                snd (get_pl_loc p)) (get_pl_hitbox p) original_loc border_x border_y)
  | Right -> set_pl_loc p (border_control_right (fst (get_pl_loc p) + get_pl_xv p,
                                                 snd (get_pl_loc p)) (get_pl_hitbox p) original_loc border_x border_y)
  | Up -> set_pl_loc p (border_control_right (fst (get_pl_loc p),
                                              snd (get_pl_loc p) + get_pl_yv p) (get_pl_hitbox p) original_loc border_x border_y)
  | Down -> set_pl_loc p (border_control_right (fst (get_pl_loc p),
                                                snd (get_pl_loc p) + (-1 * get_pl_yv p)) (get_pl_hitbox p) original_loc border_x border_y)

(* [detect_game_status s] should return -1, 0 or 1 to tell if the game is still
   running or if boss/player has reach <= 0 hp *)
let detect_game_status (s:state) =
  if not(alive (s.player_stats)) then 0
  else if not(alive (s.boss_stats)) then 1
  else -1

(* [out_of_screen loc border_x border_y] returns if coordinate loc is out of screen *)
let out_of_screen loc border_x border_y: bool =
  let (x, y) = loc in
  if (x < 0 || x > border_x) then true
  else if (y < 0 || x > border_x) then true
  else false

(* [player_fire_bullet_type s bt] is the updated state after the player fires a bullet;
 * add that bullet to bulletpool*)
let player_fire_bullet_type (s:state) (bt:bullet_type): state =
  let b = create_bullet (get_pl_loc s.player_stats) bt in
  let l = s.player_bullets_list in
  {s with player_bullets_list = (b::l)}

(* [boss_fire_bullet_type s bt] is the updated state after the boss fires a bullet;
 * add that bullet to bulletpool*)
let boss_fire_bullet_type (s:state) (bt:bullet_type) : state =
  let b = create_bullet (get_pl_loc s.boss_stats) bt in
  let l = s.boss_bullets_list in
  {s with boss_bullets_list = (b::l)}

(* [player_fire_bullet s] is the updated state after the player fires a bullet;
 * add that bullet to bulletpool*)
let player_fire_bullet (s:state) : state =
  player_fire_bullet_type s Right

(* [boss_fire_bullet s] is the updated state after the boss fires a bullet;
 * add that bullet to bulletpool*)
let boss_fire_bullet (s:state) : state =
  match get_game_stage (get_hp (s.boss_stats)) with
  | Easy -> boss_fire_bullet_type s Left
  | Medium -> (let s1 = boss_fire_bullet_type s Left in
               let s2 = boss_fire_bullet_type s1 Blue in
               boss_fire_bullet_type s2 Green)
  | Hard -> (let s1 = boss_fire_bullet_type s FastLeft in
             let s2 = boss_fire_bullet_type s1 Red in
             boss_fire_bullet_type s2 Orange)
  | Insane -> (let s1 = boss_fire_bullet_type s BigLeft in
               let s2 = boss_fire_bullet_type s1 FastRed in
               boss_fire_bullet_type s2 FastOrange)

(* [update_bullet b] is the updated bullet object. *)
let update_bullet (b:bullet) border_x border_y: bullet option=
  let loc = get_bul_loc b in
  let (posx, posy) = loc in
  if (out_of_screen loc border_x border_y)
  then None
  else Some {
      bul_loc = (posx + (get_bul_xv b), posy + (get_bul_yv b));
      bul_x_vel = get_bul_xv b;
      bul_y_vel = get_bul_yv b;
      bul_hitbox = get_bul_hitbox b;
    }

(* [between v x y] returns if v is between value x and y*)
let between v x y:bool =
  if (v <= y) && (v >= x) then true else false

(* [intersect x1 y1 l1 w1 x2 y2 l2 w2] returns if this two hitbox overlaps *)
let intersect x1 y1 l1 w1 x2 y2 l2 w2: bool =
  let r1 = between x1 x2 (x2+w2) in
  let r2 = between y1 y2 (y2+l2) in
  let r3 = between (x1+w1) x2 (x2+w2) in
  let r4 = between (y1+l1) y2 (y2+l2) in
  let r5 = between x2 x1 (x1+w1) in
  let r6 = between y2 y1 (y1+l1) in
  let r7 = between (x2+w2) x1 (x1+w1) in
  let r8 = between (y2+l2) y1 (y1+l1) in
  match (r1, r2, r3, r4, r5, r6, r7, r8) with
  | (true, true, _, _, _, _, _, _) -> true
  | (true, _, _, true, _, _, _, _) -> true
  | (_, true, true, _, _, _, _, _) -> true
  | (_, _, true, true, _, _, _, _) -> true
  | (_, _, _, _, true, true, _, _) -> true
  | (_, _, _, _, true, _, _, true) -> true
  | (_, _, _, _, _, true, true, _) -> true
  | (_, _, _, _, _, _, true, true) -> true
  | _ -> false

(* [hit b p] returns if this bullet hit this player *)
let hit (b:bullet) (p:player): bool =
  let (x1, y1) = get_bul_loc b in
  let (l1, w1) = get_bul_hitbox b in
  let (x2, y2) = get_pl_loc p in
  let (l2, w2) = get_pl_hitbox p in
  intersect x1 y1 l1 w1 x2 y2 l2 w2

(* [hit_detection bullets p] returns if bullets from this list hit this player,
  * so should work both on boss bullets -> player, or player bullets -> boss.
  * returns trus/false.*)
let rec hit_detection (bullets: bullet list) (p: player): bool =
  match bullets with
  | [] -> false
  | h::t -> if (hit h p) then true else hit_detection t p


(* [update_boss_bullets s border_x border_y] go through the boss bullet list,
 * move every bullet one step forward according to their x- y- velocity, and
 * remove the ones that are out of screen, or has hit the player *)
let rec update_boss_bullets s border_x border_y: bullet list =
  let bullet_list = s.boss_bullets_list in
  let pl = s.player_stats in
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t -> if (hit h pl) then (helper t acc) else
      (match (update_bullet h border_x border_y) with
        | None -> helper t acc
        | Some b -> helper t (b::acc)) in
  helper bullet_list []

(* [update_player_bullets s border_x border_y] go through the player bullet list,
 * move every bullet one step forward according to their x- y- velocity, and
 * remove the ones that are out of screen, or has hit the boss *)
let rec update_player_bullets s border_x border_y: bullet list =
  let bullet_list = s.player_bullets_list in
  let boss = s.boss_stats in
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t -> if (hit h boss) then (helper t acc) else
      (match (update_bullet h border_x border_y) with
        | None -> helper t acc
        | Some b -> helper t (b::acc)) in
  helper bullet_list []


let boss_direction = ref Down
let timer = ref 400

let rec new_boss_direction () =
  let i = Random.int 4 in
  match i with
  | 0 -> if !boss_direction = Left then new_boss_direction () else boss_direction := Left
  | 1 -> if !boss_direction = Right then new_boss_direction () else boss_direction := Right
  | 2 -> if !boss_direction = Up then new_boss_direction () else boss_direction := Up
  | 3 -> if !boss_direction = Down then new_boss_direction () else boss_direction := Down
  | _ -> failwith "Impossible"

(*[update_boss p] is the player object with updated position, velocity and HP*)
let update_boss player_bullets (b:player) border_x border_y: player =
  let updated_speed =
    (match get_game_stage (get_hp b) with
     | Insane -> (let xv = set_pl_xv b 2 in
                  set_pl_yv xv 2)
     | _ -> b) in
  let ns =
  (let hp = (get_hp updated_speed) in
  if (hit_detection player_bullets b)
  then set_hp updated_speed (hp - 1)
  else updated_speed) in
  timer := (!timer) - 1;
  let time_up = if !timer <= 0 then true else false in
  let hit_border = boss_reached_border (get_pl_loc ns) (get_pl_hitbox ns) border_x border_y in
  if hit_border
  then (new_boss_direction (); move_boss (!boss_direction) ns border_x border_y)
  else if time_up
  then let _ = (if get_game_stage (get_hp ns) = Insane then timer := 200 else timer := 400) in
  (new_boss_direction (); move_boss (!boss_direction) ns border_x border_y)
  else move_boss (!boss_direction) ns border_x border_y

(*[update_player p] is the player object with updated position, velocity and HP*)
let update_player boss_bullets (p:player):player =
  let hp = (get_hp p) in
  if (hit_detection boss_bullets p)
  then set_hp p (hp - 1)
  else p

let boss_fire_timer = ref 150

let boss_fire_occasional (s:state):state =
  boss_fire_timer := (!boss_fire_timer) - 1;
  let time_up = if !boss_fire_timer <= 0 then true else false in
  if time_up then
    let _ = boss_fire_timer := 150 in (boss_fire_bullet s)
  else
    s

(* [update_state s border_x border_y] is the updated state of s, game should continuously run
 *  this method *)
let update_state st border_x border_y: state =
  let game_status = detect_game_status st in
  let s = boss_fire_occasional st in
  if game_status = -1 then
    (* game still alive, update all the records of state *)
    let b = update_boss s.player_bullets_list s.boss_stats border_x border_y in
    let p = update_player s.boss_bullets_list s.player_stats in
    let b_bullets = update_boss_bullets s border_x border_y in
    let p_bullets = update_player_bullets s border_x border_y in
    {
      boss_bullets_list = b_bullets;
      player_bullets_list = p_bullets;
      player_stats = p;
      boss_stats = b;
      game_status = -1;
      win_message = s.win_message;
      lose_message = s.lose_message;
    }
  else if game_status = 1 then
    {s with game_status = 1}
    (* do what game end with win message *)
  else
    {s with game_status = 0}
    (* do what game end with lose message *)
