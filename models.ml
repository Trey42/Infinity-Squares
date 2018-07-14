type coordinate = int * int

(* Represents the player and boss models in the game. *)
type player = {
  pl_hp : int;
  pl_loc : coordinate; (*bottom left corner of player*)
  pl_x_vel : int;
  pl_y_vel : int;
  pl_hitbox : int * int; (*[(length, width)]*)
  pl_hitbox_active : bool;
}

type bullet = {
  bul_loc : coordinate; (*bottom left corner of bullet*)
  bul_x_vel : int;
  bul_y_vel : int;
  bul_hitbox : int * int; (*[(length, width)]*)
}

type bullet_type =
  | Red
  | Blue
  | Green
  | Left
  | Right
  | FastLeft
  | Orange
  | BigLeft
  | FastRed
  | FastOrange

(* [understand bullet_type] is the (x_vel, y_vel, bullet length, bullet width)
 * information. just some random parameters to get you started. *)
let understand bullet_type =
  match bullet_type with
  | Red -> (-1, -1, 40, 40)
  | Blue -> (-1, 1, 20, 20)
  | Green -> (-1, -1, 20, 20)
  | Left -> (-1, 0, 20, 20)
  | Right -> (3, 0, 20, 20)
  | FastLeft -> (-2, 0, 20, 20)
  | Orange -> (-1, 1, 40, 40)
  | BigLeft -> (-1, 0, 50, 50)
  | FastRed -> (-2, -1, 40, 40)
  | FastOrange -> (-2, 1, 40, 40)

let get_hp pl =
  pl.pl_hp

let set_hp pl hp =
  {pl with pl_hp = hp}

let alive pl =
  if pl.pl_hp > 0 then true else false

let get_pl_loc pl =
  pl.pl_loc

let set_pl_loc pl loc =
  {pl with pl_loc = loc}

let get_pl_xv pl =
  pl.pl_x_vel

let set_pl_xv pl xv =
  {pl with pl_x_vel = xv}

let get_pl_yv pl =
  pl.pl_y_vel

let set_pl_yv pl yv =
  {pl with pl_y_vel = yv}

let get_pl_hitbox pl =
  pl.pl_hitbox

let set_pl_hitbox pl hb =
  {pl with pl_hitbox = hb}

let get_hitbox_active pl =
  pl.pl_hitbox_active

let set_hitbox_active pl b =
  {pl with pl_hitbox_active = b}

let get_bul_loc bul =
  bul.bul_loc

let set_bul_loc bul loc =
  {bul with bul_loc = loc}

let get_bul_xv bul =
  bul.bul_x_vel

let set_bul_xv bul xv =
  {bul with bul_x_vel = xv}

let get_bul_yv bul =
  bul.bul_y_vel

let set_bul_yv bul yv =
  {bul with bul_y_vel = yv}

let get_bul_hitbox bul =
  bul.bul_hitbox

let set_bul_hitbox bul hb =
  {bul with bul_hitbox = hb}

let init_player (): player= {
  pl_hp = 20;
  pl_loc = (30, 30);
  pl_x_vel = 30;
  pl_y_vel = 30;
  pl_hitbox = 30, 30;
  pl_hitbox_active = true;
}

let init_boss boss_loc: player= {
  pl_hp = 150;
  pl_loc = boss_loc;
  pl_x_vel = 1;
  pl_y_vel = 1;
  pl_hitbox = 100, 100;
  pl_hitbox_active = true;
}

let create_bullet loc bullet_type =
  let (x_vel, y_vel, bul_length, bul_width) = (understand bullet_type) in
  {
    bul_loc = loc;
    bul_x_vel = x_vel;
    bul_y_vel = y_vel;
    bul_hitbox = (bul_length, bul_width);
  }

type game_stage = Easy | Medium | Hard | Insane

let get_game_stage (boss_hp : int) =
  let hp = float boss_hp in
  let init_hp = float (get_hp (init_boss (0,0))) in
  let hp_frac = hp /. init_hp in
  if hp_frac >= 0.75 then Easy
  else if hp_frac < 0.75 && hp_frac >= 0.5 then Medium
  else if hp_frac < 0.5 && hp_frac >= 0.25 then Hard
  else Insane
