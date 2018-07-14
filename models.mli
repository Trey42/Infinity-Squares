type coordinate = int * int

(* Player has HP which is represented by hearts;
 * a heart is lost every time the player is hit by a projectile, falls off the screen,
 * or is hit by/touches an enemy.
 * Player has velocity which is determined by the direction they are moving
 * Player has a hit box.
 * All models have position on screen (X, Y).*)
(* [player] is an abstract type representing the status of a player object *)
type player

(* [get_hp] is the current HP/Hearts of the player *)
val get_hp: player -> int

(* [set_hp] sets the hp of the player  *)
val set_hp: player -> int -> player

(* [alive] returns true if the player's hp > 0, false otherwise. *)
val alive: player -> bool

(* [get_location] is the current position of the player on the screen,
 * represented by the coordinate of the player's bottom left corner. *)
val get_pl_loc: player -> coordinate

(* [set_pl_loc] sets the location of the player on the screen. *)
val set_pl_loc: player -> coordinate -> player

(* [get_pl_xv] is the current x velocity of the player *)
val get_pl_xv: player -> int

(* [set_pl_xv] sets the x velocity of the player *)
val set_pl_xv: player -> int -> player

(* [get_pl_yv] is the current y velocity of the player *)
val get_pl_yv: player -> int

(* [set_pl_yv] sets the y velocity of the player *)
val set_pl_yv: player -> int -> player

(* [get_pl_hitbox] returns a pair containing the length and width of the player's
 * hitbox. *)
val get_pl_hitbox: player -> (int) * (int)

(* [set_pl_hitbox] sets the hitbox of the player. *)
val set_pl_hitbox: player -> (int) * (int) -> player

(* [get_hitbox_active] returns true if the player's hitbox is currently active
 * and they can take damage, false otherwise. *)
val get_hitbox_active: player -> bool

(* [set_hitbox_active] sets whether or not the player's hitbox is active. *)
val set_hitbox_active: player -> bool -> player

(* A bullet is a projectile that the player and the boss in the game can "shoot".
 * if the player is hit by a bullet they take damage. If the final boss is hit by
 * a bullet from the player, they take damange. *)
type bullet = {
  bul_loc : coordinate; (*bottom left corner of bullet*)
  bul_x_vel : int;
  bul_y_vel : int;
  bul_hitbox : int * int; (*[(length, width)]*)
}

(* Different types of bullets shot by the player and boss, e.g. scattershot,
 * large scattershot, fast shot, etc.. The player only uses the [Right]
 * bulet type; all other bullet types are used by the boss. *)
type bullet_type = Red | Blue | Green | Left | Right | FastLeft | Orange |
                   BigLeft | FastRed | FastOrange

(* [get_bul_xv] returns the x velocity of this bullet*)
val get_bul_xv: bullet -> int

(* [set_bul_xv] sets the x velocity of this bullet. *)
val set_bul_xv: bullet -> int -> bullet

(* [get_bul_yv] returns the y velocity of this bullet*)
val get_bul_yv: bullet -> int

(* [set_bul_yv] sets the y velocity of this bullet. *)
val set_bul_yv: bullet -> int -> bullet

(* [get_bul_loc] returns the current position of this bullet*)
val get_bul_loc: bullet -> coordinate

(* [set_bul_loc] sets the location of this bullet*)
val set_bul_loc: bullet -> coordinate -> bullet

(* [get_bul_hitbox] returns a pair containing the length and width of the bullet's
 * hitbox .*)
val get_bul_hitbox: bullet -> int * int

(* [set_bul_hitbox] sets the hitbox of this bullet. *)
val set_bul_hitbox: bullet -> int * int -> bullet

(* Initial stats of player when the game starts.*)
val init_player: unit -> player

(* Initial stats of boss when the game starts.*)
val init_boss: coordinate -> player

(* [create_bullet loc bullet_type] returns a bullet object that started from
 *  loc and is of type bullet_type (Red/Green/ whatever...)
 * for example, to create a red bullet: let a = create_bullet (1, 1) Red  *)
val create_bullet: coordinate -> bullet_type -> bullet

(* Represents the current stage of the game and that stage's relative difficulty.*)
type game_stage = Easy | Medium | Hard | Insane

(* Returns the current stage of the game based on how much HP the boss has
 * remaining. The game becomes more difficult as the boss' health decreases.*)
val get_game_stage: int -> game_stage
