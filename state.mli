open Models

(* [state] represents the current state of the game and keeps track of bullets
 * shot by the boss and player that are currently on the screen; all player and
 * boss attributes; whether the player has won, lost, or the game is still running;
 * and the message for when the player wins/loses the game respectively.*)
type state

type player = Models.player
type bullet = Models.bullet

(* [move] represents the current player input, with [Left], [Right], [Up], and
 * [Down] corresponding to the movement of the player object in that direction
 * on the screen.*)

type move = | Left
            | Right
            | Up
            | Down

(* [init_state boss_loc] is the initial setting of the game state with
 * boss starting from boss_loc*)
val init_state: int * int -> state

(* [update_state] updates attributes of all models in the current
 * room approrpriately. *)
val update_state: state -> int -> int -> state

(* [move_player m p border_x border_y] moves the player model based on the
 * user input and border sizes. *)
val move_player: move -> player -> int -> int -> player

(* [win_message] returns the winning message *)
val win_message : state -> string

(* [lose_message] returns the message when player is out of HP*)
val lose_message : state -> string

(* [get_game_status s] should return -1, 0 or 1 to tell if the game is still
 * running or if boss/player has reach <= 0 hp;
 * -1 means game still running;
 * 0 means player lost; 1 means player win *)
val get_game_status: state -> int

(* [get_player_hp s] is the current hp of the player in state s*)
val get_player_hp: state -> int

(* [get_boss_hp s] is the current hp of the boss in state s*)
val get_boss_hp: state -> int

(* [get_player_stats s] is the player stats of this state s*)
val get_player_stats: state -> player

(* [get_boss_stats s] is the boss stats of this state s*)
val get_boss_stats: state -> player

(* [set_player_stats s p] is the updated state s with new player stats p*)
val set_player_stats: state -> player -> state

(* [set_boss_stats s p] is the updated state s with new player stats p*)
val set_boss_stats: state -> player -> state

(* [get_player_bullet_list s] is the player_bullet_list of state s *)
val get_player_bullets_list: state -> bullet list

(* [get_boss_stats s] is the boss stats of this state s*)
val get_boss_bullets_list: state -> bullet list

(* [player_fire_bullet s] is the updated state after a player fires a bullet
 * (init bullet) *)
val player_fire_bullet: state -> state

(* [boss_fire_bullet s] is the updated state after a boss fires a bullet
 * (init bullet) *)
val boss_fire_bullet: state -> state

(* NOT exposing the following functions, but have them here for testing
 * purpose. The following functions are for edge detection and bullet/player
 * or bullet/boss hit detection

val border_control: int * int -> int * int -> int -> int -> int * int
val border_control_left: int * int -> int * int -> int * int -> int -> int -> int * int
val border_control_right: int * int -> int * int ->  int * int -> int -> int -> int * int
val boss_reached_border: int * int -> int * int -> int -> int -> bool
(* [out_of_screen loc border_x border_y] returns if coordinate loc is out of screen *)
val out_of_screen: int * int -> int -> int -> bool
(* [between v x y] returns if v is between value x and y*)
val between: int -> int -> int -> bool
(* [intersect x1 y1 l1 w1 x2 y2 l2 w2] returns if this two hitbox overlaps *)
val intersect: int -> int -> int -> int -> int -> int -> int -> int -> bool

*)
