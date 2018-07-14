open Models
open State

(*Some graphics code is from "Developing Applications from Objective Caml", which was the textbook used to learning
  how the graphics module worked.*)
(*Idea for putting the wait_event in the ignore function comes from https://codereview.stackexchange.com/questions/121563/snake-game-ocaml.*)

(*updates game state after a direction is given*)
let play_move d s =
  move_player d (get_player_stats s) (Graphics.size_x ()) (Graphics.size_y ())

(*matches the user input with a set number of accepted movement direction and updates the player and state accordingly*)
let g_key s c =
  match c with
  |'w' -> play_move Up s
  |'s' -> play_move Down s
  |'a' ->  play_move Left s
  |'d' ->  play_move Right s
  |_ -> (get_player_stats s)

(*matches user input iwht the fire key and if the player fired called the appropriate function*)
let s_key s c =
  match c with
  |' ' -> player_fire_bullet s
  |_ -> s

(*the plyaer flashes red when it has 1 hp left*)
let blink = ref 0

(*redraws the player object*)
let draw_player s c =
    Graphics.set_color c;
    let x = (fst(get_pl_loc(get_player_stats s))) in
    let y = (snd(get_pl_loc(get_player_stats s))) in
    let l = (fst(get_pl_hitbox(get_player_stats s))) in
    let w = (snd(get_pl_hitbox(get_player_stats s))) in
    if (get_hp(get_player_stats s) = 1) then
      if(!blink = 0) then
        (Graphics.set_color Graphics.red;
         blink := 1;
        Graphics.fill_rect x y l w)
      else
        (blink := 0;
        Graphics.fill_rect x y l w)
    else
      Graphics.fill_rect x y l w

(*changes the boss's color based on the game stage*)
let boss_color s =
  let stage = get_game_stage(get_boss_hp s) in
  match stage with
  |Easy -> Graphics.cyan
  |Medium -> Graphics.green
  |Hard -> Graphics.rgb 255 165 0
  |Insane -> Graphics.red

(*redraws the boss object*)
let draw_boss s =
    Graphics.set_color (boss_color s);
    let x = (fst(get_pl_loc(get_boss_stats s))) in
    let y = (snd(get_pl_loc(get_boss_stats s))) in
    let l = (fst (get_pl_hitbox(get_boss_stats s))) in
    let w = (snd(get_pl_hitbox(get_boss_stats s))) in
    Graphics.fill_rect x y l w


(*initializes the graphics window for the game*)
let g_init s () =
  Graphics.open_graph " 1500 x 1000";
  draw_player s Graphics.black;
  draw_boss s

(*draws the bullets on the screen from the list of existing bullets*)
let rec draw_bullets lst =
  match lst with
  | [] -> ()
  | h :: t -> Graphics.fill_rect (fst(get_bul_loc h)) (snd(get_bul_loc h)) (fst(get_bul_hitbox h)) (snd(get_bul_hitbox h)); draw_bullets t

(*redraws the player, enemy and all the bullets with the updated state*)
let redraw s =
  Graphics.clear_graph ();
  draw_player s Graphics.black;
  draw_boss s;
  Graphics.set_color Graphics.black;
  draw_bullets (get_player_bullets_list s);
  Graphics.set_color  (boss_color s);
  draw_bullets (get_boss_bullets_list s);
  Graphics.set_color Graphics.blue;
  Graphics.moveto 50 875;
  Graphics.set_text_size 1000;
  Graphics.draw_string ("HP: " ^ (string_of_int(get_player_hp s)))

(*Main loop of the game, which checks the current state and either outputs
  a win message, a lose message, or keeps the game running until either of those
  conditions are met.*)
let rec go s f_key s_key=
  if ((get_game_status s) = 1) then
    (Graphics.moveto 500 550;
     Graphics.draw_string(win_message s);
     Graphics.moveto 500 500;
    Graphics.draw_string "Press q to quit or any other key to play again.";
    ending s f_key s_key)
  else if ((get_game_status s) = 0 ) then
    (Graphics.moveto 500 550;
     Graphics.draw_string(lose_message s);
     Graphics.moveto 500 500;
    Graphics.draw_string "Press q to quit or any other key to play again.";
    ending s f_key s_key )
  else
  let g = Graphics.wait_next_event [Graphics.Poll] in
  if g.Graphics.keypressed then ignore(Graphics.wait_next_event[Graphics.Key_pressed]);
  let np = f_key s g.Graphics.key in
  let ns = update_state (set_player_stats s np) (Graphics.size_x ()) (Graphics.size_y ())in
  let nb = s_key ns g.Graphics.key in
  redraw nb;
  go nb f_key s_key

(*once the game ends, waits for the player to either quit or restart*)
and ending s f_key s_key =
  let t = Graphics.wait_next_event [Graphics.Key_pressed] in
  match t.Graphics.key with
  |'q' -> exit 0
  | _ -> Graphics.clear_graph();
        let k = init_state (1250,500) in
        g_init k ();
        Graphics.moveto 400 400;
        Graphics.set_text_size 100;
        Graphics.draw_string "Press any key to start!";
        ignore(Graphics.wait_next_event[Graphics.Key_pressed]);
        go k f_key s_key

(*initializes the state, gui and begins the main loop*)
let main() =
  let s = init_state (1250,500) in
  g_init s ();
  Graphics.moveto 300 350;
  Graphics.set_text_size 100;
  Graphics.draw_string "Press any key to start!";
  ignore(Graphics.wait_next_event[Graphics.Key_pressed]);
  go s g_key s_key

(*runs the gui and game*)
let () = main()
