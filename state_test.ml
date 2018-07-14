open OUnit2
open Models
open State

let p = init_player ()
let boss_loc33 = (3, 3)
let b = init_boss (boss_loc33)
let s = init_state (boss_loc33)
let p_hp0 = (set_hp p 0)
let p_hp1 = (set_hp p 1)
let p_hp2 = (set_hp p 2)
let b_hp0 = (set_hp b 0)
let b_hp1 = (set_hp b 1)
let b_hp2 = (set_hp b 2)
let s1 = set_player_stats s p_hp1
let s2 = set_boss_stats s b_hp1
let s3 = set_player_stats s1 p_hp2
let s4 = set_boss_stats s2 b_hp2
let lose_state = update_state (set_player_stats s p_hp0) 10 10
let win_state = update_state (set_boss_stats s b_hp0) 10 10

let basic_get_set_tests =
  [
    "get_player_hp" >:: (fun _ -> assert_equal (get_player_hp s1) 1);
    "get_boss_hp" >:: (fun _ -> assert_equal (get_boss_hp s2) 1);
    "get_player" >:: (fun _ -> assert_equal (get_player_stats s2) p_hp1);
    "get_boss" >:: (fun _ -> assert_equal (get_boss_stats s2) b_hp1);

    "set_player_stats" >:: (fun _ -> assert_equal (get_player_stats s3) p_hp2);
    "set_boss_stats" >:: (fun _ -> assert_equal (get_boss_stats s4) b_hp2);

    "get_player_bullet_list_init" >:: (fun _ -> assert_equal
                                          (get_player_bullets_list s) []);
    "get_boss_bullet_list_init" >:: (fun _ -> assert_equal
                                        (get_boss_bullets_list s) []);
    "win_message" >:: (fun _ -> assert_equal
                          (win_message win_state) "YOU WIN THE GAME!");
    "lose_message" >:: (fun _ -> assert_equal
                          (lose_message lose_state) "YOU LOSE THE GAME!")
]

(* THE FOLLOWING TESTS ARE FOR edge detection functions and hit detections,
 * they all successfully passed. the functions tested are core functions for
 * edge detection and hit detections, which are not supposed to be exposed
 * in .mli, thus commenting them out after testing.

(* testing border detection functions *)
let border_x = 5
let border_y = 5
let border_x2 = 10
let border_y2 = 10
let hitbox11 = (1, 1)
let loc00 = (0, 0)
let loc01 = (0, 1)
let loc10 = (1, 0)
let loc11 = (1, 1)
let loc_neg1_0= (-1, 0)
let loc_0_neg1 = (0, -1)
let loc23 = (2, 3)
let loc33 = (3, 3)
let loc34 = (3, 4)
let loc35 = (3, 5)
let loc38 = (3, 8)
let loc43 = (4, 3)
let loc44 = (4, 4)
let loc45 = (4, 5)
let loc46 = (4, 6)
let loc48 = (4, 8)
let loc410 = (4, 10)
let loc54 = (5, 4)
let loc55 = (5, 5)
let loc56 = (5, 6)
let loc64 = (6, 4)
let loc65 = (6, 5)
let loc_7_neg1 = (7, -1)
let loc70 = (7, 0)
let loc71 = (7, 1)
let loc78 = (7, 8)
let loc79 = (7, 9)
let loc88 = (8, 8)
let loc810 = (8, 10)
let loc811 = (8, 11)
let loc97 = (9, 7)
let loc99 = (9, 9)
let loc910 = (9, 10)
let loc108 = (10, 8)
let loc1010 = (10, 10)
let loc118 = (11, 8)

let border_control_test =
  [
    "within_border" >:: (fun _ -> assert_equal (border_control loc34 loc33
                                                  border_x border_y) loc34);
    "return_to_safe_loc_too_high" >:: (fun _ -> assert_equal
                                            (border_control loc56 loc55 border_x
                                               border_y) loc55);
    "return_to_safe_loc_too_right" >:: (fun _ -> assert_equal
                                            (border_control loc65 loc55 border_x
                                               border_y) loc55);
    "return_to_safe_loc_too_left" >:: (fun _ -> assert_equal
                                           (border_control loc_neg1_0 loc00 border_x
                                              border_y) loc00);
    "return_to_safe_loc_too_low" >:: (fun _ -> assert_equal
                                           (border_control loc_0_neg1 loc00 border_x
                                              border_y) loc00)
  ]

let border_control_left_test =
  [
    "within_border" >:: (fun _ -> assert_equal
                            (border_control_left loc33 hitbox11 loc23 border_x2
                               border_y2) loc33);
    "only_in_left_half_screen" >:: (fun _ -> assert_equal
                            (border_control_left loc44 hitbox11 loc34 border_x2
                               border_y2) loc34);
    "force_reset_to_left_half" >:: (fun _ -> assert_equal
                                (border_control_left loc65 hitbox11 loc55 border_x2
                                   border_y2) loc35);
    "reset_from_bottom" >:: (fun _ -> assert_equal
                                    (border_control_left loc_0_neg1 hitbox11 loc01
                                       border_x2 border_y2) loc01);
    "reset_from_top" >:: (fun _ -> assert_equal
                                (border_control_left loc410 hitbox11 loc48
                                   border_x2 border_y2) loc48);
    "reset_from_left" >:: (fun _ -> assert_equal
                              (border_control_left loc_neg1_0 hitbox11 loc10
                              border_x2 border_y2) loc10)
  ]

let border_control_right_test =
  [
    "within_border" >:: (fun _ -> assert_equal
                            (border_control_right loc65 hitbox11 loc64 border_x2
                               border_y2) loc65);
    "only_in_right_half_screen" >:: (fun _ -> assert_equal
                                       (border_control_right loc44 hitbox11 loc34 border_x2
                                          border_y2) loc34);
    "force_reset_to_left_half" >:: (fun _ -> assert_equal
                                       (border_control_right loc65 hitbox11 loc55 border_x2
                                          border_y2) loc35);
    "reset_from_bottom" >:: (fun _ -> assert_equal
                                (border_control_right loc_7_neg1 hitbox11 loc71
                                   border_x2 border_y2) loc71);
    "reset_from_top" >:: (fun _ -> assert_equal
                             (border_control_right loc810 hitbox11 loc88
                                border_x2 border_y2) loc88);
    "reset_from_right" >:: (fun _ -> assert_equal
                              (border_control_right loc108 hitbox11 loc88
                                 border_x2 border_y2) loc88)
  ]

let boss_reached_border_test =
  [
    "within_border" >:: (fun _ -> assert_equal
                            (boss_reached_border loc88 hitbox11
                               border_x2 border_y2) false);
    "reached_right" >:: (fun _ -> assert_equal
                        (boss_reached_border loc97 hitbox11
                           border_x2 border_y2) true);
    "reached_left" >:: (fun _ -> assert_equal
                         (boss_reached_border loc55 hitbox11
                            border_x2 border_y2) true);
    "reached_top" >:: (fun _ -> assert_equal
                        (boss_reached_border loc79 hitbox11
                           border_x2 border_y2) true);
    "reached_bottom" >:: (fun _ -> assert_equal
                         (boss_reached_border loc70 hitbox11
                         border_x2 border_y2) true)
  ]

let out_of_screen_test =
  [
    "within_border" >:: (fun _ -> assert_equal
                            (out_of_screen loc33 border_x2 border_y2) false);
    "within_border_edge_left" >:: (fun _ -> assert_equal
                                      (out_of_screen loc01 border_x2
                                         border_y2) false);
    "within_border_edge_right" >:: (fun _ -> assert_equal
                                       (out_of_screen loc810 border_x2
                                          border_y2) false);
    "within_border_edge_bottom" >:: (fun _ -> assert_equal
                                        (out_of_screen loc10 border_x2
                                           border_y2) false);
    "within_border_edge_top" >:: (fun _ -> assert_equal
                                     (out_of_screen loc1010 border_x2
                                        border_y2) false);
    "out_of_screen_left" >:: (fun _ -> assert_equal
                                 (out_of_screen loc_neg1_0 border_x2
                                    border_y2) true);
    "out_of_screen_bottom" >:: (fun _ -> assert_equal
                                   (out_of_screen loc_0_neg1 border_x2
                                      border_y2) true);
    "out_of_screen_right" >:: (fun _ -> assert_equal
                                 (out_of_screen loc118 border_x2
                                    border_y2) true);
    "out_of_screen_top" >:: (fun _ -> assert_equal
                                   (out_of_screen loc811 border_x2
                                      border_y2) true)
  ]

let between_test =
  [
    "between_true" >:: (fun _ -> assert_equal (between 3 2 4) true);
    "between_true_equal" >:: (fun _ -> assert_equal (between 3 3 4) true);
    "between_true_equal2" >:: (fun _ -> assert_equal (between 4 3 4) true);
    "between_false" >:: (fun _ -> assert_equal (between 2 3 4) false)
  ]

(* square 1: (0, 0) (0, 3) (3, 3) (3, 0)
 * square 2: (1, 1) (1, 3) (3, 3) (3, 1)
 * square 3: (5, 5) (5, 6) (6, 6) (6, 5)
*)
let intersect_test =
  [
    "intersect_1" >:: (fun _ -> assert_equal
                          (intersect 0 0 3 3 1 1 2 2) true);
    "intersect_1_rev" >:: (fun _ -> assert_equal
                          (intersect 1 1 2 2 0 0 3 3) true);
    "not_intersect1" >:: (fun _ -> assert_equal
                             (intersect 0 0 3 3 5 5 1 1) false);
    "not_intersect1_rev" >:: (fun _ -> assert_equal
                             (intersect 5 5 1 1 0 0 3 3) false);
    "not_intersect2" >:: (fun _ -> assert_equal
                            (intersect 1 1 2 2 5 5 1 1) false);
  ]
*)
