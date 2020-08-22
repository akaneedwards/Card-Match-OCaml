open Command
open State
open View
open AI
open Select

exception Empty
exception Malformed

let bomb_index = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]

(* [get_second_coordinates state coord1] returns valid second coordiantes*)
let get_second_coordinates state coord1 =
  match select state with
  | c when c=coord1 -> print_endline "Please input different coordinates\n";
  select state
  | coord2 -> coord2
  
(* prompt_size prompts the user for a size, one of small, medium or large 
   returns a tuple (width, height)*)
let rec prompt_size =   
  print_welcome_message ();
  print_endline "Please choose a size: small, medium , large\n";
  print_string  "> ";
  match read_line () with
  | "small" -> (3,3)
  | "medium" -> (5,5)
  | "large" -> (7,7)
  |_-> raise Malformed

(* prompt_difficulty prompts the user for a value from 1-10 
   returns a int from 1-10*)
let prompt_difficulty = 
  print_endline "Please enter a value from 1-10 for difficulty.\n";
  print_string  "> ";
  match read_line () with
  |difficulty -> if (int_of_string difficulty)>10 then 10 
  else if int_of_string difficulty<1 then 1 else int_of_string difficulty

  let rec prompt_mode =
    print_endline "Please choose a mode: single or AI mode";
    match read_line () with
    | "single" -> SingleMode
    | "Single" -> SingleMode
    | "Single mode" -> SingleMode
    | "single mode" -> SingleMode
    | "AI" -> AIMode
    | "AI mode" -> AIMode
    | "ai mode" -> AIMode
    | "ai" -> AIMode
    |_-> raise Malformed

(* initializes an instance of the game with the size and difficulty that 
   the user inputs 
   returns a state*)
let initialize size difficulty=
  initialize_state size difficulty

(* returns the coord_id with the coordinate *)
let get_coord_id coord state = 
  (coord, string_of_int (get_id_from_pos state coord))

(* win_condition checks the win condition returns bool*)
let win_condition state = 
  let rec win_condition_helper l = 
    match l with 
    |[]->true
    |h::t -> if h.status = Matched || h.status = Bomb 
             then win_condition_helper t else false
  in win_condition_helper state.permutation

(*[who_won state] returns who won.*)
let who_won state = 
  match state.scores with
  |(player, ai)-> if ai > player then AI else Player 

(*[get_coord which_card ai coord1 state] return valid coordinates*)
let rec get_coord which_card ai coord1 state = 
  let coord = match which_card with 
  |0-> if(state.turn = AI) then ai else select state (*card 1*)
  |1-> if(state.turn = AI) then ai else 
  get_second_coordinates state coord1 (*card 2*)
  |_-> failwith "you should not be here"
  in match check_matched coord state with
  |true-> print_endline "You cannot select a matched card\n";
          get_coord which_card ai coord1 state
  |false-> coord

(*[win_handler state] returns who won.*)
let win_handler state = 
  if (state.mode = SingleMode) then (print_win_message ();
    exit 0;)
    else 
    if (state.mode = AIMode && who_won state = AI) then 
    (print_losing_message (); exit 0;) else
    print_win_message ();
    exit 0;;

(* turn is a function that should be called at each turn of the game.
   prompts user for coordinates of the card positions and prints the 
   game display with those cards flipped, calls itself with the new state *)
let rec turn state size= 
  if win_condition state then (
    win_handler state 
  ) 
  else 
    let (ai_c1,ai_c2) = if state.turn = AI then next_move state 
    else (1,1),(1,1) in
    let coord1 = get_coord 0 ai_c1 (-1,-1) state in
    let matched_cards = get_list_to_show state state.matched_positions [] in
    let card1 = get_coord_id coord1 state in
    print state.height state.width  state.scores 
     matched_cards (card1::[]) false;

    if (get_card coord1 state).status= Bomb then 
    (
    print state.height state.width  state.scores 
     matched_cards (card1::(list_bomb_for_print state coord1 bomb_index)) true; 
    print state.height state.width  state.scores ((coord1,string_of_int 
    (get_card coord1 state).id)::matched_cards) ([]) false;
    let new_state = update_bomb state (get_card coord1 state) in 
     turn (update_bomb_permutation 
     coord1 (get_bomb_area coord1 bomb_index new_state) new_state) size; )
    else
    let coord2 = get_coord 1 ai_c2 coord1 state in
    let card2 = get_coord_id coord2 state in
    
    if (get_card coord2 state).status= Bomb then 
    (
    print state.height state.width  state.scores matched_cards 
    (card1::card2::(list_bomb_for_print state coord2 bomb_index)) true;
    print state.height state.width  state.scores ((coord2,string_of_int 
    (get_card coord2 state).id)::matched_cards) ([]) false;
    let new_state = update_bomb state (get_card coord2 state) in 
    turn (update_bomb_permutation coord2 
     (get_bomb_area coord2 bomb_index new_state) new_state) size;)
    else
    print state.height state.width  state.scores 
    matched_cards (card1::card2::[]) true;
    let new_state = flip coord1 coord2 state size in
    let new_matched_cards = (get_list_to_show new_state
     new_state.matched_positions []) in
    print state.height state.width  new_state.scores
     new_matched_cards [] false;
    turn new_state size
;;


(* play_game initializes the game with the user input f size and difficuty *)
let play_game () =
  let size = prompt_size in 
  let difficulty = prompt_difficulty in
  let mode = prompt_mode in
  let state = initialize size difficulty mode in
  print  state.height state.width state.scores [] [] false;
  turn state (state.height*state.width);;

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* TODO add instructions here *)
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the card-matching game.\n");
  print_endline "Match all the cards to win!\n";
  play_game();;

(* Execute the game engine. *)
let () = main ()
