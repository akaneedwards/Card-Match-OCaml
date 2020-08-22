(* changed *)

module T = ANSITerminal
open Printf

let text_color = T.black
let card_back_color = T.on_blue
let board_color = T.on_cyan
let card_front_color = T.on_white
let card_selected_color = T.on_green
let card_front_flipped_color = T.on_yellow
let width_card = 5
let height_card = 3
let colors = [T.blue;T.red;T.cyan;T.green;T.magenta]
let prev_x = ref 1
let prev_y = ref 1 
let max_y = ref (-1)

(** [read_whole_file file] returns the string stored in [file]. *)
let read_whole_file filename =
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

(** [print_background] prints the background 
    for the board with specific size *)
let print_background width height scores=
  T.erase T.Screen;
  T.set_cursor 1 1;
  let s = String.make ((width*(width_card+1))+1) ' ' in
  for i = 0 to (height*(height_card+1)) do
    T.print_string [board_color] s;
    printf "\n";
  done;
  T.move_bol();
  let s = match scores with
  |(player,-1) -> (String.concat "" ["Score: ";(string_of_int player);"\n"]);
  |(player,ai) -> 
  (String.concat "" 
  ["PlayerScore: ";(string_of_int player);" ";
  "AiScore: ";(string_of_int ai);]); in
  T.print_string [T.Underlined;T.red;T.Bold] s;
  printf "\n";
  if((!max_y) = -1) then let x,y = T.pos_cursor () in
  max_y := (y+1);;

(** [print_card x y color] prints a card at the [x] [y] position *)
let print_boarder x y color=
  T.set_cursor x y;
  let s = String.make (width_card+2) ' ' in
  T.print_string [color] s;
  printf "\n";
  for i = 1 to (height_card) do
    T.set_cursor x (y+i);
    T.print_string [color] " ";
    T.set_cursor (x+width_card+1) (y+i);
    T.print_string [color] " ";
    printf "\n";
  done;
  T.set_cursor x (y+height_card+1);
  T.print_string [color] s;
  printf "\n";;

(** [print_card x y color] prints a card at the [x] [y] position *)
let print_card x y color=
  T.set_cursor x y;
  let s = String.make (width_card) (Char.chr 46) in
  for i = 0 to (height_card-1) do
    T.set_cursor x (y+i);
    T.print_string [color;T.Bold] s;
    printf "\n";
  done;;

(** [print_cards] prints all the cards on board *)
let print_cards width height = 
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      print_card (2+ ((width_card+1)*j)) (2+((height_card+1)*i)) 
      card_back_color;
    done;
  done;;

(** [print_board width height score] prints the whole board *)
let print_board width height score= 
  print_background width height score;
  print_cards width height;;

(** [print_open_card width height score] prints an open card with 
    letter [c] at position [x][y] *)
let print_open_card x y c color=
  T.set_cursor x y;
  let s = String.make (width_card) ' ' in
  for i = 0 to (height_card-1) do
    T.set_cursor x (y+i);
    T.print_string [color] s;
    printf "\n";
    T.set_cursor (x+width_card/2) (y+height_card/2);
    T.print_string [card_front_color;text_color] c;
  done;;

(** [print_open_cards l] prints a list [l] of open cards *)
let print_open_cards l color=
  List.iter 
    (fun ((x,y),c) -> print_open_card (2+ ((width_card+1)*(x-1)))
    (2+ ((height_card+1)*(y-1))) c color) l;;

(** [print_time s] prints a countdown with [s] seconds*)
let print_time s = 
  for i = 3 downto 1 do
    printf "Time left %i%!" i;
    Unix.sleep 1;
    T.move_bol();
  done;;

let print_cursor x y =
  print_boarder (1+ ((width_card+1)*((!prev_x)-1))) 
  (1+ ((height_card+1)*((!prev_y)-1))) board_color;
  prev_x :=x;
  prev_y :=y;
  print_boarder (1+ ((width_card+1)*(x-1))) (1+ ((height_card+1)*(y-1)))
  card_selected_color; 
  T.set_cursor 1 (!max_y);;

let print width height scores matched_cards flipped_cards timer =
  print_board width height scores;
  print_open_cards matched_cards card_front_color;
  print_open_cards flipped_cards card_front_flipped_color;
  T.set_cursor 1 (!max_y);
  if timer then print_time 5 else ();;

let print_message file= 
  let s = read_whole_file file in
  for i = 0 to 10 do
    T.erase T.Screen;
    T.set_cursor 1 1;
    T.print_string [T.Bold;T.Blink;List.nth colors (i mod 5)] s;
    flush stdout;
    Unix.sleepf 0.5;
  done;
  printf "\n";;

let print_win_message () = 
  print_message "win.txt" 

let print_losing_message () = 
  print_message "lose.txt" 

let print_welcome_message () = 
  let s = read_whole_file "welcome.txt" in
  T.print_string [T.Bold;T.red] s;
  printf "\n";;

let print_robot_smart () =
  let s = read_whole_file "robot.txt" in
  T.set_cursor 1 (!max_y);
  T.print_string [T.Bold;] s;
  T.print_string [T.Bold;] "Let me think! I am making a smart move";
  flush stdout;
  Unix.sleep 3;
  T.set_cursor 1 (!max_y);
  T.erase T.Below;
  printf "\n";;

let print_robot_random () =
  let s = read_whole_file "robot.txt" in
  T.set_cursor 1 (!max_y);
  T.print_string [T.Bold;] s;
  T.print_string [T.Bold;] "I am making a random move";
  flush stdout;
  Unix.sleep 2;
  T.set_cursor 1 (!max_y);
  T.erase T.Below;
  printf "\n";;