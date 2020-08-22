open View
open State

let x = ref 1
let y = ref 1
let max_x = ref 0
let max_y = ref 0

(*[loop ()] loops until valid coordinates are selected.*)
let rec loop () =
  print_cursor (!x) (!y);
  match input_char stdin with
  | 'p' -> (!x,!y)
  | 'w' -> y:= max 1 ((!y)-1);
    loop ();
  | 's' -> y:= min (!max_y) ((!y)+1);
    loop ();
  | 'a' -> x:= max 1 ((!x)-1);
    loop ();
  | 'd' -> x:= min (!max_x) ((!x)+1);
    loop ();
  | _ -> loop ()

(*[set_cannoncical on] sets the attributes of Unix.*)
let set_canonical on =
  let attr = Unix.tcgetattr Unix.stdin in
  let attr' = { attr with c_icanon = on } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr'

(*[main ()] returns the coordinates of card to select. Sets the attributes.*)
let rec main () =
  set_canonical false;
  let (x,y) = loop () in
  set_canonical true;
  (x,y)

(*[select state] returns the coordinates of card to select*)
let select state = 
  max_x:= state.height;
  max_y:= state.width;
  main ()

