open State
open View

(*[find_card card l] finds a duplicate of [card] in [l]*)
let rec find_card card l =
  match l with
  | [] -> None
  | (card2,pos2)::t -> if (card2.id = card.id && card2.number <> card.number)
    then Some pos2 else find_card card t

(*[find_pair l] finds a pair of matching cards in the list 
and returns there indices as a pair*)
let rec find_pair seen_cards =
  match seen_cards with
  | [] -> None
  | (card,pos)::t -> 
    (match find_card card t with 
    | None -> find_pair t
    | Some pos2 -> Some (pos,pos2)
    )

(*[next_index r permutation pos1] gets the index of the next
  card unmatched card after r*)
let rec next_index r l pos1 size =
  if (r=pos1) then next_index ((r+1) mod size) l pos1 size
  else (
  match (List.nth l r).status with
  | Matched -> next_index ((r+1) mod size) l pos1 size
  | Bomb -> next_index ((r+1) mod size) l pos1 size
  | _ -> r
  )

(*[random_move state] returns a random pair of cards' indicies to open*)
let random_move state =
  let size = List.length state.permutation in
  let l = state.permutation in
  let pos1 = next_index (Random.int size) l (-1) size in
  let pos2 = next_index (Random.int size) l pos1 size in 
  (pos1,pos2)

(*[smart_move state] returns a pair of cards' indicies to open*)
let smart_move state =
  print_endline "Making smart move";
  let l = check_matched_list state in
  match find_pair l with 
  | None -> random_move state
  | Some (pos1,pos2) -> (pos1,pos2)

(*[coord pos state] returns the coordinates of the card at position [pos]*)
let coord pos state=
    ((pos mod state.width)+1, (pos/state.height)+1)

let next_move state =
  let r = Random.int 11 in
  let pos1,pos2 = (
  if r > state.difficulty then (
  print_robot_random ();
  random_move state)
  else (print_robot_smart (); smart_move state)) in
  (coord pos1 state,coord pos2 state)