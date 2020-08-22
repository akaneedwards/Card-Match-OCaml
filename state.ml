type status = Matched | Unmatched | Bomb | Punishment
type mode = SingleMode | AIMode
type turn = AI | Player
let bomb_index = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]

type card = {
  id : int;
  number : int; 
  seen: int;
  status: status
}

type state = {
  permutation : card list;
  seen : (card * int) list; 
  difficulty: int;
  scores : (int * int); (* playerScore / aiScore *)
  matched_cards: (card list * card list); (* player cards / ai cards *)
  matched_positions : (int * int) list;
  turn: turn;
  mode: mode;
  width: int;
  height: int;
}

(*[coord pos state] returns the coordinates of the card at position [pos]*)
let coord pos state=
  ((pos mod state.width)+1, (pos/state.height)+1)

(* [get_list_from_permutation_of_pos] *)
let get_list_from_permutation_of_pos state size= 
  let rec get_list_from_permutation_of_pos_helper index acc= 
    match index with
    |0->acc
    |v-> get_list_from_permutation_of_pos_helper  (index - 1) ((coord v state)::acc)
  in get_list_from_permutation_of_pos_helper size []

let rec get_card_in_permuation index l = 
  match l with 
  | [] -> failwith "index out of bounds"
  | h::t -> if (index = 0) then h else get_card_in_permuation (index-1) t

(*[random_order x y] Randomly chooses 1 or 0*)
let random_order x y = 
  Random.self_init ();
  match Random.int 2 with
  |1->1
  |_->0

(* check bounds makes sure that the input positions 
   are between the height and width constraints
   returns false if they are not, true elsewise *)
let check_bounds (x1,y1) state = 
  if x1 > state.width ||x1 < 1  || 
     y1 > state.height  || y1 < 1  
  then false else true

(*[shuffle l] creates a permutation of list l*)
let shuffle l = 
  List.sort random_order l

(*[generate_cards id l] Generates a random list for the cards 
  and initializes the cards' [id], [number], [seen], and [status].
  [id] is the number representing the card, [number] indicates
  which card in the pair it is, [seen] being an int representing how
  many times the card has been flipped, and [status] being the status of the
  card. The accumulator is a list [l]*)
let rec generate_cards id l = 
  match id with 
  | 0 -> l
  | x -> {id=x;number=1;seen=0;status=Unmatched}::
         {id=x;number=2;seen=0;status=Unmatched}::generate_cards (id-1) l


(* get_unmatched_cards gets the list of cards that have not been matched *)

let get_unmatched_cards state= 
  let rec get_unmatched_cards_helper lst = 
    match lst with
    |[]->[]
    |h::t-> if h.status <> Matched then h::(get_unmatched_cards_helper t) 
     else get_unmatched_cards_helper t
  in get_unmatched_cards_helper state.permutation

(* get_new_permutation makes a new permutation with
   the old matched cards in the same index and the others shuffled*)
let get_new_permutation state = 
    let rec get_new_permutation_helper state_list lst acc= 
    match state_list with
    |[]-> acc
    |h::t-> if h.status <> Matched then
     match lst with
     |[]-> acc
     |hd::tl-> get_new_permutation_helper t tl (hd::acc)
    else get_new_permutation_helper t lst (h::acc)
    in get_new_permutation_helper state.permutation 
     (shuffle (get_unmatched_cards state)) []

(*[update_punish] changes the state for when a player hits a punishment card*)
let update_punish state = 
    {
      state with
      permutation = get_new_permutation state;
      seen = [];
      turn = if state.turn = AI then Player else AI;
    }



(*[add_bombs number l] adds and initializes bombs to the list of cards*)
let rec add_bombs number l = 
  match number with 
  | 0 -> l
  | x -> {id = -1; number = number; seen = 0; status = Bomb} :: 
   add_bombs (number-1) l

(*[add_punishment number l] adds and initializes punishment
  cards to the list of cards*)
let rec add_punishment number l = 
  match number with 
  | 0 -> l
  | x -> {id = -2; number = -2; seen = 0; status = Punishment} ::
    add_punishment (number-1) l

(*[random_card_permutation size] initializes a random list for the
  grid of cards. [size] is an int indicating the size of the grid.*)
let random_card_permutation size =
  let cards = (size*45/100)* 2 in
  let bombs = (size-cards) in
  generate_cards (cards/2) []
  |> add_bombs bombs
  |> shuffle
(*[initialize_state ((height, width):(int * int)) (difficulty:int)] initializes
  the state according to the [height], [width], and [difficulty] for the game.
  [permutation] is the randomized list for the grid. [seen] is initially empty
  because no cards are seen at the beginning of the game. 
  [matched_cards] is a tuple of two empty lists because no cards are matched
  at the start of the game. [matched_positions] is an empty list because
  there are no matched cards yet. The game starts on the player's turn so 
  [turn] is set to Player.*)
let initialize_state ((height, width):(int * int)) 
(difficulty:int) mode: state = 
  {
    permutation = random_card_permutation (height*width);
    seen = [];
    scores = (0 , (if mode = SingleMode then -1 else 0)); 
    difficulty = difficulty;
    matched_cards = ([],[]);
    matched_positions = [];
    turn = Player;
    mode = mode;
    width = width;
    height =  height;
  }

type result = Legal of state | Illegal of string

(*[get_card_in_permuation index l] searches and returns for the card 
 at [index] in [l]*)
let rec get_card_in_permuation index l = 
  match l with 
  | [] -> failwith "index out of bounds"
  | h::t -> if (index = 0) then h else get_card_in_permuation (index-1) t

(*[get_index (x,y) state] converts the index inputted into its index
 in a single linked list*)
let get_index (x,y) state = 
  state.width * (y-1) + x - 1 

(*[get_card (x,y) state] returns the card at [(x, y)] in
 [permutation] in [state]*)
let get_card (x,y) state = 
  let index = get_index (x,y) state in
  get_card_in_permuation index state.permutation

let check_matched (x,y) state = 
  let card = get_card (x,y) state in
  card.status = Matched 


let check_matched_list state = 
  List.filter (fun (card, pos)-> card.status <> Matched) state.seen

(*[update_permutation (card:card) matched_bool permutation]
  updates [permutation] with the [seen] in [card] if the [card] was not 
  matched, and sets [status] to Matched if the [card] is matched*)
let rec update_permutation (card:card) matched_bool permutation = 
  match permutation with 
  | [] -> []
  | h::t -> if(card=h) then {card with seen = (card.seen)+1; status =
    match matched_bool with 
      | true -> Matched |false -> card.status}:: t
    else h:: update_permutation card matched_bool t 


(* get_id_from_pos retuns the id of the card that has the position (x,y)
 from the state*)
let get_id_from_pos state (x,y)=
  let index = get_index (x,y) state in 
  let card = get_card_in_permuation index state.permutation in 
  card.id

(* get_list_to_show_helper returns the list of matched card positions and 
 their ids in order to input to the printing view functions *)
let rec get_list_to_show state l acc= 
  match l with 
  |[]-> acc
  |h::t-> (h, string_of_int (get_id_from_pos state h))::
   get_list_to_show state t acc

(* for bombs: if one of the cards is a bomb, update the number of times
 the cards around have been seen *)
(* gets the positions of the cards around the bomb *)
let get_bomb_area (x,y) bomb_index state= 
  let rec bomb_helper (x,y) l = 
    match l with 
    |[]-> []
    |(a,b)::t-> if check_bounds (a+x,b+y) state then ((a+x,b+y)::
     bomb_helper (x,y) t ) else bomb_helper (x,y) t
  in bomb_helper (x,y) bomb_index

(* returns the list to print for the cards around the bomb 
(also checks bounds)*)
let list_bomb_for_print state (x,y) bomb_index = 
  let lst = get_list_to_show state (get_bomb_area (x,y) bomb_index state) []
   in lst


let update_bomb state card =
  let rec update_bomb_helper permutation card =
  match permutation with 
  | [] -> []
  | h::t when card=h -> {card with status = Matched}::t
  | h::t -> h::(update_bomb_helper t card)
  in {state with permutation = update_bomb_helper state.permutation card;
                 }

(*[update_bomb_permutation call bomb_area permuation
  this will update the permutation for every card in the list lst*)
let rec update_bomb_permutation (x,y) lst state = 
  match lst with 
  |[]-> state
  |h::t -> let card = (get_card h state) in 
  update_bomb_permutation (x,y) t 
    ({ state with permutation = update_permutation card ((x,y) = h)  
    state.permutation;
    matched_positions = (x,y)::state.matched_positions
    })


(*[update_score turn (score1,score2) (card_1:card) (card_2:card)] 
  updates [score1] and [score2] representing the two players' scores by
  calculating the score by how many times the matched cards were seen.*)
let update_score turn (score1,score2) (card_1:card) (card_2:card) = 
  let s1 = max (1000 - 10*card_1.seen) 500 in
  let s2 = max (1000 - 10*card_2.seen) 500 in
  match turn with
  | Player -> (score1+s1+s2,score2)
  | AI -> (score1,score2+s1+s2)

(*[update_matched_cards turn (l1,l2) card_1 card_2] updates the list ofcards
  matched for the respective player*)
let update_matched_cards turn (l1,l2) card_1 card_2 = 
  match turn with
  | Player -> (card_1::card_2::l1,l2)
  | AI -> (l1,card_1::card_2::l2)


let update_seen card id matched seen=
  let rec update_seen_helper card lst =  
  match lst with 
  | [] -> [({card with status = if(matched) then Matched else Unmatched}, id)]
  | (h, pos)::t -> if h.id = card.id && h.number = card.number then 
   ({card with status = if(matched) then Matched else Unmatched}, pos)::t
  else (h, pos)::(update_seen_helper card t)
  in update_seen_helper card seen
  
  (*[flip position_1 position_2 state] makes two cards at [position_1]
  and [position_2] in [permutation] in [state] and updates [permutation] 
  according to if [card_1] and [card_2] match. Updates [seen] if the cards 
  were incorrectly matched and [score] depending on [seen]. Updates
  [matched_cards] by adding the newly matched cards to [matched_cards] and 
  changes [turn] to Player if [turn] was AI and [turn] to AI if [turn] was
  Player. Updates [matched_positions] to keep newly added cards 
  in [matched_cards] visible.*)
let flip position_1 position_2 state size= 
  let card_1 = get_card position_1 state in
  let card_2 = get_card position_2 state in

    {
    state with 
    permutation = (let matched_bool = if (card_1.id = card_2.id)
     then true else false in 
            update_permutation card_1 matched_bool state.permutation |>
            update_permutation card_2  matched_bool);
    seen = update_seen card_1 (get_index position_1 state) 
    (card_1.id = card_2.id) state.seen 
    |> update_seen card_2 (get_index position_2 state) (card_1.id = card_2.id);
    scores = if (card_1.id = card_2.id) 
             then update_score state.turn state.scores card_1 card_2 else
              state.scores;
    matched_cards = 
      if (card_1.id = card_2.id) 
             then update_matched_cards state.turn state.matched_cards 
             card_1 card_2 else state.matched_cards;
    turn = if(state.turn = Player && state.mode = AIMode) then AI else Player;
    matched_positions = if (card_1.id = card_2.id) then 
    (position_1 :: position_2 
    :: state.matched_positions) else state.matched_positions
  }