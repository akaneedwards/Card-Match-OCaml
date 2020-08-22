open State 
exception Empty
exception Malformed

(* prompt_coordinates prompts the user to input card positions
   returns the x,y coordinates of both positions as a tuple *)
let rec prompt_coordinates state = 
  print_endline "Please enter coordinates in order to match in the 
  following form : x,y\n";
  print_string  "> ";
  let rec parse state str =
    let list_string = Str.split (Str.regexp "[ \t^a-zA-Z(),/;.]+") 
    str |> List.map int_of_string in
    match list_string with
    | x::y::[] -> if(check_bounds (y,x) state) then (x,y) 
    else (print_endline "Outbound coordinates!Please try again\n"; 
    prompt_coordinates state)
    | _-> print_endline "invalid input try again\n";prompt_coordinates state; 
  in 
  match read_line() with 
  | str -> parse state str

