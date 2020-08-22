(** [print width height score cards timer] this is the primary print function*)
val print : int -> int -> (int*int) -> ((int*int)*string) list -> ((int*int)*string) list -> bool -> unit

(** [print_cursor x y] prints the cursor around x y card*)
val print_cursor : int -> int -> unit 

(** [print_win_message ()] prints the message stored in the win.txt file.*)
val print_win_message : unit -> unit

(** [print_losing_message ()] prints the message stored in the win.txt file.*)
val print_losing_message : unit -> unit

(** [print_robot_smart()] prints robot animation*)
val print_robot_smart: unit -> unit

(** [print_robot_random ()] prints robot animation *)
val print_robot_random: unit -> unit

(** [print_welcome_message ()] prints the message stored in the win.txt file.*)
val print_welcome_message : unit -> unit
