(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* a *)
fun all_except_option (target_str, target_list) =
    let fun remove_from_list(current_list) =
	    case current_list of [] => []
			       | x::xs => if same_string(x, target_str)
					  then xs
					  else x::remove_from_list(xs)
	val filtered_list = remove_from_list(target_list)
    in
	if filtered_list = target_list then NONE else SOME filtered_list
    end

			    
(* b *)
fun get_substitutions1 (target_list, target_string) =
    let fun substitutions_helper(remaining_list) =
	    case remaining_list of [] => []
				 | x::xs => case all_except_option(target_string, x)
					      of NONE => substitutions_helper(xs)
					      | SOME i => i@substitutions_helper(xs)
    in
	substitutions_helper(target_list)
    end


(* c *)
fun get_substitutions2 (target_list, target_string) =
    let fun substitutions_helper(remaing_list, current_ret) =
	    case remaing_list of [] => current_ret
			       | x::xs => case all_except_option(target_string, x)
					   of NONE => substitutions_helper(xs, current_ret)
					    | SOME i => substitutions_helper(xs, current_ret@i) 
    in
	substitutions_helper(target_list, [])
    end


(* d *)
fun similar_names (target_list, full_name) =
    let val {first=x, middle=y, last=z} = full_name
	val substitution_list = get_substitutions1(target_list, x)
	fun substitute_helper(name_list) =
	    case name_list of [] => []
			    | x'::xs' => {first=x', middle=y, last=z}::substitute_helper(xs') 	    
    in
	full_name::substitute_helper(substitution_list)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10 *)
(*    though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color card =
    case card of (Spades, _) => Black
	       | (Clubs, _) => Black
	       | (Diamonds, _) => Red
	       | (Hearts, _) => Red


(* b *)
fun card_value card =
    case card of (_, Ace) => 11
	       | (_, Num n) => n
	       | _ => 10


(* c *)
fun remove_card (cs: card list, c:card, e:exn) =
    let fun remove_card_helper(cards) =
	    case cards of [] => raise e
			| x::xs => if x = c
				   then xs
				   else x::remove_card_helper(xs)
    in
	remove_card_helper(cs)
    end

(* d *)
(* bad style *)
fun all_same_color cards =
    case cards of [] => true
	| x::xs => let val color = card_color(x)
		       fun same_color_helper(cards_list) =
			   case cards_list of [] => true
			    | x'::xs' => if card_color(x') = color
					 then same_color_helper(xs')
					 else false
						
		   in
		       same_color_helper(xs)
		   end

(* good style *)
fun all_same_color_two cs =
    case cs of
	head::neck::tail => card_color head = card_color neck
			    andalso all_same_color(neck::tail)
     |  _ => true => 


(* e *)
fun sum_cards cards =
    let fun sum_cards_helper (card_list, cur_ret) = 
	    case card_list of [] => cur_ret
			    | x::xs => sum_cards_helper(xs, card_value(x) + cur_ret)
    in
	sum_cards_helper(cards, 0)
    end
    

(* f *)
(* bad style *)
fun score (card_list, goal) =
    let val sum = sum_cards(card_list)
	val all_color = all_same_color(card_list)
    in
	if sum > goal andalso all_color = true
	then (3 * (sum - goal)) div 2
	else if sum > goal andalso all_color = false
	then 3 * (sum - goal)
	else if sum <= goal andalso all_color = true
	then (goal - sum) div 2
	else goal - sum
    end


(* good style *)
fun score (cs,goal) =
    let
	val sum = sum_cards cs
    in
	(if sum >= goal then 3 * (sum - goal) else goal - sum)
	div (if all_same_color cs then 2 else 1)
    end


(* g *)
(* bad style*)
fun officiate (card_list, move_list, goal) =
    let fun officiate_helper (card_list, move_list, draw_list) =
	    if sum_cards(draw_list) > goal
	    then score (draw_list, goal)
	    else
		case move_list of [] => score(draw_list, goal)
				| x'::xs' => case x' of Discard c =>
							let val list_removed = remove_card(draw_list, c, IllegalMove)
							in officiate_helper(card_list, xs', list_removed)
							end
						      | Draw => case card_list of [] => score(draw_list, goal)
										| y'::ys' => officiate_helper(ys', xs', y'::draw_list)
    in
	officiate_helper(card_list, move_list, [])
    end
