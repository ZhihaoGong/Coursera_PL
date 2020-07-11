(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
				     
fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	 |  Variable x        => f2 x
	 |  TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	 |  ConstructorP(_,p) => r p
	 |  _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
			       
(**** you can put all your code here ****)

(* 1 *)
fun only_capitals str_list =
    List.filter((fn str => Char.isUpper(String.sub(str, 0)))) str_list


(* 2 *)
fun longest_string1 str_list =
    foldl((fn (x, y) => if String.size y >= String.size x then y else x)) "" str_list


(* 3 *)
fun longest_string2 str_list =
    foldl((fn (x, y) => if String.size y > String.size x then y else x)) "" str_list


(* 4 *)
fun longest_string_helper compare str_list = foldl(fn (x, y) => if compare(String.size x, String.size y) then x else y) "" str_list

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(* 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f xs =
    case xs of [] => raise NoAnswer
	     | x'::xs' => case f x' of SOME v => v
			     | _ =>  first_answer f xs'


(* 8 *)
fun all_answers f lst =
    let fun all_answers_helper remaining_lst acc =
	    case remaining_lst of [] => SOME acc
				| x'::xs' => case f x' of NONE => NONE
							| SOME v => all_answers_helper xs' (acc@v)							      
    in
	all_answers_helper lst []
    end


(* 9-a *)
fun count_wildcards p =
    g (fn() => 1) (fn str => 0) p


(* 9-b *)
fun count_wild_and_variable_lengths p =
    g (fn() => 1) String.size p


(* 9-c *)
fun count_some_var (str, p) =
    g (fn() => 0) (fn x => if x = str then 1 else 0) p


(* 10 *)
fun check_pat pat =
    let fun distinct_strs p =
	    case p of Variable x => [x]
		    | TupleP x_list => foldl (fn (x, y) => (distinct_strs x) @ y) [] x_list
		    | ConstructorP(_, x) => distinct_strs x
		    | _ => []
	fun check_repeats p_list =
	    case p_list of [] => false
			 | x'::xs' => if List.exists (fn (y) => x' = y) xs'
				      then true
				      else check_repeats xs'
	val f = check_repeats o distinct_strs
    in
       not (f pat)
    end

	
 (* 11 *)
fun match (value, pat) =
    case (value, pat) of (_, Wildcard) => SOME []
		       | (sv, Variable sp) => SOME [(sp, sv)]
		       | (Unit, UnitP) => SOME []
		       | (Const sv, ConstP sp) => if sv = sp
						  then SOME []
						  else NONE
		       | (Tuple sv_list, TupleP sp_list) => if List.length sv_list = List.length sp_list
							    then all_answers match (ListPair.zip(sv_list, sp_list))
							    else NONE
		       | (Constructor(svs, sv), ConstructorP(sps, sp)) => if svs = sps
									  then match(sv, sp)
									  else NONE
		       | (_, _) => NONE 
					      

				
(* 12 *)
fun  first_match pat lst =
     SOME (first_answer (fn (x) => match(pat, x)) lst) handle NoAnswer => NONE
