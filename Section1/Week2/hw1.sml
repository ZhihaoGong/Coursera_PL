(* helper functions *)
fun get_year(date: int*int*int) = #1 date

fun get_month(date: int*int*int) = #2 date

fun get_day(date: int*int*int) = #3 date


(* 1 *)
fun is_older (date1: int*int*int, date2: int*int*int) =
    if get_year(date1) < get_year(date2)
    then true
    else if get_year(date1) > get_year(date2)
    then false
    else if get_month(date1) < get_month(date2)
    then true
    else if get_month(date1) > get_month(date2)
    then false
    else if get_day(date1) < get_day(date2)
    then true
    else false

	     
(* 2 *)
fun number_in_month (dates: (int*int*int) list, month: int) =
    let
	fun count_number (dates_list: (int*int*int) list) =
	    if null dates_list
	    then 0
	    else
		let val rest_count = count_number(tl dates_list)
		in
		    if get_month(hd dates_list) = month
		    then 1 + rest_count
		    else
			rest_count
		end
    in
	count_number(dates)
    end

	
(* 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
    let
	fun count_number (months_list: int list) =
	    if null months_list
	    then 0
	    else
		count_number(tl months_list) + number_in_month(dates, hd months_list)
    in
	count_number(months)
    end

	
(* 4 *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
    let
	fun find_dates (dates_list: (int*int*int) list) =
	    if null dates_list
	    then []
	    else
		let val rest_dates = find_dates(tl dates_list)
		in
		    if get_month(hd dates_list) = month
		    then hd dates_list::rest_dates
		    else rest_dates
		end
    in
	find_dates(dates)
    end
		
		   
(* 5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    let
	fun find_dates (months_list: int list) =
	    if null months_list
	    then []
	    else
		dates_in_month(dates, hd months_list)@find_dates(tl months_list)
    in
	find_dates(months)
    end


val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


(* 6 *)
fun get_nth (string_list: string list, n: int) =
    let
	fun find_n_est (current_list, count) =
	    if count = n
	    then hd current_list
	    else find_n_est(tl current_list, count + 1)
    in
	find_n_est(string_list, 1)
    end


(* 7 *)
val month_vocabulary = ["January", "February", "March", "April", "May", "June",
			"July", "August", "September", "October", "November", "December"]
fun date_to_string (date: (int*int*int)) =
    let
	val month_word = get_nth(month_vocabulary, get_month(date))
    in
	month_word ^ " " ^ Int.toString(get_day(date)) ^ ", " ^ Int.toString(get_year(date))
    end

	
(* 8 *)
fun number_before_reaching_sum (sum: int, nums_list: int list) =
    let
	fun before_reaching_num_helper (counter: int, cur_sum: int, cur_list: int list) =
	    if cur_sum + hd cur_list >= sum
	    then counter
	    else before_reaching_num_helper(counter + 1, cur_sum + hd cur_list, tl cur_list)
    in
	before_reaching_num_helper(0, 0, nums_list)
    end


(* 9 *)
val number_days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
fun what_month (day: int) =
    number_before_reaching_sum(day, number_days_in_month) + 1


(* 10 *)
fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else
	let
	    fun find_month_range (cur_day: int) =
		if cur_day = day2
		then what_month(cur_day)::[]
		else what_month(cur_day)::find_month_range(cur_day + 1)
	in
	    find_month_range(day1)
	end

(* 11 *)
fun oldest (days: (int*int*int) list) =
    if null days
    then NONE
    else if null (tl days) 
    then SOME(hd days)
    else
	let val rest_oldest = oldest(tl days)
	in
	    if is_older(hd days, valOf(rest_oldest))
	    then SOME(hd days)
	    else rest_oldest
	end

    
