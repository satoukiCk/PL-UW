 fun is_older(date1 : int * int * int , date2 : int * int * int) =
  if (#1 date1 <  #1 date2)
  then true
  else if (#1 date1 > #1 date2)
  then false
  else if (#2 date1 < #2 date2)
  then true
  else if (#2 date1 > #2 date2)
  then false
  else if(#3 date1 < #3 date2)
  then true
  else if(#3 date1 > #3 date2)
  then false
  else false

fun number_in_month(dates :(int * int * int) list , month : int) =
  if null dates
  then 0
  else if((#2 (hd dates)) = month)
  then 1 + number_in_month(tl dates,month)
  else number_in_month(tl dates,month)
  
fun number_in_months(dates : (int * int * int) list , months : int list) =
  if null months
  then 0
  else number_in_month(dates,hd months) + number_in_months(dates , tl months)
  
fun dates_in_month(dates : (int * int * int) list , month : int) =
  if null dates
  then []
  else if((#2 (hd dates)) = month)
  then (hd dates) :: dates_in_month(tl dates,month)
  else
      dates_in_month(tl dates , month)

fun dates_in_months(dates : (int * int * int) list,months :int list) =
  if null months
  then []
  else dates_in_month(dates , hd months) @ dates_in_months(dates , tl months)
			 
fun get_nth(strings : string list , n : int) =
  if n <> 1
  then get_nth(tl strings,n-1)
  else
      (hd strings)
      
fun date_to_string (date : int * int * int) =
  let
      val month = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
      get_nth(month,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int , nums : int list) =
  if (sum - (hd nums)) <= 0
  then 0
  else
      1 + number_before_reaching_sum(sum - (hd nums), tl nums)

fun what_month(day : int) =
  let
      val month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day,month) + 1
  end

fun month_range (day1 : int , day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1,day2)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else let
      fun find_oldest(dates : (int * int * int) list) =
	if null (tl dates)
	then hd dates
	else let val tl_ans = find_oldest(tl dates)
	     in
		 if is_older(hd dates,tl_ans)
		 then hd dates
		 else tl_ans
	     end
  in SOME(find_oldest dates)
  end
	   
		
