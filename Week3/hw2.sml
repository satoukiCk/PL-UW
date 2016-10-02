fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s: string , strs:string list) =
  let fun remove(s,strs) =
	case strs of
	    [] => []
	  | str::strs' => if same_string(s,str)
			  then remove(s,strs')
			  else
			      str::remove(s,strs')
      val result = remove(s,strs)
  in
     
      if result = strs
      then NONE
      else SOME(result)	  
  end

fun get_substitutions1 (strls : string list list , s : string) =
  case strls of
      [] => []
    | strl::strls' => let val result = all_except_option(s,strl)
		      in
			  case result of
			      NONE => get_substitutions1(strls',s)
			    | SOME list => list @  get_substitutions1(strls',s)
		      end
			  
fun get_substitutions2 (strls : string list list , s : string) =
  let
      fun aux(strls,acc) =
	case strls of
	    []  => acc
	  | strl :: strls  => let val result = all_except_option(s,strl)
			      in
				  case result of
				      NONE => aux(strls,acc)
				    | SOME list => aux(strls,list@acc)
			      end				  
  in
      aux(strls,[])
  end
      
fun similar_names (strls : string list list , r: {first:string , last:string, middle:string}) =
  let val {first = x,last = z ,middle = y} = r
      val subs = get_substitutions1(strls,x)
      fun get(subs : string list, z : string , y : string) =
	case subs of
	    [] => []
	   |sub::subs'  => {first = sub , last = z ,middle = y} :: get(subs',z,y) 
  in
     r::get(subs,z,y)
  end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (c : card) =
  let val (s,r) = c
  in
      case s of
	  Clubs => Black
	| Spades => Black
	| Diamonds => Red
	| Hearts => Red
  end

fun card_value (c : card) =
  let val (s,r) = c
  in
      case r of
	  Num i => i
	| Jack => 10
	| Queen => 10
	| King => 10
	| Ace => 11 		  
  end

fun remove_card (cs : card list , c : card , e : exn) =
  let fun remove(cs : card list , c: card) =
	case cs of
	    [] => []
	  | ca::cas' => if ca = c
			then cas'
			else
			    ca::remove(cas',c)
      val result = remove(cs,c)
  in
      if result = cs
      then raise e
      else result
  end

fun all_same_color (cs : card list) =
  case cs of
      [] => true
   | _::[] => true
   | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))  

fun sum_cards (cs : card list) =
  let fun sum(cs,acc) =
	case cs of
	    [] => acc
	  | c :: cs => sum(cs,acc + card_value(c))
  in
      sum(cs,0)
  end

fun score (cs : card list , goal : int) =
  let val sum = sum_cards(cs)
      fun cal(sum : int , goal : int) =
	if sum > goal
	then 3*(sum-goal)
	else goal - sum
      val result = cal(sum , goal)
  in
      if all_same_color(cs)
      then result div 2
      else result				 
  end
      
fun officiate (cs : card list , ms : move list ,goal: int) =
  let 
      fun aux(cs1 : card list,ms : move list,held : card list) =
	case ms of
	    [] => score(held,goal)
	   |m :: ms' => case m of
			    Discard d => aux(cs1,ms',remove_card(held,d,IllegalMove))
		          | Draw  => case cs1 of
					 [] => score(held,goal)
				       | c::cs' => let val added_held = c::held
						   in
						       if sum_cards(added_held) > goal
						       then score(added_held,goal)
						       else
							   aux(cs',ms',added_held)
						   end	      	    
  in
      aux(cs,ms,[])
  end
      
