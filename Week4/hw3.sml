fun only_capitals (strs : string list) =
  List.filter (fn x => Char.isUpper(String.sub(x,0))) strs

fun longest_string1 (strs : string list) =
  foldl (fn (x,y) =>if  String.size(x) > String.size(y) then x else y) "" strs

fun longest_string2 (strs : string list) =
  foldl (fn (x,y) =>if  String.size(x) >= String.size(y) then x else y) "" strs

fun longest_string_helper f (strs : string list) =
  foldl (fn (x,y) =>if f(String.size(x),String.size(y)) then x else y) "" strs


val longest_string3 = longest_string_helper (fn (x,y) => x>y) 
		    
val longest_string4 = longest_string_helper (fn (x,y) => x>=y) 

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode
  
exception NoAnswer

fun first_answer f list =
  case list of
      [] => raise NoAnswer
    | x::xs' =>  case f x of
		     SOME x => x
		   | _=> first_answer f xs'
	 

fun all_answers f list =
  let fun helper fhelp acc list =
	case list of
	    [] => acc
	  | x::xs' => case fhelp x of
			  NONE => raise NoAnswer
			| SOME i =>( helper fhelp (i@acc) xs')
  in SOME (helper f [] list) handle NoAnswer => NONE
  end

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
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards  = g (fn x => 1) (fn x=> 0) 

val count_wild_and_variable_lengths  = g (fn x => 1) (fn x => String.size(x))

fun count_some_var (s:string , p : pattern) =
  g (fn x=> 0) (fn x => if x = s then 1 else 0) p


fun check_pat (p : pattern) =
  let
      fun get_list (p : pattern) =
	case p of
	    Variable s => [s]
	  | TupleP ps => List.foldl (fn (p,i) => (get_list p) @ i) [] ps
	  | _ => []
      fun distinct (strs : string list) =
	case strs of
	    [] => true
	  | str::strs' => if List.exists (fn x => x = str ) strs' then false else distinct strs' 
  in distinct (get_list p)
  end

fun match (v : valu , p : pattern) =
  case p of
      Wildcard => SOME []
    | Variable s =>  SOME[(s,v)]
    | UnitP =>( case v of
		   Unit => SOME []
		 | _ => NONE )

    | TupleP ps => ( case v of
			 Tuple vs =>( all_answers (fn (v,p) => match(v,p))( ListPair.zipEq(vs,ps)) handle UnequalLengths => NONE)
		       | _ => NONE)
    | ConstP n  => (case v of
			Const i => if i=n then SOME [] else NONE
		     |  _=> NONE ) 
    | ConstructorP (s1,p') =>  (case v of
				    Constructor (s2,v') => if s1 = s2 then match(v',p') else NONE
				 | _  =>  NONE)

fun first_match v ps =
  SOME (first_answer(fn x => match(v,x)) ps) handle NoAnswer => NONE
