fun firsts (xs : (int * int) list) =
  if null xs
  then []
  else (#1 (hd xs)) :: firsts(tl xs)
			     
