fun list_product(xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)
      
fun countdown (x : int) =
  if x = 0
  then []
  else x::countdown(x-1)
		   
