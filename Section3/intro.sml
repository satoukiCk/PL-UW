fun double x = 2*x
fun incr x = x + 1
val a_tuple = (double ,incr ,double(incr 7))
val eighteen = (#1 a_tuple) 9


fun n_times (f,n,x) =
  if n = 0
  then x
  else f (n_times(f,n-1,x)
	 )
fun increment x = x+1
fun double x = x+ x
val x1 = n_times(double ,4 ,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,12,16])
