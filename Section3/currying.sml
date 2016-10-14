fun sorted3_tupled (x,y,z) = z >= y andalso y>=x

val sorted3 = fn x => fn y => fn z => z>= y andalso y>= x

val t2 = sorted3 7 9 11


		 
fun sorted3_nicer x y z = z>=y andalso y>=x
val t4 = sorted3_nicer 7 9 11

fun add x y z = x + y + z
val t5 = add 3 4 5
