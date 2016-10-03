fun f g = g 2
val x = 4
fun h y = x + y
val z = f h


fun f1 y =
  let val x = y + 1
  in
      fn z => x + y + z
  end

fun f2 y =
  let
      val q = y + 1
  in
      fn z => q +y +z (*2y+1+z*)
  end

val a1 =(f1 7)
val a2 = (f1 7) 4


val x = 1
fun f y =
  let val x = y+1
  in fn z => x + y + z end

val x = "hi"
val g = f 7  (*add 15 to the argument. eg: z = 15 + 4*)
val z = g 4
