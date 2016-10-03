fun fold (f,acc,xs) =
  case cs of
      [] => acc
    | x:xs' =>  fold(f,f(acc,x),xs')

fun f1 xs = fold((fn (x,y)=> x+y),0,xs)
fun f2 xs = fold((fn (x,y)=> x andalso y >= 0), true , xs)
