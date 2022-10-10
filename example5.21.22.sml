fun fib(0) = 0
  | fib(1) = 1
  | fib(n) = fib(n-1) + fib(n-2)

fun fibbetter(n) =
    let fun fibhelper(count,current,previous) =
	    if count = n then previous
	    else fibhelper(count+1, previous+current, current)
    in
	fibhelper(0,1,0)
    end;

fib(40);
fibbetter(40);
