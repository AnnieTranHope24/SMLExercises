fun plus (a:int,b) = a+b;

plus (5,8);

(* curried function - parameters are not a tuple *)
fun cplus (a:int) b = a+b;

cplus 5 8;

val add5 = cplus 5;

add5 9;

(* anonymous curried function *)
val anonymous = fn x => fn y => y*y + x;
anonymous 2 3;
