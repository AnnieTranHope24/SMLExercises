fun sum nil = 0
  | sum ((h:int)::t) = h + sum t;

sum [1,2,3,4,5];

fun product nil = 1
  | product ((h:int)::t) = h * product t;

product [1,2,3,4,5];

fun reduce f init nil = init
  | reduce f init (h::t) = f(h, reduce f init t);

val sum2 = reduce Int.+ 0;
sum2 [1,2,3,4,5];
val product2 = reduce Int.* 1;
product2 [1,2,3,4,5];

(* map-reduce sum of squares*)
reduce Int.+ 0 (map (fn x=>x*x) [1,2,3,4,5]);
