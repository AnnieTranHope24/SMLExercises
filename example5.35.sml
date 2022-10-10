fun filter bfun nil = nil
| filter bfun (h::t) = if bfun h then h::filter bfun t
		       else filter bfun t;

filter (fn x=>(Int.mod(x,2)=0)) [1,2,3,4,5,6];
