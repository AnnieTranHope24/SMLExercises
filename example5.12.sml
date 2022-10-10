fun append(L1, L2) =
    if null L1 then L2 
    else hd (L1)::append(tl(L1), L2)
	
(* use pattern-matching of parameters *)	
fun append(nil,L2) = L2
  | append(h::t,L2) = h::(append(t,L2))
