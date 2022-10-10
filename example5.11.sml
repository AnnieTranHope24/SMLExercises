fun length(x) =
    if null x then 0
    else 1+length(tl(x))

fun append(L1, L2) =
    if null L1 then L2 
    else hd (L1)::append(tl(L1), L2)

fun reverse(L) =
    if null L then [] else append(reverse(tl(L)), hd(L)::nil)
	
val jimmy = [1,2,3,4];
reverse(jimmy)
