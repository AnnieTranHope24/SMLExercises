signature SetSig =
sig
    datatype 'a set = Set of 'a list;
    val emptyset : 'a set
    val singleton : 'a -> 'a set
    val member : ''a -> ''a set -> bool
    val union : ''a set -> ''a set -> ''a set
    val card : 'a set -> int 
    val intersect : ''a set -> ''a set -> ''a set 
end



structure Set : SetSig =
struct

datatype 'a set = Set of 'a list

val emptyset = Set []

fun singleton e = Set [e]

fun member e (Set []) = false
  | member e (Set (h::t)) = (e = h) orelse member e (Set t)

fun notmember element st = not (member element st)

fun union (s1 as Set L1) (s2 as Set L2) = 
    let fun noDup e = notmember e s2
    in
	Set ((List.filter noDup L1)@(L2))
    end

fun card (Set L1) = length L1;

fun intersect (s1 as Set L1) (s2 as Set L2) =
    let fun dup e = member e s2
    in
	Set (List.filter dup L1)
    end

end (* end of the Set structure *)

open Set; (* like java import *)

val A = Set [1,2,3];
val B = Set[3,4,5];
member 4 A;
member 4 B;
union A B;
val C = Set[1.0,2.0,3.0];
(* Set.member 3.0 C;  *)
card A; 
intersect A B; 
