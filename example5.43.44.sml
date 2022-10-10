signature SetSig =
sig
    datatype 'a set = Set of 'a list;
    val emptyset : 'a set
    val singleton : 'a -> 'a set
    val member : ''a -> ''a set -> bool
    val union : ''a set -> ''a set -> ''a set
	val card : 'a set -> int
	val intersect : ''a set -> ''a set -> ''a set
	val setdif : ''a set -> ''a set -> ''a set
	val subset : ''a set -> ''a set -> bool
	(* val simetdif : ''a set -> ''a set -> ''a set *)
	(* val forall : ''a set -> (''a -> bool) -> bool *)
	(* val forsome : ''a set -> (''a -> bool) -> bool *)
	(* val forsomeone : 'a set -> ('a -> bool) -> bool    *)
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
fun setdif (s1 as Set L1) (s2 as Set L2) =
    let fun noDup1 e  = notmember e s2
	in
	let fun noDup2 e = notmember e s1
    in
	Set ((List.filter noDup1 L1)@(List.filter noDup2 L2))
	end 
	end
fun subset (Set []) s2 = true
	|subset (Set (h::t)) s2 =
	(member h s2) andalso (subset (Set t) (s2))
	
end (* end of the Set structure *)

val A = Set.Set [1,2,3];
val B = Set.Set[3,4,5];
val D = Set.Set[];
(* Set.member 4 A; *)
(* Set.member 4 B; *)
(* Set.union A B; *)
(* Set.setdif A B; *)
Set.subset B D;
val C = Set.Set [1.0,2.0,3.0];
(* Set.member 3.0 C; *)
(* Set.card A; *)
(* Set.intersect A B; *)
