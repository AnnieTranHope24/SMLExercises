(*exercise 2
(* - 8 mod 3 = 8 div 3 orelse 4 div 0 = 4; *)
(* val it = true : bool *)
(* - 8 mod 3 = 8 div 3 andalso 4 div 0 = 4; *)
(* uncaught exception Div [divide by zero] *)
  (* raised at: stdIn:5.29-5.32 *)*)
  
(*exercise 3*)
	(* if  8 mod 3 = 8 div 3 then *)
		(* true *)
	(* else *)
		(* if 4 div 0 = 4 then *)
			(* true *)
		(* else *)
			(* false; *)
(* (*exercise 4*)		 *)
(* if 8 mod 3 = 8 div 3 then *)
	(* if 4 div 0 = 4 then *)
		(* true *)
	(* else *)
		(* false *)
(* else *)
	(* false; *)
(*  https://www.cs.cmu.edu/afs/cs/academic/class/15814-f03/www/sml-intro.pdf*)
(*exercise 5*)	
String.str(#"h");
(*exercise 6*)	
Real.floor(4.567);
(*exercise 7*)
Char.ord(#"a");
(*exercise 8*)
Char.chr(100);
(*exercise 10*)
fun gcd(x, y) =
	if y = 0 then x
	else gcd(y, x mod y);
gcd(6, 8);
gcd(0, 7);
gcd(5, 0);
(*exercise 11*)
fun allCaps(s) =
	if s ="" then ""
	else 
		let val head = String.sub(s, 0)
		in 
		let val headInt = Char.ord(head)
		in
		if headInt>96 andalso headInt<123 then
			let val headIntCap = headInt -32
			in
			let val headCap = Char.chr(headIntCap)
			in
			String.str(headCap)^allCaps(String.extract(s, 1, NONE))
			end
			end
		else
			String.str(head)^allCaps(String.extract(s, 1, NONE))
		end
		end;
allCaps("Annie");
allCaps("123tran");
allCaps("");
(*exercise 12*)
fun firstCaps(lst) = 
	if lst=[] then []
	else 
		let val first = String.sub(hd(lst), 0)
		in
		let val headInt = Char.ord(first)
		in
		if headInt>96 andalso headInt<123 then
			let val headIntCap = headInt -32
			in
			let val headCap = Char.chr(headIntCap)
			in
			let val sModified = String.str(headCap)^String.extract(hd(lst), 1, NONE)
			in
			sModified :: firstCaps(tl(lst))
			end
			end
			end
		else 
			hd(lst)::firstCaps(tl(lst))
		end
		end;
firstCaps([]);		
firstCaps(["annie", "tran", "123ngoc"]);
(*exercise 13*)
fun swap([])=[]
	|swap(h::[]) = [h]
	|swap(h::t) = 
		let val s1 = swap(t)
		in
		if tl(s1) = [] then 
			let val hoftofs1 = hd(s1)
			in
			hoftofs1::h::swap(tl(t))
			end
		else 		
			let val h2oftofs1 = hd(tl(s1))
			in
			h2oftofs1::h::swap(tl(t))		
			end
		end;
swap([1,2,3,4,5]);
swap([]);
swap([1]);
swap([1,2,3,4]);
(*exercise 14*)
fun rotate(i, []) = []
	|rotate(i, h::t) =
		if i>=length(h::t) orelse i<=0 then
			h::t
		else 
			tl(rotate(i-1, h::t))@[hd(rotate(i-1, h::t))];
rotate(2, [1,2,3,4,5]);
rotate(5, [1,2]);
rotate(1, []);
