(*exercise 15 *) 
fun delete(i, s) = 
	let val cList = String.explode(s)
	in
	let fun reduce(i, []) = []
			|reduce(0, h::t) = t
			|reduce(i, h::t) = tl(reduce(i-1, h::t))
	in
	let fun help(i, []) = []
			|help(0, h::t) = t
			| help(i, h::t) = 
				if i>=length(h::t) orelse i<0 then
					h::t
				else
					List.take(h::t, i)@reduce(i, h::t)
	in
	String.implode(help(i, cList))
	end
	end
	end;
delete(3, "Hi there");
delete(0, "Hi there");
delete(20, "Hi there");
delete(1, "");
(*exercise 19*)
fun delete2 i s = 
	let val cList = String.explode(s)
	in
	let fun reduce i [] = []
			|reduce 0 (h::t) = t
			|reduce i (h::t) = tl(reduce (i-1) (h::t))
	in	
	let fun help (i:int) [] = []
			| help 0 (h::t) = t
			| help i (h::t) = 
				if i>=length(h::t) orelse i<0 then
					h::t
				else
					List.take(h::t, i)@reduce i (h::t)
	in
	String.implode(help i cList)
	end
	end
	end;
delete2 7 "Hi there";
(*exercise 20*)
fun delete5 s = 
	delete2 5 s;
delete5 "Hi there";
(*exercise 22*)
fun filter bfun nil = nil
	| filter bfun (h::t) = if bfun h then h::filter bfun t
							else filter bfun t;
fun isLower s = 
	if s="" then false
	else 
		if Char.ord(String.sub(s, 0))>96 andalso Char.ord(String.sub(s, 0))<123 then true
		else false;
filter isLower ["Annie", "tran", "cat"];
(*exercise 23*)
fun cap c = 
	if Char.ord(c)>96 andalso Char.ord(c)<123 then Char.chr(Char.ord(c)-32)
	else c;		
fun allCaps s = 
	let val cList = String.explode(s)
		in	
		String.implode(map cap cList)
		end;
allCaps "Annie";
(*exercise 25*)
fun transform f nil = nil
	|transform f (h::t) = (f h)::(transform f t) handle _ => h::(transform f t);
transform (fn x => 15 div x) [1,3,0,5];
(*exercise 26*)
datatype Natural = O
				 | succ of Natural
(*exercise 27*)
fun convert(O) = 0
	|convert(succ(n)) = 1 + convert(n);

convert(succ(succ(succ(O))));
(*exercise 28*)
fun add(x, O) = x
	|add(x, succ(n)) =succ(add(x, n));
val r = add(succ(O), succ(succ(O)));(*1+2*)
convert(r);
(*exercise 29*)	
fun mul(x, O) = O
	|mul(x, succ(n)) = add(x, mul(x, n));
val m = mul(succ(succ(O)), succ(succ(succ(O))));(*2*3*)
convert(m);
(*exercise 30*)	
fun hadd (l) = foldr add O l;
val s = hadd([succ(O), succ(succ(O)), succ(succ(O))]);(*[1, 2, 2]*)
convert(s);
(*exercise 24*)
(*http://www.cs.cornell.edu/courses/cs312/2006fa/recitations/rec09.html*)
(*https://stackoverflow.com/questions/66257455/how-to-read-a-file-in-sml-line-by-line*)
fun find(s,file) = 
	let
	val ins = TextIO.openIn file
	in
		let fun helper() =
			let val line = TextIO.inputLine(ins)
			in
				if Option.isSome(line) then 
					let val valLine = valOf(line)
					in
					if String.isSubstring s valLine then TextIO.output(TextIO.stdOut, valLine)
					else ();
					helper()
					end
				else ()
			end		
		in
			helper()
		end
	end;
	
find("exercise", "assignment2.sml");	

(* while loop solution *)
  (* let *)
    (* val ins = TextIO.openIn file *)
	(* val finish = ref false *)
	(* val i = ref (TextIO.inputLine(ins)) *)
	(* in *)
		(* while not (!finish) do  *)
			(* let val v = valOf(!i) *)
			(* in *)
				(* if String.isSubstring s v then TextIO.output(TextIO.stdOut, v) *)
				(* else TextIO.output(TextIO.stdOut, ""); *)
				(* if !i = NONE then finish := true *)
				(* else finish := false; *)
				(* i := (TextIO.inputLine(ins)) *)
			(* end; *)
	(* !finish *)
	(* end; *)
	