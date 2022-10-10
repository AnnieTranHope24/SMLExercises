datatype TokenType = Identifier | Keyword | Number | Add | Sub | Times | Divide | LParen | RParen | EOF | Unrecognized;

datatype AST = 
	 add' of AST * AST
       | sub' of AST * AST
       | prod' of AST * AST
	   |mul' of AST * AST
       | div' of AST * AST
       | negate' of AST
       | integer' of int
       | store' of AST
       | recall';

(* "min" stands for "memory in"; "mout" stands for "memory out" *)
fun evaluate(add'(e1,e2),min:int) = 
    let val (r1,mout1) = evaluate(e1,min)
	val (r2,mout) = evaluate(e2,mout1)
    in
	(r1+r2,mout)
    end

  | evaluate(sub'(e1,e2),min) = 
    let val (r1,mout1) = evaluate(e1,min)
	val (r2,mout) = evaluate(e2,mout1)
    in
	(r1-r2,mout)
    end
  | evaluate(mul'(e1,e2),min) = 
    let val (r1,mout1) = evaluate(e1,min)
	val (r2,mout) = evaluate(e2,mout1)
    in
	(r1*r2,mout)
    end	
	| evaluate(div'(e1, e2), min) = 
    let val (r1,mout1) = evaluate(e1,min)
	val (r2,mout) = evaluate(e2,mout1)
    in
		(r1 div r2, mout)
    end		
	|evaluate(negate'(e1), min) = 
    let val (r1,mout) = evaluate(e1,min)
		val ngate = r1*(~1)
		in
			(ngate, mout)
		end

  | evaluate(integer'(value:int),min) = (value,min)
  |evaluate(store'(e1), min) = 
    let val (r1,mout) = evaluate(e1,min)
		in
			(r1, r1)
		end
	|evaluate(recall', min) = (min, min);
	
evaluate(add'(integer'(3),integer'(4)),0);

val ex519 = add'(store'(negate'(integer'(6))), recall');
evaluate(ex519,0);

