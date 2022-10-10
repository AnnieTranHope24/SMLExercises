let val x = ref 0
in
    x := !x + 1;
    TextIO.output(TextIO.stdOut, "The new value of x is "^
				 Int.toString(!x)^"\n");
    !x
end
