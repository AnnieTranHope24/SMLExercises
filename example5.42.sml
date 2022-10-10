exception emptyList;

fun maxIntList [] = raise emptyList
  | maxIntList (h::nil) = h
  | maxIntList (h::t) = Int.max(h, maxIntList t);

maxIntList(nil) handle emptyList => (TextIO.output(TextIO.stdOut,"Don't do that!\n"); 0)
