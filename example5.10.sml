fun implode(lst) =
    if lst = [] then ""
    else str(hd(lst))^implode(tl(lst));

implode([#"H",#"e",#"l",#"l",#"o"]);

fun explode(s) =
	if s = "" then []
	else String.sub(s,0) :: 
		explode(String.extract(s,1,NONE));

explode("hi");

implode(explode("hello"));