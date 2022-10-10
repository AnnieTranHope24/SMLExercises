fun sumupto(0) = 0
  | sumupto(n) =
    let val sum = sumupto(n-1)
    in
	n + sum
    end
