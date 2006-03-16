fun fact(n) { if (n == 0) then { 1 } else { n * fact(n-1) } }
fun last(list, deflt) { if (list == []) then deflt else 
			  last(tl(list), hd(list))}
fact(6);                        # 720
last([1, 2, 3, 4, 5, 6], 0);;   # 6
