fun add(x,y) {x + y}
fun fact(n) { if (n == 0) then { 1 } else { n * fact(n-1) } }
fun cartesian(f, n) { (arg=n|(val=f(n))) }


fun foldl(op, init, list) {
    if list == [] then 
        init
    else
        foldl(op, op(init, hd(list)), tl(list))
}

cartesian(fact, 10);
foldl(add, 0, [1, 2, 3, 4, 5, 6])











