fun fact(n) { if (n == 0) then { 1 } else { n * fact(n-1) } }
fun factpair(n) { (arg=n,fact=fact(n)) }

factpair(6).fact;
r = factpair(12);
r;
r.fact;
r.arg;

n=8;
(arg=n,fact=fact(n)).fact
