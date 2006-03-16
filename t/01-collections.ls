l1 = [1, 2, 3];
l2 = [3, 4, 5];
s1 = set[1, 2, 3];
s2 = set[3, 4, 5];
b1 = bag[1, 2, 3];
b2 = bag[3, 4, 5];


b2 :bag: b1; # bag[3, 4, 5, 1, 2, 3] : bag[int]
b1 :bag: b2; # bag[1, 2, 3, 3, 4, 5] : bag[int]
s1 :set: s2; # set[1, 2, 3, 4, 5] : set[int]
l1 :: l2     # [1, 2, 3, 3, 4, 5] : [int]
