select (t2540."2@2@f1") as "f1",(t2540."2@2@f2") as "f2",(t2540."2@2@f3") as "f3",(t2540."2@2@f4") as "f4",(t2540."2@2@i1") as "i1",(t2540."2@2@i2") as "i2",(t2540."2@2@i3") as "i3",(t2540."2@2@i4") as "i4" from "factorials" as t2501,(select distinct (t2502."f") as "1@f",(t2502."i") as "1@i",(t2502."f") as "2@f",(t2502."i") as "2@i" from (select distinct * from "factorials") as t2502,(select distinct * from "factorials") as t2503) as t2504,(select distinct (t2505."f") as "1@1@f",(t2505."i") as "1@1@i",(t2505."f") as "1@2@f",(t2505."i") as "1@2@i",(t2507."f") as "2@1@f",(t2507."i") as "2@1@i",(t2507."f") as "2@2@f1",(t2505."f") as "2@2@f2",(t2507."i") as "2@2@i1",(t2505."i") as "2@2@i2" from (select distinct * from "factorials") as t2505,(select distinct * from "factorials") as t2506,(select distinct * from "factorials") as t2507,(select distinct * from "factorials") as t2508,(select distinct * from "factorials") as t2509) as t2510,(select distinct (t2511."f") as "1@1@1@f",(t2511."i") as "1@1@1@i",(t2511."f") as "1@1@2@f",(t2511."i") as "1@1@2@i",(t2513."f") as "1@2@1@f",(t2513."i") as "1@2@1@i",(t2513."f") as "1@2@2@f1",(t2511."f") as "1@2@2@f2",(t2513."i") as "1@2@2@i1",(t2511."i") as "1@2@2@i2",(t2516."f") as "2@1@1@f",(t2516."i") as "2@1@1@i",(t2516."f") as "2@1@2@f",(t2516."i") as "2@1@2@i",(t2518."f") as "2@2@1@f",(t2518."i") as "2@2@1@i",(t2518."f") as "2@2@2@f1",(t2516."f") as "2@2@2@f2",(t2513."f") as "2@2@2@f3",(t2511."f") as "2@2@2@f4",(t2518."i") as "2@2@2@i1",(t2516."i") as "2@2@2@i2",(t2513."i") as "2@2@2@i3",(t2511."i") as "2@2@2@i4" from (select distinct * from "factorials") as t2511,(select distinct * from "factorials") as t2512,(select distinct * from "factorials") as t2513,(select distinct * from "factorials") as t2514,(select distinct * from "factorials") as t2515,(select distinct * from "factorials") as t2516,(select distinct * from "factorials") as t2517,(select distinct * from "factorials") as t2518,(select distinct * from "factorials") as t2519,(select distinct * from "factorials") as t2520,(select distinct * from "factorials") as t2521) as t2522,(select distinct (t2523."f") as "1@1@1@1@f",(t2523."i") as "1@1@1@1@i",(t2523."f") as "1@1@1@2@f",(t2523."i") as "1@1@1@2@i",(t2525."f") as "1@1@2@1@f",(t2525."i") as "1@1@2@1@i",(t2525."f") as "1@1@2@2@f1",(t2523."f") as "1@1@2@2@f2",(t2525."i") as "1@1@2@2@i1",(t2523."i") as "1@1@2@2@i2",(t2528."f") as "1@2@1@1@f",(t2528."i") as "1@2@1@1@i",(t2528."f") as "1@2@1@2@f",(t2528."i") as "1@2@1@2@i",(t2530."f") as "1@2@2@1@f",(t2530."i") as "1@2@2@1@i",(t2530."f") as "1@2@2@2@f1",(t2528."f") as "1@2@2@2@f2",(t2525."f") as "1@2@2@2@f3",(t2523."f") as "1@2@2@2@f4",(t2530."i") as "1@2@2@2@i1",(t2528."i") as "1@2@2@2@i2",(t2525."i") as "1@2@2@2@i3",(t2523."i") as "1@2@2@2@i4",(t2534."f") as "2@1@1@1@f",(t2534."i") as "2@1@1@1@i",(t2534."f") as "2@1@1@2@f",(t2534."i") as "2@1@1@2@i",(t2536."f") as "2@1@2@1@f",(t2536."i") as "2@1@2@1@i",(t2536."f") as "2@1@2@2@f1",(t2534."f") as "2@1@2@2@f2",(t2536."i") as "2@1@2@2@i1",(t2534."i") as "2@1@2@2@i2",(t2530."f") as "2@2@f1",(t2528."f") as "2@2@f2",(t2525."f") as "2@2@f3",(t2523."f") as "2@2@f4",(t2530."i") as "2@2@i1",(t2528."i") as "2@2@i2",(t2525."i") as "2@2@i3",(t2523."i") as "2@2@i4" from (select distinct * from "factorials") as t2523,(select distinct * from "factorials") as t2524,(select distinct * from "factorials") as t2525,(select distinct * from "factorials") as t2526,(select distinct * from "factorials") as t2527,(select distinct * from "factorials") as t2528,(select distinct * from "factorials") as t2529,(select distinct * from "factorials") as t2530,(select distinct * from "factorials") as t2531,(select distinct * from "factorials") as t2532,(select distinct * from "factorials") as t2533,(select distinct * from "factorials") as t2534,(select distinct * from "factorials") as t2535,(select distinct * from "factorials") as t2536,(select distinct * from "factorials") as t2537,(select distinct * from "factorials") as t2538,(select distinct * from "factorials") as t2539 where (t2528."f") <= (t2536."i")) as t2540 where (((((((((((((((((((((((((t2522."1@1@1@f") = (t2540."1@1@1@1@f")) and ((t2522."1@1@1@i") = (t2540."1@1@1@1@i"))) and ((t2522."1@1@2@f") = (t2540."1@1@1@2@f"))) and ((t2522."1@1@2@i") = (t2540."1@1@1@2@i"))) and ((t2522."1@2@1@f") = (t2540."1@1@2@1@f"))) and ((t2522."1@2@1@i") = (t2540."1@1@2@1@i"))) and ((t2522."1@2@2@f1") = (t2540."1@1@2@2@f1"))) and ((t2522."1@2@2@f2") = (t2540."1@1@2@2@f2"))) and ((t2522."1@2@2@i1") = (t2540."1@1@2@2@i1"))) and ((t2522."1@2@2@i2") = (t2540."1@1@2@2@i2"))) and ((t2522."2@1@1@f") = (t2540."1@2@1@1@f"))) and ((t2522."2@1@1@i") = (t2540."1@2@1@1@i"))) and ((t2522."2@1@2@f") = (t2540."1@2@1@2@f"))) and ((t2522."2@1@2@i") = (t2540."1@2@1@2@i"))) and ((t2522."2@2@1@f") = (t2540."1@2@2@1@f"))) and ((t2522."2@2@1@i") = (t2540."1@2@2@1@i"))) and ((t2522."2@2@2@f1") = (t2540."1@2@2@2@f1"))) and ((t2522."2@2@2@f2") = (t2540."1@2@2@2@f2"))) and ((t2522."2@2@2@f3") = (t2540."1@2@2@2@f3"))) and ((t2522."2@2@2@f4") = (t2540."1@2@2@2@f4"))) and ((t2522."2@2@2@i1") = (t2540."1@2@2@2@i1"))) and ((t2522."2@2@2@i2") = (t2540."1@2@2@2@i2"))) and ((t2522."2@2@2@i3") = (t2540."1@2@2@2@i3"))) and ((t2522."2@2@2@i4") = (t2540."1@2@2@2@i4"))) and ((((((((((((t2510."1@1@f") = (t2540."2@1@1@1@f")) and ((t2510."1@1@i") = (t2540."2@1@1@1@i"))) and ((t2510."1@2@f") = (t2540."2@1@1@2@f"))) and ((t2510."1@2@i") = (t2540."2@1@1@2@i"))) and ((t2510."2@1@f") = (t2540."2@1@2@1@f"))) and ((t2510."2@1@i") = (t2540."2@1@2@1@i"))) and ((t2510."2@2@f1") = (t2540."2@1@2@2@f1"))) and ((t2510."2@2@f2") = (t2540."2@1@2@2@f2"))) and ((t2510."2@2@i1") = (t2540."2@1@2@2@i1"))) and ((t2510."2@2@i2") = (t2540."2@1@2@2@i2"))) and ((((((((((((t2510."1@1@f") = (t2522."1@1@1@f")) and ((t2510."1@1@i") = (t2522."1@1@1@i"))) and ((t2510."1@2@f") = (t2522."1@1@2@f"))) and ((t2510."1@2@i") = (t2522."1@1@2@i"))) and ((t2510."2@1@f") = (t2522."1@2@1@f"))) and ((t2510."2@1@i") = (t2522."1@2@1@i"))) and ((t2510."2@2@f1") = (t2522."1@2@2@f1"))) and ((t2510."2@2@f2") = (t2522."1@2@2@f2"))) and ((t2510."2@2@i1") = (t2522."1@2@2@i1"))) and ((t2510."2@2@i2") = (t2522."1@2@2@i2"))) and ((((((t2504."1@f") = (t2522."2@1@1@f")) and ((t2504."1@i") = (t2522."2@1@1@i"))) and ((t2504."2@f") = (t2522."2@1@2@f"))) and ((t2504."2@i") = (t2522."2@1@2@i"))) and ((((t2501."f") = (t2522."2@2@1@f")) and ((t2501."i") = (t2522."2@2@1@i"))) and ((((((t2504."1@f") = (t2510."1@1@f")) and ((t2504."1@i") = (t2510."1@1@i"))) and ((t2504."2@f") = (t2510."1@2@f"))) and ((t2504."2@i") = (t2510."1@2@i"))) and ((((t2501."f") = (t2510."2@1@f")) and ((t2501."i") = (t2510."2@1@i"))) and (((t2501."f") = (t2504."1@f")) and ((t2501."i") = (t2504."1@i")))))))))