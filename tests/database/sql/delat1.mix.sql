select (t2236."f") as "f" from "factorials" as t2232,lateral (select distinct (t2232."f") as "f" from (select distinct * from "factorials") as t2235 where (t2232."i") = (t2235."f")) as t2236
