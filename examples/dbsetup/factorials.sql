--
-- PostgreSQL database dump
--

--
-- Name: factorials; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE factorials (
    i integer,
    f bigint
);


--
-- Data for Name: factorials; Type: TABLE DATA; Schema: public; Owner: -
--

COPY factorials (i, f) FROM stdin;
1	1
3	6
4	24
5	120
6	720
7	5040
8	40320
9	362880
10	3628800
11	39916800
12	479001600
13	6227020800
14	87178291200
15	1307674368000
16	20922789888000
17	355687428096000
18	6402373705728000
19	121645100408832000
20	2432902008176640000
2	2
\.

--
-- PostgreSQL database dump complete
--
