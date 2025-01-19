--
-- PostgreSQL database dump
--

--
-- Name: items; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE items (
    i integer NOT NULL,
    name text
);


--
-- Data for Name: items; Type: TABLE DATA; Schema: public; Owner: -
--

COPY items (i, name) FROM stdin;
0	Rupert
1	Edward
2	Pooh
3	Paddington
\.


--
-- Name: items_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (i);

--
-- PostgreSQL database dump complete
--
