--
-- Name: author_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE author_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: authors; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE authors (
    id integer DEFAULT nextval('author_id_seq'::regclass),
    name character varying(255)
);


--
-- Name: paper_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE paper_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: paperauthor; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE paperauthor (
    paperid integer NOT NULL,
    authorid integer NOT NULL
);


--
-- Name: papers; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE papers (
    id integer DEFAULT nextval('paper_id_seq'::regclass),
    title character varying(255)
);


--
-- Name: paperauthor_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY paperauthor
    ADD CONSTRAINT paperauthor_pkey PRIMARY KEY (paperid, authorid);

--
-- PostgreSQL database dump complete
--
