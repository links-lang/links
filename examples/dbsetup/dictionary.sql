--
-- PostgreSQL database dump
--

--
-- Name: def_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE def_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: definitions; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE definitions (
    word character varying(255) NOT NULL,
    meaning text NOT NULL,
    id integer NOT NULL
);


--
-- Name: wordlist; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE wordlist (
    word character varying(255),
    type character varying(50),
    meaning text,
    id integer
);


--
-- Name: wordlist_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE wordlist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: wordlist_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE wordlist_id_seq OWNED BY definitions.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY definitions ALTER COLUMN id SET DEFAULT nextval('wordlist_id_seq'::regclass);


--
-- Name: wordlist_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY definitions
    ADD CONSTRAINT wordlist_pkey PRIMARY KEY (id);


--
-- Name: wordlist_word_index; Type: INDEX; Schema: public; Owner: -; Tablespace:
--

CREATE INDEX wordlist_word_index ON definitions USING btree (word);

--
-- PostgreSQL database dump complete
--
