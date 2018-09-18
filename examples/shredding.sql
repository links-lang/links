--
-- Name: contacts; Type: TABLE; Schema: public; Owner: dbwiki; Tablespace:
--

CREATE TABLE contacts (
    dept text,
    contact text,
    client boolean
);

--
-- Name: departments; Type: TABLE; Schema: public; Owner: dbwiki; Tablespace:
--

CREATE TABLE departments (
    dept text
);

--
-- Name: employees; Type: TABLE; Schema: public; Owner: dbwiki; Tablespace:
--

CREATE TABLE employees (
    dept text,
    employee text,
    salary integer
);

--
-- Name: keytasks; Type: TABLE; Schema: public; Owner: dbwiki; Tablespace:
--

CREATE TABLE keytasks (
    task text
);

--
-- Name: tasks; Type: TABLE; Schema: public; Owner: dbwiki; Tablespace:
--

CREATE TABLE tasks (
    employee text,
    task text
);

--
-- Data for Name: contacts; Type: TABLE DATA; Schema: public; Owner: dbwiki
--

INSERT INTO contacts VALUES ('Product', 'Pam', false);
INSERT INTO contacts VALUES ('Product', 'Pat', true);
INSERT INTO contacts VALUES ('Research', 'Rob', false);
INSERT INTO contacts VALUES ('Research', 'Roy', false);
INSERT INTO contacts VALUES ('Sales', 'Sam', false);
INSERT INTO contacts VALUES ('Sales', 'Sid', false);
INSERT INTO contacts VALUES ('Sales', 'Sue', true);


--
-- Data for Name: departments; Type: TABLE DATA; Schema: public; Owner: dbwiki
--

INSERT INTO departments VALUES ('Product');
INSERT INTO departments VALUES ('Quality');
INSERT INTO departments VALUES ('Research');
INSERT INTO departments VALUES ('Sales');


--
-- Data for Name: employees; Type: TABLE DATA; Schema: public; Owner: dbwiki
--

INSERT INTO employees VALUES ('Product', 'Alex', 20000);
INSERT INTO employees VALUES ('Product', 'Bert', 900);
INSERT INTO employees VALUES ('Research', 'Cora', 50000);
INSERT INTO employees VALUES ('Research', 'Drew', 60000);
INSERT INTO employees VALUES ('Sales', 'Erik', 2000000);
INSERT INTO employees VALUES ('Sales', 'Fred', 700);
INSERT INTO employees VALUES ('Sales', 'Gina', 100000);


--
-- Data for Name: keytasks; Type: TABLE DATA; Schema: public; Owner: dbwiki
--

INSERT INTO keytasks VALUES ('abstract');
INSERT INTO keytasks VALUES ('enthuse');


--
-- Data for Name: tasks; Type: TABLE DATA; Schema: public; Owner: dbwiki
--

INSERT INTO tasks VALUES ('Alex', 'build');
INSERT INTO tasks VALUES ('Bert', 'build');
INSERT INTO tasks VALUES ('Cora', 'abstract');
INSERT INTO tasks VALUES ('Cora', 'build');
INSERT INTO tasks VALUES ('Cora', 'call');
INSERT INTO tasks VALUES ('Cora', 'dissemble');
INSERT INTO tasks VALUES ('Cora', 'enthuse');
INSERT INTO tasks VALUES ('Drew', 'abstract');
INSERT INTO tasks VALUES ('Drew', 'enthuse');
INSERT INTO tasks VALUES ('Erik', 'call');
INSERT INTO tasks VALUES ('Erik', 'enthuse');
INSERT INTO tasks VALUES ('Fred', 'call');
INSERT INTO tasks VALUES ('Gina', 'call');
INSERT INTO tasks VALUES ('Gina', 'dissemble');

