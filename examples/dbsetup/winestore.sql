--
-- PostgreSQL database dump
--

--
-- Name: countries; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE countries (
    country_id smallint NOT NULL,
    country character(30) NOT NULL
);


--
-- Name: customer; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE customer (
    cust_id integer NOT NULL,
    surname character varying(50),
    firstname character varying(50),
    initial character(1),
    title_id smallint,
    address character varying(50),
    city character varying(50),
    state character varying(20),
    zipcode character varying(10),
    country_id smallint,
    phone character varying(15),
    birth_date character(10)
);


--
-- Name: grape_variety; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE grape_variety (
    variety_id smallint NOT NULL,
    variety character varying(50) DEFAULT ''::character varying NOT NULL
);


--
-- Name: inventory; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE inventory (
    wine_id integer NOT NULL,
    inventory_id smallint NOT NULL,
    on_hand integer NOT NULL,
    cost numeric(5,2) NOT NULL,
    date_added date
);


--
-- Name: item_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: items; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE items (
    cust_id integer DEFAULT nextval('item_id_seq'::regclass) NOT NULL,
    order_id integer NOT NULL,
    item_id smallint NOT NULL,
    wine_id smallint NOT NULL,
    qty smallint,
    price numeric(5,2)
);


--
-- Name: orders; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE orders (
    cust_id integer NOT NULL,
    order_id integer NOT NULL,
    date character varying(12),
    instructions character varying(128),
    creditcard character(16),
    expirydate character(5)
);


--
-- Name: region; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE region (
    region_id smallint NOT NULL,
    region_name character varying(100) NOT NULL
);


--
-- Name: titles; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE titles (
    title_id smallint NOT NULL,
    title character(10)
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE users (
    cust_id integer NOT NULL,
    user_name character varying(50) NOT NULL,
    password character varying(32) NOT NULL
);


--
-- Name: wine; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wine (
    wine_id integer NOT NULL,
    wine_name character varying(50) NOT NULL,
    wine_type smallint NOT NULL,
    year smallint NOT NULL,
    winery_id smallint NOT NULL,
    description bytea
);


--
-- Name: wine_type; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wine_type (
    wine_type_id smallint NOT NULL,
    wine_type character varying(32) NOT NULL
);


--
-- Name: wine_variety; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE wine_variety (
    wine_id integer DEFAULT 0 NOT NULL,
    variety_id smallint DEFAULT (0)::smallint NOT NULL,
    id smallint DEFAULT (0)::smallint NOT NULL
);


--
-- Name: winery; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE winery (
    winery_id smallint NOT NULL,
    winery_name character varying(100) NOT NULL,
    region_id smallint NOT NULL
);


--
-- Data for Name: countries; Type: TABLE DATA; Schema: public; Owner: -
--

COPY countries (country_id, country) FROM stdin;
1	Afghanistan                   
2	Albania                       
3	Algeria                       
4	American Samoa                
5	Andorra                       
6	Angola                        
7	Anguilla                      
8	Antigua & Barbuda             
9	Argentina                     
10	Armenia                       
11	Aruba                         
12	Australia                     
13	Austria                       
14	Azerbaijan                    
15	Bahamas                       
16	Bahrain                       
17	Bangladesh                    
18	Barbados                      
19	Belarus                       
20	Belgium                       
21	Belize                        
22	Benin                         
23	Bermuda                       
24	Bhutan                        
25	Bolivia                       
26	Bosnia-Herzegovina            
27	Botswana                      
28	Brazil                        
29	British Virgin Is.            
30	Brunei                        
31	Bulgaria                      
32	Burkina Faso                  
33	Burundi                       
34	Cambodia                      
35	Cameroon                      
36	Canada                        
37	Canary Is.                    
38	Cape Verde                    
39	Central Africa                
40	Chad                          
41	Chile                         
42	China                         
43	Cocos I.                      
44	Colombia                      
45	Congo (Republic of the)       
46	Corsica                       
47	Costa Rica                    
48	Cote d'Ivoire                 
49	Crete                         
50	Croatia                       
51	Cuba                          
52	Cyprus                        
53	Czech Republic                
54	Dem. Rep. of Congo            
55	Denmark                       
56	Djibouti                      
57	Dominica                      
58	Dominican Republic            
59	DPR of Korea                  
60	Easter I.                     
61	Ecuador                       
62	Egypt                         
63	El Salvador                   
64	England                       
65	Equatorial Guinea             
66	Eritrea                       
67	Estonia                       
68	Ethiopia                      
69	Fed. Rep. of Germany          
70	Fiji                          
71	Finland                       
72	France                        
73	French Guiana                 
74	French Polynesia              
75	Gabon                         
76	Galapagos Is.                 
77	Georgia                       
78	Ghana                         
79	Gibraltar                     
80	Greece                        
81	Greenland                     
82	Grenada                       
83	Guadeloupe                    
84	Guam                          
85	Guatemala                     
86	Guernsey                      
87	Guinea                        
88	Guinea-Bissau                 
89	Guyana                        
90	Haiti                         
91	Honduras                      
92	Hong Kong                     
93	Hungary                       
94	Iceland                       
95	India                         
96	Indonesia                     
97	Iran                          
98	Iraq                          
99	Ireland                       
100	Isle of Man                   
101	Israel                        
102	Italy                         
103	Jamaica                       
104	Japan                         
105	Jersey                        
106	Jordan                        
107	Kazakhstan                    
108	Kenya                         
109	Kuwait                        
110	Kyrgyzstan                    
111	Laos                          
112	Latvia                        
113	Lebanon                       
114	Lesotho                       
115	Liberia                       
116	Libya                         
117	Liechtenstein                 
118	Lithuania                     
119	Luxembourg                    
120	Macao                         
121	Macedonia                     
122	Madagascar                    
123	Malawi                        
124	Malaysia                      
125	Maldives                      
126	Mali                          
127	Malta                         
128	Mauritania                    
129	Mauritius                     
130	Mexico                        
131	Micronesia                    
132	Moldovia                      
133	Monaco                        
134	Mongolia                      
135	Montserrat                    
136	Morocco                       
137	Mozambique                    
138	Myanmar                       
139	Namibia                       
140	Nauru                         
141	Nepal                         
142	Netherlands                   
143	New Caledonia                 
144	New Zealand                   
145	Nicaragua                     
146	Niger                         
147	Nigeria                       
148	Northern Ireland              
149	Norway                        
150	Oman                          
151	Pakistan                      
152	Panama                        
153	Papua New Guinea              
154	Paraguay                      
155	Peru                          
156	Philippines                   
157	Poland                        
158	Portugal                      
159	Puerto Rico                   
160	Qatar                         
161	Republic of Korea             
162	Revillagigedo                 
163	Romania                       
164	Russia                        
165	Rwanda                        
166	Samoa                         
167	San Marino                    
168	Sardinia                      
169	Saudi Arabia                  
170	Scotland                      
171	Senegal                       
172	Serbia and Montenegro         
173	Seychelles                    
174	Sierra Leone                  
175	Singapore                     
176	Slovak Republic               
177	Slovenia                      
178	Solomon Is.                   
179	Somalia                       
180	South Africa                  
181	Spain                         
182	Sri Lanka                     
183	stine                         
184	St. Kitts & Nevis             
185	St. Lucia                     
186	St. Vincent                   
187	Sudan                         
188	Suriname                      
189	Swaziland                     
190	Sweden                        
191	Switzerland                   
192	Syria                         
193	Taiwan                        
194	Tajikistan                    
195	Tanzania                      
196	Thailand                      
197	The Gambia                    
198	Togo                          
199	Tonga                         
200	Trinidad & Tobago             
201	Tunisia                       
202	Turkey                        
203	Turkmenistan                  
204	Tuvalu                        
205	Uganda                        
206	Ukraine                       
207	United Arab Emirates          
208	United States of America      
209	Uruguay                       
210	Uzbekistan                    
211	Vanuatu                       
212	Vatican                       
213	Venezuela                     
214	Vietnam                       
215	Virgin Is.                    
216	Wales                         
217	Yemen                         
218	Zambia                        
219	Zimbabwe                      
\.


--
-- Data for Name: customer; Type: TABLE DATA; Schema: public; Owner: -
--

COPY customer (cust_id, surname, firstname, initial, title_id, address, city, state, zipcode, country_id, phone, birth_date) FROM stdin;
1	Rosenthal	Joshua	B	1	34 Mellili Ln	Earlwood	VIC	6750	12	(613)83008460	1969-01-26
2	Serrong	Martin	P	1	45 Rosenthal Ccl	Doveton	VIC	6779	12	(613)87783583	1980-12-17
3	Leramonth	Jacob	I	1	59 Dalion Pl	Belmont	SA	7664	12	(618)74731325	1975-01-01
4	Keisling	Perry	R	5	146 Marzalla Ccl	Tara	NSW	7161	12	(612)73572833	1970-04-28
5	Mockridge	Joel	 	1	138 Rosenthal St	Tottenham	WA	5518	12	(618)86626713	1981-01-17
6	Ritterman	Richard	A	1	14 Dalion Cres	Mohogany	NT	7143	12	(618)86044644	1960-10-26
7	Morfooney	Sandra	V	4	176 Holdenson Crt	Olinda	QLD	6962	12	(617)75717701	1981-08-03
8	Krennan	Betty	V	2	31 Dimitria Ccl	Underwood	VIC	7872	12	(613)78027240	1970-04-04
9	Patton	Steven	 	1	197 Chester St	Torquay	WA	8868	12	(618)87611686	1968-10-13
10	Dalion	Horacio	Y	1	197 Barneshaw Ccl	Bauple	SA	7664	12	(618)67524135	1922-11-26
11	Keisling	Betty	V	2	173 Pattendon Ln	Eleker	NSW	6366	12	(612)52868706	1977-07-01
12	Tonnibrook	Sandra	B	2	158 Mettaxus Pl	Kadina	SA	6796	12	(618)76574741	1967-01-04
13	Dalion	Chris	T	1	36 Woodburne Cres	Essendon	WA	6950	12	(618)71708855	1955-01-22
14	Sorrenti	Caitlyn	O	2	21 Oaton Pl	Portsea	WA	7592	12	(618)76353023	1968-02-11
15	Cassisi	Derryn	K	5	9 Morfooney Cres	Portsea	QLD	5966	12	(617)78202002	1961-08-01
16	Dimitria	Lynette	D	2	172 Mellili Ccl	Lemont	WA	8855	12	(618)87502528	1961-09-04
17	Tonkin	Hugh	D	1	7 Taggendharf St	Bentley	QLD	7325	12	(617)83660800	1971-01-11
18	Stribling	James	F	1	155 Mellaseca Pl	Kulpi	NT	5400	12	(618)87677740	1953-02-11
19	Mellili	Melissa	S	2	91 Marzalla St	Tarneit	VIC	5698	12	(613)67606578	1968-10-01
20	Leramonth	Harry	Y	5	117 Barneshaw Cres	Eleker	VIC	5605	12	(613)77524715	1966-08-14
21	Ruscina	Jasmine	W	4	92 Skerry Ln	Karumba	WA	7892	12	(618)67752312	1971-01-11
22	Patton	Penelope	E	2	11 Tonkin St	Coonawarra	WA	8022	12	(618)80700641	1974-12-11
23	Lombardi	Hugh	T	1	174 Woodestock Ln	Alexandra	QLD	8018	12	(617)82808434	1953-11-21
24	Patton	Richard	P	1	190 Mettaxus Ccl	McLaren	NSW	5418	12	(612)70851445	1980-02-04
25	Pattendon	Megan	S	2	140 Serrong Ccl	Doveton	QLD	5501	12	(617)65380456	1966-08-26
26	Titshall	Richard	P	5	100 Mellili Ln	Berala	WA	5817	12	(618)64544413	1974-12-11
27	Marzalla	Sandra	F	4	148 Leramonth Ln	Underwood	NT	8466	12	(618)81235750	1964-10-01
28	Chester	Jasmine	B	2	158 Ruscina St	Ellendale	QLD	7327	12	(617)75303516	1943-04-24
29	Triskit	Perry	I	1	56 Marzalla Crt	Earlwood	NSW	8804	12	(612)64475386	1981-02-03
30	Marzalla	George	I	1	155 Woodestock St	Tarneit	NSW	5965	12	(612)64157468	1966-08-11
31	Dalion	Sandra	U	4	169 Triskit Cres	Coburg	WA	5446	12	(618)78544248	1938-04-01
32	Archibald	Joshua	M	1	94 Tonkin Cres	Olinda	NT	8815	12	(618)86032738	1971-01-04
33	Galti	Lynette	S	4	38 Skerry Cres	Hedley	TAS	7654	12	(613)77686341	1967-01-22
34	Woodestock	Patty	T	2	127 Rosenthal Ccl	Mepunga	WA	6831	12	(618)71145830	1968-02-11
35	Rosenthal	Jacob	Y	5	42 Lombardi Crt	Kidman	NSW	6539	12	(612)74320342	1966-08-01
36	Mellili	Cynthia	T	4	106 Eggelston Cres	Belmont	SA	5177	12	(618)71065081	1971-01-04
37	Tonnibrook	Penelope	F	4	139 Oaton Pl	Ormond	TAS	8146	12	(613)64380866	1943-04-04
38	Sears	Hugh	H	1	32 Pattendon Ln	Kalimna	TAS	5806	12	(613)58657348	1974-12-11
39	Nancarral	David	Q	1	65 Marzalla Ln	Semaphore	NT	7165	12	(618)82365228	1968-10-01
40	Cassisi	Chris	W	5	106 Taggendharf Crt	Portsea	NSW	7674	12	(612)80143316	1962-08-04
41	Mockridge	Megan	 	4	108 Dalion Ln	Tara	NSW	5185	12	(612)58643827	1943-04-04
42	Chester	Melissa	N	4	115 Holdenson Pl	Garvoc	SA	8640	12	(618)71403176	1974-12-11
43	Belcombe	Rhiannon	J	4	38 Mockridge Rd	Kidman	SA	7239	12	(618)83225052	1943-04-21
44	Mellili	Michelle	E	4	67 Cassisi Cres	Hedley	VIC	7494	12	(613)81640164	1955-06-04
45	Serrong	Dimitria	D	4	130 Titshall Crt	Milvale	NT	6745	12	(618)78621875	1970-11-01
46	Tonnibrook	Nicholas	N	1	188 Eggelston Ln	Mepunga	NSW	5970	12	(612)70108833	1980-02-11
47	Chemnis	Kym	T	4	49 Eggelston Ln	Stormlea	NT	7251	12	(618)67125337	1971-01-01
48	Eggelston	Bronwyn	F	2	140 Eggelston St	Mohogany	WA	6817	12	(618)72013132	1943-04-24
49	Morfooney	Anna	O	2	6 Oaton Pl	Richmond	VIC	7173	12	(613)67472553	1968-10-13
50	Nancarral	Robyn	G	4	23 Mellaseca St	Athlone	WA	8136	12	(618)71827807	1966-08-11
51	Kinsala	Bronwyn	G	4	81 Leramonth St	McLaren	TAS	6925	12	(613)84344486	1981-01-01
52	Marzalla	Karen	V	2	183 Eggelston Rd	Montague	NT	6939	12	(618)68433312	1943-04-04
53	Leramonth	Anthony	D	1	86 Cassisi Pl	Broadmeadows	QLD	5961	12	(617)60573031	1971-01-22
54	Woodestock	George	I	1	143 Oaton Cres	Richmond	SA	6697	12	(618)74584227	1968-02-11
55	Mellaseca	Rochelle	I	4	155 Eggelston Pl	Huntly	VIC	8872	12	(613)66718351	1967-01-01
56	Lombardi	Jasmine	F	2	141 Woodburne Ccl	Alexandra	QLD	8552	12	(617)53311402	1943-04-04
57	Tonkin	Nicholas	C	1	13 Mellili St	Bentley	QLD	7050	12	(617)66121222	1943-04-04
58	Chester	Betty	B	2	189 Keisling Pl	McLaren	VIC	8814	12	(613)72562481	1974-12-11
59	Mellaseca	Dimitria	D	5	115 Mellaseca Rd	Umina	SA	5715	12	(618)57227745	1955-10-01
60	Oaton	Caitlyn	U	2	72 Oaton Pl	Sunbury	QLD	8654	12	(617)73501775	1966-08-04
61	Archibald	Melissa	W	4	93 Sears St	Armidale	NSW	8355	12	(612)71584827	1943-04-04
62	Woodburne	Harry	P	1	141 Cassisi Ccl	Semaphore	SA	7947	12	(618)73282244	1962-02-11
63	Skerry	Robyn	K	4	155 Oaton Ln	Seabrook	NSW	7991	12	(612)62157248	1971-01-21
64	Krennan	Andrew	I	1	191 Rosenthal Crt	Hedley	WA	5005	12	(618)61115427	1968-06-14
65	Stribling	Anna	N	2	163 Barneshaw Ccl	Eleker	NSW	8782	12	(612)88343531	1966-08-01
66	Tonkin	Michelle	Z	2	2 Leramonth Ln	Longwood	NT	6072	12	(618)51730188	1974-12-11
67	Nancarral	Patty	E	2	142 Kinsala Pl	Kulpi	NT	8979	12	(618)55300371	1970-11-01
68	Sorrenti	Jasmine	H	2	96 Kinsala Ccl	Fitzroy	VIC	8709	12	(613)63848463	1980-02-24
69	Mellili	Patty	Q	2	121 Galti Pl	Singleton	WA	8442	12	(618)68174421	1968-10-13
70	Archibald	Derryn	H	5	107 Krennan Cres	Tabbita	TAS	8983	12	(613)80128451	1966-08-11
71	Mellaseca	Lynette	T	4	102 Dimitria Ccl	Gray	NSW	8957	12	(612)86073018	1962-10-01
72	Titshall	William	J	1	61 Krennan Ccl	Karumba	TAS	6860	12	(613)84444532	1943-04-04
73	Krennan	Samantha	M	2	81 Tonkin Ln	Bauple	NSW	6908	12	(612)64371220	1981-06-23
74	Leramonth	Cynthia	L	5	26 Krennan Ln	Pambulah	TAS	7201	12	(613)78352882	1968-02-11
75	Triskit	Jasmine	K	4	111 Eggelston St	Portsea	VIC	7219	12	(613)80232320	1955-08-01
76	Ritterman	Betty	G	2	150 Cassisi Cres	Bentley	VIC	7095	12	(613)52834232	1943-04-04
77	Stribling	Perry	F	5	122 Mettaxus Cres	Eleker	NSW	7676	12	(612)86040165	1943-04-04
78	Leramonth	James	K	1	167 Kinsala Rd	Portsea	TAS	8737	12	(613)63236780	1971-01-11
79	Woodburne	Michelle	D	2	62 Galti Ccl	Portsea	VIC	7380	12	(613)70344857	1968-10-01
80	Belcombe	Horacio	D	5	73 Stribling Crt	Nyah	NT	7253	12	(618)68772275	1962-08-04
81	Archibald	Hugh	A	1	195 Serrong Rd	Stormlea	NT	6087	12	(618)56424183	1943-04-04
82	Stribling	Patty	M	2	79 Woodburne St	Broadmeadows	WA	5761	12	(618)61488231	1970-01-11
83	Mockridge	Michael	L	1	196 Holdenson Cres	St Albans	SA	5407	12	(618)74541348	1943-04-21
84	Sears	Andrew	Z	1	143 Archibald Ccl	Tarneit	QLD	6500	12	(617)55414635	1968-06-04
85	Lombardi	Michael	V	1	149 Patton Ln	Kalimna	WA	7102	12	(618)71758846	1966-08-01
86	Holdenson	Cynthia	Z	4	112 Serrong Crt	Karumba	VIC	6476	12	(613)53502477	1974-12-11
87	Ritterman	Nicholas	B	1	100 Mellili Rd	Coburg	NSW	8542	12	(612)54528126	1938-04-01
88	Barneshaw	Lynette	S	4	112 Mellili Rd	Armidale	SA	8209	12	(618)85160470	1943-04-24
89	Morfooney	Chris	K	1	54 Marzalla Ccl	Coonawarra	NT	6030	12	(618)74768053	1968-11-26
90	Eggelston	Caitlyn	P	4	122 Ruscina Pl	St Albans	VIC	5976	12	(613)84514654	1980-08-11
91	Dalion	Robyn	Y	4	174 Triskit Ccl	St Albans	VIC	8577	12	(613)52744123	1962-10-01
92	Florenini	Joel	Q	1	125 Cassisi Cres	Eleker	QLD	5491	12	(617)77623448	1943-04-04
93	Morfooney	Larry	C	1	103 Woodestock Ccl	Alexandra	NT	6750	12	(618)72280650	1979-02-32
94	Oaton	Joel	V	1	144 Taggendharf Cres	Coonawarra	QLD	5093	12	(617)68315683	1968-01-11
95	Nancarral	Craig	J	1	190 Serrong Cres	Lucknow	WA	7230	12	(618)56166080	1981-06-01
96	Woodburne	Betty	A	2	120 Sorrenti Pl	Mohogany	VIC	5218	12	(613)68528130	1943-04-04
97	Eggelston	Peter	G	5	11 Tonnibrook Ccl	Kidman	QLD	5081	12	(617)71183241	1943-04-04
98	Galti	Dimitria	L	2	105 Chester Ccl	Eleker	NT	5732	12	(618)64327532	1971-01-11
99	Holdenson	Michelle	Q	4	115 Serrong Ln	Chatswood	TAS	8994	12	(613)62218455	1968-10-01
100	Mellili	Melissa	 	2	135 Barneshaw Rd	Earlwood	WA	6025	12	(618)63467365	1966-08-04
101	Chester	David	S	1	196 Woodburne Crt	Mohogany	TAS	7898	12	(613)66630137	1943-04-04
102	Serrong	Sandra	G	5	114 Kinsala Rd	Gray	WA	5928	12	(618)76858076	1962-02-11
103	Tonnibrook	Perry	K	1	3 Galti Ln	Hedley	NSW	7045	12	(612)81430320	1943-04-21
104	Krennan	Caitlyn	E	5	161 Barneshaw St	Tara	VIC	7187	12	(613)72716318	1961-12-04
105	Eggelston	Kym	R	4	95 Morfooney Rd	Springfield	SA	7164	12	(618)88286344	1966-08-26
106	Leramonth	Derryn	E	1	180 Leramonth Ccl	Doveton	NSW	5368	12	(612)67701614	1955-06-11
107	Tonkin	Anna	N	2	5 Leramonth Cres	Langwarrin	NT	8842	12	(618)71383438	1938-04-01
108	Skerry	Evonne	R	5	36 Mockridge Ln	Gobur	WA	5362	12	(618)85581314	1943-04-14
109	Patton	Samantha	 	4	39 Ritterman Ccl	Longwood	NT	8622	12	(618)67820536	1968-01-26
110	Lombardi	Harry	F	1	163 Lombardi Ccl	Nareen	SA	7600	12	(618)78240780	1966-08-11
111	Holdenson	Peter	K	1	85 Woodburne Ln	Halton	NSW	7065	12	(612)61520211	1970-11-01
112	Chester	Betty	Y	5	117 Barneshaw Ln	Lemont	VIC	6744	12	(613)66440513	1980-02-04
113	Dimitria	David	D	1	38 Patton Rd	Lucaston	TAS	8613	12	(613)65735243	1962-01-22
114	Sorrenti	Jacob	S	1	180 Ruscina Cres	Bentley	TAS	7201	12	(613)54756104	1968-02-11
115	Ruscina	Rochelle	P	4	114 Barneshaw Rd	Legana	TAS	6275	12	(613)84141842	1979-08-01
116	Belcombe	David	R	1	141 Chester St	Lucknow	NT	6001	12	(618)72565881	1943-04-04
117	Mellili	Richard	S	5	152 Dalion Rd	Lucaston	QLD	6348	12	(617)71516178	1981-06-03
118	Chemnis	Anna	O	2	125 Ruscina Ccl	Richmond	NSW	5418	12	(612)76822848	1974-12-11
119	Woodestock	Sandra	Z	2	176 Mettaxus Rd	Hedley	WA	6951	12	(618)87306458	1968-10-01
120	Kinsala	Caitlyn	O	2	199 Barneshaw Ln	Tottenham	QLD	7444	12	(617)58484561	1966-08-04
121	Krennan	Jim	N	1	109 Sears Rd	Broadmeadows	SA	7327	12	(618)66666675	1955-10-13
122	Mettaxus	Sandra	Q	4	161 Rosenthal Rd	Montague	TAS	6181	12	(613)85703876	1974-12-11
123	Kinsala	Melinda	M	4	134 Belcombe Ln	Huntly	NSW	7201	12	(612)51440243	1943-04-21
124	Morfooney	Karen	L	2	115 Rosenthal Ccl	Alexandra	SA	6894	12	(618)84702672	1963-02-04
125	Florenini	Cynthia	 	2	54 Keisling Cres	Graman	TAS	6903	12	(613)68361411	1971-01-26
126	Nancarral	Patty	B	2	94 Skerry Ccl	Huntly	NSW	8253	12	(612)57038336	1967-01-11
127	Dimitria	Caitlyn	O	4	92 Pattendon Rd	Eleker	NT	6276	12	(618)57004088	1938-04-01
128	Nancarral	Bronwyn	I	2	178 Sears Cres	Milvale	SA	7148	12	(618)83747253	1943-04-24
129	Serrong	Larry	R	1	40 Ritterman Ln	Coburg	TAS	5774	12	(613)71243043	1968-01-26
130	Tonnibrook	William	K	1	2 Marzalla Ln	Ellendale	TAS	6419	12	(613)73483586	1966-08-11
131	Lombardi	Nicholas	U	5	89 Taggendharf Ccl	Kadina	WA	6299	12	(618)73808058	1938-04-01
132	Mettaxus	George	I	1	173 Eggelston Ln	Belmont	SA	8675	12	(618)56582732	1943-04-04
133	Dimitria	Megan	U	4	51 Titshall Ccl	Gobur	WA	7846	12	(618)71467410	1962-11-22
134	Pattendon	Jasmine	R	4	123 Rosenthal Ccl	Legana	VIC	6856	12	(613)62367750	1980-02-11
135	Oaton	Chris	T	1	4 Galti Cres	Gray	TAS	6626	12	(613)86372534	1967-01-01
136	Ruscina	Patty	D	2	77 Sears Ln	Halton	NT	6313	12	(618)54126237	1943-04-04
137	Barneshaw	Joel	W	1	34 Leramonth Rd	Evandale	SA	6914	12	(618)62710277	1955-10-13
138	Triskit	Harry	B	1	49 Chester Pl	Evandale	WA	8242	12	(618)55207855	1974-12-11
139	Tonkin	James	P	1	130 Sears Pl	Westleigh	NSW	5040	12	(612)76144377	1981-10-01
140	Eggelston	Rhiannon	O	2	81 Tonnibrook Rd	Rhodes	NSW	7584	12	(612)83877675	1970-38-04
141	Dalion	Jim	O	1	147 Pattendon St	Sunbury	NSW	7874	12	(612)56205753	1971-01-26
142	Chemnis	Derryn	G	1	87 Krennan Rd	Huntly	TAS	5587	12	(613)87386417	1962-02-11
143	Galti	Jasmine	 	2	152 Archibald Ln	Murrabit	VIC	5280	12	(613)51860718	1943-04-21
144	Nancarral	Joshua	D	1	161 Keisling Ccl	Portsea	TAS	7068	12	(613)53742507	1978-02-04
145	Nancarral	Chris	L	1	66 Chester Ln	Richmond	VIC	5816	12	(613)86873765	1971-01-26
146	Leramonth	Evonne	M	2	14 Belcombe Pl	Mininera	WA	8976	12	(618)85074516	1967-01-11
147	Kinsala	Sandra	V	4	121 Ruscina St	Tara	WA	8183	12	(618)56763881	1938-04-01
148	Dimitria	Melissa	V	4	156 Mockridge Ccl	Culgoa	WA	5532	12	(618)51054660	1943-04-24
149	Taggendharf	Mark	O	1	140 Leramonth Rd	Earlwood	NT	8989	12	(618)57557112	1968-10-13
150	Morfooney	Jim	G	1	104 Cassisi Crt	Lucknow	QLD	5250	12	(617)81702264	1966-08-11
151	Tonkin	Perry	 	1	66 Tonnibrook Ln	Semaphore	NT	5422	12	(618)63565451	1938-04-01
152	Woodestock	Derryn	O	1	39 Rosenthal St	Mininera	SA	6507	12	(618)57322816	1943-04-14
153	Mockridge	Andrew	S	1	160 Woodburne St	Kalimna	VIC	5829	12	(613)64134525	1955-01-22
154	Archibald	Horacio	J	5	84 Belcombe Ln	Tabbita	WA	8423	12	(618)75711388	1968-02-11
155	Mellaseca	Derryn	U	1	180 Cassisi Crt	St Albans	NSW	8153	12	(612)74587068	1970-11-01
156	Cassisi	Joshua	E	1	110 Chester Rd	Mepunga	NSW	8783	12	(612)67027076	1980-01-04
157	Pattendon	Hugh	Q	1	186 Mockridge Ln	Athlone	SA	6012	12	(618)53103766	1967-01-26
158	Lombardi	Patty	E	2	165 Leramonth Cres	Mininera	TAS	7670	12	(613)77465370	1974-12-11
159	Ritterman	Martin	G	1	165 Chemnis St	Singleton	NSW	8596	12	(612)55210021	1968-10-01
160	Woodburne	Martin	V	5	91 Barneshaw Crt	Culgoa	WA	8658	12	(618)67038260	1971-01-04
161	Chemnis	Hugh	O	1	115 Keisling Cres	Pambulah	NSW	5914	12	(612)81773048	1981-02-03
162	Florenini	Richard	M	1	178 Cassisi Crt	Kadina	NT	6291	12	(618)67878813	1974-12-11
163	Ruscina	Penelope	U	4	38 Tonnibrook Cres	Doveton	NT	8762	12	(618)75045778	1943-04-21
164	Taggendharf	Chris	J	1	118 Tonnibrook Cres	Gray	WA	5727	12	(618)62282530	1962-02-04
165	Ruscina	James	T	1	57 Kinsala St	Stormlea	WA	5315	12	(618)71767108	1966-08-26
166	Sears	Samantha	I	4	77 Morfooney Crt	Westleigh	VIC	6156	12	(613)53304168	1943-04-11
167	Serrong	Jasmine	B	4	155 Pattendon St	Belmont	VIC	5362	12	(613)70217803	1938-04-01
168	Chester	Jim	M	1	109 Ruscina Ccl	Fernvale	QLD	7905	12	(617)52351633	1955-06-24
169	Leramonth	Sandra	Q	2	121 Cassisi Crt	Coburg	WA	6944	12	(618)58037400	1968-10-01
170	Ritterman	Samantha	B	2	93 Morfooney Pl	Nareen	WA	7849	12	(618)85263858	1966-08-11
171	Mellaseca	Michelle	R	2	86 Cassisi Pl	Kalimna	NSW	5008	12	(612)66105565	1970-30-01
172	Dimitria	Patty	 	2	157 Rosenthal Rd	Springfield	QLD	5859	12	(617)55562363	1971-01-04
173	Ritterman	Joshua	R	1	187 Dimitria Cres	Lemont	NT	6834	12	(618)68160556	1971-01-22
174	Mellili	Belinda	I	2	171 Chester St	Langwarrin	WA	6633	12	(618)80435000	1968-02-11
175	Taggendharf	Larry	C	1	36 Leramonth Cres	Vasey	VIC	8988	12	(613)62462616	1962-08-01
176	Taggendharf	Karen	D	2	126 Patton Pl	Olinda	QLD	7279	12	(617)74162877	1971-01-04
177	Stribling	Penelope	O	2	67 Pattendon Rd	Sunbury	SA	8845	12	(618)52711023	1970-11-10
178	Chester	Larry	E	1	154 Keisling Ccl	Tarneit	QLD	8088	12	(617)55030884	1980-02-11
179	Triskit	Jim	W	1	150 Marzalla Ccl	Singleton	TAS	8395	12	(613)63542348	1968-06-01
180	Patton	Nicholas	Z	5	192 Leramonth Cres	Bauple	SA	6317	12	(618)85080453	1966-08-04
181	Sorrenti	Cynthia	T	2	143 Mockridge Cres	Tabbita	WA	8816	12	(618)87667104	1943-04-04
182	Dimitria	William	W	1	199 Ritterman Pl	Essendon	NT	7023	12	(618)88063736	1974-12-11
183	Taggendharf	Chris	T	1	98 Sears Pl	Gray	QLD	8429	12	(617)78367044	1981-02-21
184	Mockridge	Dimitria	I	4	15 Mettaxus Ln	Coonawarra	WA	8938	12	(618)60611636	1955-02-04
185	Mettaxus	Samantha	R	2	63 Holdenson Cres	Lemont	TAS	8109	12	(613)82070243	1966-08-26
186	Cassisi	Megan	Z	4	93 Eggelston Pl	Mininera	TAS	8240	12	(613)62004588	1962-02-11
187	Titshall	Nicholas	 	1	147 Galti Crt	Singleton	QLD	8570	12	(617)77403016	1971-01-01
188	Belcombe	Martin	U	1	24 Nancarral Ln	St Albans	NSW	6971	12	(612)67567775	1968-02-24
189	Dimitria	Belinda	K	2	21 Mellaseca Crt	McLaren	NSW	6038	12	(612)88460442	1968-10-13
190	Pattendon	Belinda	B	4	96 Belcombe Cres	Bauple	VIC	8697	12	(613)56841817	1967-01-11
191	Rosenthal	Chris	A	1	29 Mellili Ccl	Longwood	TAS	7246	12	(613)83012326	1971-01-01
192	Tonkin	Michelle	I	2	52 Ruscina Ln	Belmont	NSW	5315	12	(612)62645788	1943-04-04
193	Galti	Jim	W	1	164 Marzalla Crt	Portsea	SA	7121	12	(618)60602713	1971-01-22
194	Woodburne	David	 	5	64 Stribling Pl	Richmond	VIC	5141	12	(613)84332583	1968-02-11
195	Mockridge	Horacio	D	1	95 Galti Pl	Seabrook	NSW	8670	12	(612)71483507	1966-08-01
196	Nancarral	Joshua	Y	1	32 Dimitria Ln	Olinda	VIC	6118	12	(613)65766107	1943-04-14
197	Skerry	Marie	H	2	109 Woodestock Crt	Chatswood	TAS	7847	12	(613)62417846	1962-10-13
198	Oaton	Mark	O	5	191 Ritterman Ln	Rhodes	QLD	5285	12	(617)78227785	1974-12-11
199	Leramonth	Harry	K	1	197 Mellaseca Ccl	Murrabit	VIC	5158	12	(613)57108558	1955-11-01
200	Dalion	Anthony	D	1	198 Eggelston Crt	Portsea	TAS	6556	12	(613)66526148	1980-08-04
201	Oaton	Michael	N	1	168 Kinsala Rd	Mepunga	TAS	8905	12	(613)72284445	1967-01-26
202	Nancarral	Derryn	V	1	199 Stribling Ln	Chatswood	SA	7621	12	(618)54148018	1974-12-11
203	Tonkin	Lynette	N	5	25 Marzalla Cres	Culgoa	QLD	7831	12	(617)57157653	1971-01-21
204	Rosenthal	George	F	1	176 Taggendharf Ccl	Langwarrin	NSW	8547	12	(612)65322620	1968-02-04
205	Tonkin	Mark	I	1	177 Tonnibrook Ccl	Kalimna	WA	6578	12	(618)50041388	1981-08-03
206	Titshall	Anna	Q	4	106 Taggendharf St	Sunbury	VIC	7837	12	(613)87861632	1974-12-11
207	Archibald	Derryn	C	1	33 Oaton St	St Albans	QLD	6632	12	(617)70306414	1971-01-01
208	Rosenthal	Betty	W	4	99 Nancarral Ccl	Broadmeadows	VIC	7233	12	(613)66386754	1962-02-24
209	Sorrenti	Melinda	J	5	17 Triskit Rd	Kulpi	TAS	6596	12	(613)53582787	1968-10-13
210	Krennan	Rhiannon	C	2	45 Sorrenti St	Nyah	NT	7932	12	(618)56808736	1967-01-11
211	Nancarral	Jasmine	Q	2	155 Tonnibrook Crt	Mepunga	VIC	7524	12	(613)62370203	1938-04-01
212	Keisling	Belinda	G	2	114 Lombardi Ln	Lucaston	QLD	8671	12	(617)56248475	1943-04-04
213	Tonkin	Bronwyn	V	4	98 Florenini Cres	Murrabit	VIC	6529	12	(613)76711015	1971-01-22
214	Belcombe	Evonne	H	4	153 Tonkin Cres	Tottenham	NT	8157	12	(618)83807301	1968-02-11
215	Florenini	Jasmine	F	5	102 Cassisi Pl	Gobur	QLD	8507	12	(617)88777823	1955-08-01
216	Mellaseca	Perry	S	1	24 Dalion Cres	Rhodes	WA	5460	12	(618)64826275	1943-04-04
217	Florenini	Mark	V	1	60 Holdenson Pl	Montague	NSW	6369	12	(612)68501817	1962-10-13
218	Patton	Caitlyn	P	2	73 Dimitria Crt	Underwood	TAS	7259	12	(613)51036245	1943-04-11
219	Triskit	Sandra	X	4	40 Holdenson Ccl	Sunshine	WA	6815	12	(618)85288465	1968-01-01
220	Florenini	Melinda	O	2	60 Barneshaw Pl	Longwood	NT	7406	12	(618)53584046	1966-08-04
221	Leramonth	David	P	1	124 Stribling Ccl	Gray	TAS	6114	12	(613)66387010	1970-11-26
222	Rosenthal	Karen	X	4	165 Florenini Ln	Legana	TAS	8606	12	(613)50341560	1980-02-11
223	Taggendharf	Chris	P	5	61 Mockridge Ln	Kulpi	SA	7887	12	(618)63880685	1971-01-21
224	Leramonth	William	B	1	82 Dimitria Rd	Underwood	WA	8486	12	(618)71452321	1968-02-04
225	Keisling	Rhiannon	M	4	144 Rosenthal Ln	Doveton	VIC	5577	12	(613)73386421	1966-08-26
226	Titshall	Jim	A	1	64 Stribling Ln	Rhodes	VIC	5289	12	(613)72720207	1974-12-11
227	Serrong	Larry	W	1	109 Archibald Cres	Fernvale	QLD	6695	12	(617)71584868	1981-10-01
228	Mockridge	Michael	P	1	25 Oaton Ln	Ormond	NSW	6976	12	(612)76246216	1962-02-24
229	Tonnibrook	Belinda	K	4	1 Pattendon Rd	Semaphore	NT	5117	12	(618)66468866	1968-10-13
230	Keisling	Kym	M	4	12 Kinsala Pl	Vasey	NSW	8793	12	(612)63023461	1967-01-11
231	Titshall	William	W	1	6 Tonkin Rd	Langwarrin	NSW	8162	12	(612)76586130	1955-10-01
232	Florenini	Mark	Z	1	30 Sears Crt	Springfield	NT	8206	12	(618)88381283	1943-04-04
233	Keisling	Marie	L	5	155 Dimitria Ccl	Mohogany	NSW	5280	12	(612)52031782	1971-01-22
234	Ritterman	Jacob	M	1	148 Tonnibrook Cres	Essendon	QLD	7849	12	(617)54337454	1968-30-11
235	Triskit	Kym	E	4	73 Ritterman Ccl	Garvoc	QLD	8239	12	(617)76248246	1971-01-01
236	Mockridge	Megan	D	5	32 Morfooney Rd	Sunbury	WA	7962	12	(618)51184432	1943-04-04
237	Tonkin	Samantha	K	4	120 Florenini Rd	Tarneit	QLD	8409	12	(617)72156113	1962-10-13
238	Stribling	Samantha	Q	2	14 Serrong Crt	Ormond	TAS	8465	12	(613)76144405	1974-12-11
239	Patton	Jim	Z	1	178 Nancarral Pl	Tara	NT	6109	12	(618)82336081	1968-01-01
240	Tonkin	David	P	1	81 Holdenson Ln	Colac	WA	7689	12	(618)52502842	1966-08-14
241	Chester	Nicholas	E	1	189 Stribling Rd	Berala	TAS	8659	12	(613)81815015	1943-04-04
242	Woodburne	Patty	C	4	80 Keisling St	Lucknow	VIC	7198	12	(613)50381102	1974-12-11
243	Mellaseca	Anthony	G	1	63 Stribling Crt	Broadmeadows	VIC	5723	12	(613)52453634	1970-11-21
244	Tonkin	James	X	1	168 Mellili St	Greensborough	VIC	8149	12	(613)67468121	1980-02-04
245	Woodburne	Rochelle	 	4	172 Tonnibrook Rd	Essendon	WA	6388	12	(618)53515348	1966-08-26
246	Dalion	Marie	C	2	60 Rosenthal Ln	Longwood	TAS	8141	12	(613)70307833	1962-02-11
247	Morfooney	Rochelle	J	5	198 Tonnibrook Crt	Essendon	QLD	8188	12	(617)68618537	1955-10-01
248	Leramonth	Kym	W	5	170 Belcombe Crt	Athlone	NT	5830	12	(618)62751127	1943-04104
249	Taggendharf	Betty	L	2	111 Barneshaw Cres	Portsea	NSW	6328	12	(612)70378652	1981-02-03
250	Kinsala	Rhiannon	Y	2	99 Cassisi Ccl	Greensborough	TAS	5733	12	(613)51167265	1971-01-11
251	Woodburne	Michelle	F	4	35 Mettaxus Rd	Nyah	NSW	6736	12	(612)88422053	1938-04-01
252	Eggelston	Perry	X	1	66 Dalion Cres	Singleton	NT	5811	12	(618)85305585	1943-04-04
253	Titshall	Penelope	V	2	172 Rosenthal Crt	Murrabit	NT	5459	12	(618)58063756	1971-01-22
254	Lombardi	George	Q	1	199 Florenini Pl	Karumba	WA	6103	12	(618)56674366	1968-01-11
255	Lombardi	David	W	1	62 Mettaxus Cres	Mohogany	SA	7207	12	(618)87447516	1966-08-01
256	Galti	Sandra	N	4	3 Mettaxus Rd	Umina	TAS	6361	12	(613)82867811	1943-04-04
257	Woodburne	Robyn	Y	2	40 Florenini Ln	Tara	SA	6655	12	(618)72201151	1962-10-13
258	Sorrenti	Chris	R	1	64 Mellili Pl	Westleigh	NT	6106	12	(618)60772417	1974-12-11
259	Barneshaw	Rhiannon	H	2	33 Woodestock Ln	Chatswood	TAS	8732	12	(613)8110186010	1968-10-01
260	Sears	Kym	V	4	29 Dimitria St	Alexandra	VIC	8126	12	(613)63740656	1966-08-04
261	Tonnibrook	Chris	U	1	8 Nancarral Ln	Halton	NT	7320	12	(618)78476587	1967-01-26
262	Ritterman	Hugh	C	5	158 Ritterman Cres	Mepunga	NSW	8525	12	(612)76228526	1955-02-11
263	Mellili	Anna	V	2	82 Holdenson Ccl	Eleker	NT	7900	12	(618)73352435	1943-04-21
264	Chester	Marie	S	2	145 Ruscina St	Longwood	WA	5365	12	(618)72757083	1968-02-04
265	Dalion	Hugh	H	5	49 Titshall Crt	Karumba	VIC	7763	12	(613)84068631	1970-11-26
266	Mettaxus	Caitlyn	I	2	68 Tonnibrook St	Semaphore	WA	8127	12	(618)72563542	1980-02-11
267	Florenini	Andrew	G	5	96 Ruscina Ccl	Murrabit	SA	5657	12	(618)80333262	1938-04-01
268	Cassisi	Larry	Q	1	26 Archibald Pl	Seabrook	SA	8136	12	(618)54048670	1962-02-24
269	Kinsala	Peter	C	1	198 Oaton St	Sunshine	SA	6488	12	(618)62648362	1968-01-26
270	Dalion	Michelle	D	4	157 Galti Ccl	Stormlea	TAS	6349	12	(613)72457761	1971-01-11
271	Mellili	Derryn	X	1	174 Krennan Rd	Lucknow	VIC	7269	12	(613)72640247	1981-10-01
272	Sorrenti	Steven	K	1	135 Taggendharf Cres	Nabawa	SA	5334	12	(618)57330828	1967-01-04
273	Krennan	Dimitria	D	2	131 Oaton Rd	Alexandra	WA	7205	12	(618)74513345	1943-04-21
274	Eggelston	Melissa	Z	4	44 Chester Rd	Bentley	NSW	6397	12	(612)80144865	1968-02-11
275	Keisling	Penelope	Q	2	144 Taggendharf Ln	Fitzroy	SA	8869	12	(618)86337218	1966-08-01
276	Stribling	Kym	Q	2	48 Eggelston Ln	Legana	QLD	6120	12	(617)76888135	1943-04-04
277	Triskit	Patty	L	2	189 Barneshaw St	Mininera	QLD	5537	12	(617)62516871	1955-10-13
278	Titshall	Sandra	U	4	160 Chester Crt	Essendon	TAS	8690	12	(613)61700800	1974-12-11
279	Mellaseca	Hugh	W	1	52 Triskit St	Springfield	TAS	5091	12	(613)80178768	1962-10-01
280	Skerry	Jacob	X	1	176 Morfooney Pl	Seabrook	SA	8496	12	(618)60437266	1971-01-04
281	Triskit	David	Q	1	77 Woodburne Pl	Kulpi	WA	7838	12	(618)66568380	1967-01-26
282	Krennan	Samantha	T	2	59 Mellaseca Pl	Lemont	NT	7530	12	(618)60622335	1974-12-11
283	Sorrenti	Rhiannon	U	2	125 Sears Rd	Kidman	WA	8500	12	(618)76378025	1943-04-21
284	Florenini	Harry	K	1	131 Sorrenti Pl	Pambulah	QLD	8595	12	(617)87467032	1968-01-14
285	Galti	Caitlyn	A	4	176 Barneshaw Pl	Eleker	NT	7244	12	(618)81251811	1966-08-26
286	Mettaxus	George	V	1	120 Holdenson Ln	Mininera	WA	5772	12	(618)53382017	1974-12-11
287	Florenini	Kym	M	4	29 Sorrenti St	Langwarrin	TAS	8471	12	(613)85466846	1970-11-01
288	Sorrenti	Patty	V	4	179 Patton Ccl	Doveton	SA	7852	12	(618)62881443	1980-02-24
289	Dalion	Jacob	D	1	104 Lombardi St	Lucknow	QLD	8873	12	(617)56224436	1968-10-13
290	Taggendharf	Dimitria	I	4	25 Belcombe Pl	Tara	VIC	7236	12	(613)86844520	1972-08-11
291	Barneshaw	Dimitria	A	4	38 Archibald Ln	Mohogany	TAS	8638	12	(613)62366520	1938-04-01
292	Woodestock	Derryn	K	1	20 Mellaseca St	Springfield	QLD	6877	12	(617)64320543	1955-06-04
293	Nancarral	George	D	1	61 Oaton Crt	Garvoc	NT	5475	12	(618)80535856	1981-02-23
294	Skerry	Craig	L	1	17 Holdenson Ccl	Underwood	SA	6322	12	(618)67680430	1968-02-11
295	Lombardi	Penelope	X	4	161 Keisling Ccl	Kulpi	SA	6411	12	(618)50175170	1971-01-01
296	Rosenthal	Caitlyn	A	4	95 Mellili Ln	Langwarrin	VIC	5322	12	(613)63082753	1943-04-04
297	Dimitria	Nicholas	R	1	63 Mellili Rd	Ormond	NSW	6758	12	(612)84002548	1943-04-04
298	Oaton	Rochelle	F	2	20 Krennan Rd	Longwood	NSW	8896	12	(612)60651750	1974-12-11
299	Eggelston	Martin	E	1	75 Krennan Ccl	Longwood	TAS	8999	12	(613)74102683	1962-01-01
300	Dalion	Sandra	N	4	59 Woodestock Ln	Sunshine	NSW	7713	12	(612)63352636	1966-08-04
301	Patton	Michelle	T	4	86 Belcombe Ln	Gobur	WA	8422	12	(618)87485231	1967-01-26
302	Mockridge	Penelope	I	2	92 Serrong St	Milvale	SA	6592	12	(618)64101633	1974-12-11
303	Lombardi	Sandra	 	4	181 Nancarral Pl	Chatswood	VIC	6648	12	(613)55721746	1943-04-21
304	Marzalla	Harry	H	1	197 Marzalla St	Hedley	VIC	7588	12	(613)56022605	1968-02-04
305	Cassisi	Kym	V	2	186 Chemnis St	Culgoa	NSW	8525	12	(612)75743511	1966-08-26
306	Tonkin	Martin	F	1	55 Lombardi Pl	Huntly	NSW	5686	12	(612)63088006	1974-12-11
307	Ritterman	Peter	R	1	89 Morfooney Cres	Evandale	SA	5656	12	(618)85584145	1955-10-01
308	Chemnis	Samantha	N	2	51 Pattendon Crt	Gray	SA	5630	12	(618)58855182	1962-02-24
309	Ritterman	Penelope	G	4	139 Dimitria Pl	Doveton	SA	5702	12	(618)76767623	1968-11-26
310	Mockridge	Belinda	E	2	14 Dalion Cres	McLaren	SA	6531	12	(618)60037501	1980-11-11
311	Sorrenti	Michelle	C	4	30 Krennan Cres	St Albans	NSW	8243	12	(612)64610180	1938-04-01
312	Triskit	Michael	C	1	117 Kinsala Rd	Tarneit	QLD	7970	12	(617)78180832	1967-01-04
313	Galti	Horacio	C	1	63 Keisling Ln	Coburg	NSW	6982	12	(612)57246787	1943-04-21
314	Eggelston	Michelle	H	5	137 Woodburne Crt	Sunbury	SA	6804	12	(618)73830028	1968-01-11
315	Nancarral	Melinda	Z	4	130 Patton Cres	Umina	NSW	8423	12	(612)56356545	1981-08-01
316	Chemnis	Joel	V	1	82 Morfooney Rd	Mininera	NSW	7632	12	(612)58824612	1943-04-04
317	Mellaseca	Larry	X	1	123 Pattendon Rd	Athlone	VIC	8130	12	(613)82247557	1943-04-04
318	Sears	Nicholas	G	1	113 Mettaxus Ccl	Tabbita	NT	6555	12	(618)80376325	1974-12-11
319	Oaton	Perry	F	1	129 Dalion Ccl	Karumba	QLD	7683	12	(617)72135003	1962-10-01
320	Mellaseca	Craig	Y	1	194 Serrong Ln	Longwood	QLD	6170	12	(617)67303806	1966-08-04
321	Stribling	Betty	U	2	136 Holdenson Rd	Hedley	WA	6897	12	(618)73707407	1972-10-13
322	Rosenthal	Perry	C	1	61 Sorrenti Ln	Umina	TAS	7330	12	(613)53887853	1955-02-11
323	Dimitria	Peter	Y	1	147 Keisling St	Nareen	VIC	6705	12	(613)58407153	1967-01-21
324	Mettaxus	Mark	E	1	119 Skerry Ccl	Westleigh	NSW	7788	12	(612)72746843	1968-02-04
325	Morfooney	Jim	F	5	31 Taggendharf Crt	Alexandra	NSW	6695	12	(612)63243807	1962-08-26
326	Galti	Rhiannon	C	2	134 Chester Ln	Broadmeadows	QLD	6887	12	(617)61266404	1971-01-11
327	Pattendon	Mark	E	1	53 Sorrenti Crt	Underwood	VIC	5484	12	(613)71123101	1938-04-01
328	Triskit	Sandra	B	5	176 Woodestock Pl	Sunbury	NT	6898	12	(618)81127688	1943-04-14
329	Pattendon	Anthony	Q	1	104 Chemnis St	Seabrook	NSW	6312	12	(612)62351267	1968-10-13
330	Rosenthal	Karen	L	2	105 Nancarral Ccl	Sunshine	WA	5024	12	(618)77411726	1962-01-11
331	Tonkin	Rhiannon	B	4	16 Kinsala Crt	Legana	NSW	7574	12	(612)58557765	1970-11-01
332	Mellili	Jasmine	C	4	57 Keisling Crt	Tabbita	WA	8240	12	(618)50500415	1980-06-04
333	Sears	Melissa	I	4	145 Mettaxus Cres	Doveton	QLD	8140	12	(617)50124173	1943-04-21
334	Serrong	Caitlyn	X	4	88 Archibald Crt	Lucaston	NSW	8348	12	(612)67318043	1968-02-11
335	Oaton	Anthony	Q	1	13 Titshall Crt	Tottenham	QLD	8775	12	(617)58438861	1966-08-01
336	Stribling	Belinda	Q	5	129 Galti Crt	Greensborough	WA	5753	12	(618)70653302	1943-04-04
337	Kinsala	Rochelle	A	4	53 Marzalla St	Lemont	SA	8704	12	(618)87023207	1981-02-03
338	Holdenson	David	U	1	166 Mellili Pl	Underwood	TAS	8046	12	(613)56848420	1955-02-11
339	Holdenson	Caitlyn	S	4	89 Sears St	Lucknow	QLD	7832	12	(617)87223488	1962-10-01
340	Dimitria	Betty	S	4	167 Woodburne Cres	Nabawa	QLD	8637	12	(617)85161555	1966-08-04
341	Eggelston	Peter	S	5	55 Cassisi St	Legana	WA	6989	12	(618)54171472	1971-01-26
342	Ritterman	Betty	N	4	185 Mettaxus Crt	Broadmeadows	SA	6784	12	(618)68231328	1974-12-11
343	Titshall	Penelope	H	4	147 Galti Crt	Tara	TAS	5303	12	(613)63641362	1943-04-21
344	Patton	Perry	C	1	71 Titshall Rd	Greensborough	VIC	7048	12	(613)61252000	1968-02-04
345	Cassisi	Derryn	 	1	36 Marzalla Cres	Doveton	TAS	8510	12	(613)86247530	1971-01-26
346	Krennan	Perry	K	1	16 Belcombe Pl	Belmont	VIC	6901	12	(613)73645561	1974-12-11
347	Triskit	Kym	 	4	160 Oaton Cres	Huntly	NT	7228	12	(618)86423062	1938-04-01
348	Woodestock	Michael	S	1	30 Dimitria Crt	Chatswood	NT	7542	12	(618)56657208	1962-02-24
349	Nancarral	Jasmine	M	4	163 Sorrenti Pl	Murrabit	QLD	5649	12	(617)60702138	1968-10-13
350	Mockridge	Caitlyn	K	4	43 Sorrenti Pl	Rhodes	NT	5482	12	(618)68460035	1967-01-11
351	Ritterman	Richard	 	1	167 Tonkin Ccl	Mininera	NT	6176	12	(618)52315657	1938-04-01
352	Galti	Richard	S	1	136 Woodburne St	Gray	TAS	8771	12	(613)57744058	1943-04-04
353	Ritterman	Bronwyn	D	2	52 Pattendon Cres	Hedley	QLD	6241	12	(617)58670440	1955-11-22
354	Rosenthal	Martin	S	1	60 Nancarral Crt	Earlwood	WA	7017	12	(618)57503602	1980-02-11
355	Stribling	Sandra	E	2	120 Nancarral Ln	Mohogany	SA	8985	12	(618)78855613	1966-08-01
356	Dalion	Joel	S	5	79 Dimitria Rd	Kidman	NSW	5779	12	(612)73450616	1971-01-04
357	Triskit	Harry	Y	1	33 Marzalla St	Milvale	TAS	8055	12	(613)60148154	1962-10-13
358	Triskit	Andrew	W	1	195 Woodburne Crt	Bauple	NT	5391	12	(618)74045151	1974-12-11
359	Skerry	Belinda	A	4	113 Lombardi Rd	Kadina	SA	7191	12	(618)78520614	1981-26101
360	Cassisi	Joshua	S	1	186 Stribling Rd	Singleton	VIC	5660	12	(613)68106230	1971-01-04
361	Mockridge	James	P	1	170 Taggendharf Pl	Portsea	NSW	6043	12	(612)55017453	1967-01-26
362	Ruscina	Cynthia	S	2	105 Florenini Ccl	Fitzroy	NT	6379	12	(618)67703644	1974-12-11
363	Ruscina	Kym	J	2	171 Mockridge Pl	Nareen	NT	5145	12	(618)51532850	1943-04-21
364	Titshall	Samantha	V	4	126 Barneshaw Pl	Alexandra	NSW	5324	12	(612)85524740	1968-02-04
365	Dalion	Larry	R	1	137 Mettaxus Ccl	Berala	TAS	6003	12	(613)76410103	1966-08-26
366	Leramonth	Derryn	B	1	99 Woodburne Ccl	Huntly	WA	8116	12	(618)86780423	1974-12-11
367	Holdenson	Anthony	V	1	152 Mellaseca Crt	Fernvale	WA	7382	12	(618)56321486	1938-04-01
368	Titshall	Harry	R	1	105 Titshall St	Singleton	NT	6376	12	(618)51716212	1955-02-24
369	Rosenthal	Michelle	O	4	122 Lombardi Ln	Halton	VIC	6715	12	(613)73285035	1968-10-13
370	Sears	Michelle	O	2	71 Holdenson Pl	Pambulah	NSW	6570	12	(612)80051128	1966-08111
371	Mettaxus	Kym	M	2	50 Tonkin St	Olinda	SA	7186	12	(618)67138817	1971-01-01
372	Chemnis	Nicholas	C	1	80 Rosenthal Ccl	Kalimna	WA	5953	12	(618)86760854	1967-01-14
373	Pattendon	Michelle	R	4	189 Tonkin Ln	Tottenham	QLD	7473	12	(617)54022468	1943-04-21
374	Tonkin	Nicholas	Q	1	187 Leramonth St	McLaren	VIC	7047	12	(613)74073747	1968-02-11
375	Florenini	Nicholas	P	1	135 Woodestock Pl	Legana	QLD	5996	12	(617)87045888	1970-11-01
376	Oaton	Jim	P	1	190 Sorrenti St	Mininera	WA	5250	12	(618)55466816	1980-02-04
377	Keisling	Jacob	K	5	45 Belcombe Pl	Kidman	NSW	8710	12	(612)65600651	1943-04-04
378	Pattendon	Jasmine	 	4	46 Krennan Rd	Nabawa	WA	5102	12	(618)66846221	1974-12-11
379	Sears	James	X	1	133 Skerry Cres	Torquay	VIC	7630	12	(613)62407041	1962-10-01
380	Galti	Marie	G	2	157 Dimitria Pl	Nabawa	SA	5413	12	(618)82162238	1966-08-04
381	Dimitria	Craig	X	1	23 Mellaseca Ccl	Sunbury	WA	6123	12	(618)63065251	1981-10-03
382	Mellaseca	Patty	W	2	137 Keisling Ln	Underwood	SA	8711	12	(618)52875731	1974-12-11
383	Mellili	Bronwyn	G	4	137 Barneshaw Ln	Semaphore	SA	6695	12	(618)88340614	1955-06-21
384	Woodburne	Lynette	O	2	41 Krennan Ln	Westleigh	QLD	7225	12	(617)76574850	1968-02-04
385	Ruscina	Kym	E	2	191 Patton Rd	Mepunga	WA	6559	12	(618)61662137	1966-08-26
386	Ritterman	David	B	1	54 Chemnis Ln	Earlwood	NSW	5543	12	(612)61502320	1971-01-11
387	Barneshaw	Anthony	H	1	191 Keisling Pl	Huntly	NT	6587	12	(618)73687436	1938-04-01
388	Pattendon	Jasmine	T	4	121 Krennan St	Springfield	QLD	8793	12	(617)68154087	1943-04-24
389	Patton	Joel	Z	1	148 Nancarral Crt	Coonawarra	NT	5085	12	(618)80142465	1968-10-13
390	Mellaseca	Harry	M	1	34 Tonnibrook Crt	Ellendale	NSW	6583	12	(612)78771153	1962-01-11
391	Taggendharf	Belinda	I	2	106 Ritterman Rd	Alexandra	WA	8624	12	(618)70273612	1938-04-01
392	Leramonth	Samantha	T	2	66 Patton Cres	Mohogany	NSW	5468	12	(612)78573744	1967-01-04
393	Florenini	Megan	M	2	175 Serrong Pl	Gobur	VIC	6788	12	(613)74607450	1943-04-21
394	Mellili	Megan	K	4	46 Stribling Rd	Tabbita	TAS	5326	12	(613)52447088	1968-02-11
395	Nancarral	Samantha	W	4	26 Pattendon Pl	Longwood	WA	5983	12	(618)63123108	1966-08-01
396	Belcombe	Lynette	F	4	200 Woodestock Crt	Halton	NSW	5365	12	(612)77860646	1943-04-04
397	Lombardi	Anthony	U	1	25 Sorrenti Ccl	Montague	WA	6959	12	(618)84570862	1970-11-26
398	Florenini	Melissa	Y	2	145 Sorrenti Rd	Ellendale	NSW	5219	12	(612)87771341	1980-02-11
399	Ritterman	Perry	J	1	39 Woodburne Crt	Murrabit	TAS	7564	12	(613)64242434	1962-10-01
400	Florenini	Karen	T	4	56 Serrong Pl	Greensborough	NT	6695	12	(618)52038114	1966-08-04
401	Tonkin	Melissa	V	2	148 Chester Ccl	Lemont	QLD	7186	12	(617)68034688	1971-01-26
402	Woodburne	Hugh	U	1	174 Sears Crt	Kulpi	SA	7059	12	(618)54247115	1974-12-11
403	Ruscina	Jacob	H	1	122 Krennan Cres	Rhodes	NSW	7916	12	(612)74123044	1981-02-21
404	Archibald	Kym	L	2	46 Morfooney Crt	Seabrook	QLD	6698	12	(617)51145060	1968-02-04
405	Morfooney	Bronwyn	X	2	120 Galti St	Broadmeadows	NT	7838	12	(618)54355607	1971-01-26
406	Mellaseca	George	K	1	100 Lombardi Pl	Lemont	TAS	5076	12	(613)63270843	1974-12-11
407	Mellili	Peter	R	1	97 Keisling Ccl	Kalimna	NT	8823	12	(618)55222168	1938-04-01
408	Patton	Joshua	Z	1	172 Nancarral Ln	Athlone	NT	8174	12	(618)60641242	1962-02-24
409	Cassisi	Marie	X	2	117 Eggelston Cres	Huntly	QLD	7889	12	(617)85400864	1968-10-13
410	Woodestock	Andrew	 	1	95 Mellaseca Ccl	Gobur	TAS	8053	12	(613)76425843	1938-04-11
411	Woodestock	Derryn	V	1	150 Mellaseca Ccl	Stormlea	WA	6831	12	(618)65635781	1938-04-01
412	Leramonth	Rhiannon	F	4	14 Belcombe Ccl	Essendon	TAS	8371	12	(613)84450750	1967-01-04
413	Triskit	Horacio	C	1	49 Florenini Ccl	Armidale	NSW	6643	12	(612)58822374	1955-02-21
414	Keisling	Samantha	L	2	91 Marzalla Ccl	Belmont	NSW	6042	12	(612)56570501	1968-02-11
415	Pattendon	Rochelle	J	2	12 Galti Ln	Milvale	NT	8788	12	(618)64550016	1966-08-01
416	Sears	Sandra	E	4	179 Titshall Pl	Chatswood	SA	7421	12	(618)57238400	1971-01-14
417	Kinsala	Nicholas	J	1	42 Barneshaw Ccl	St Albans	NT	5061	12	(618)68063630	1943-04-04
418	Kinsala	Kym	I	5	67 Sorrenti Pl	Underwood	SA	6613	12	(618)87704343	1974-12-11
419	Chemnis	Lynette	N	5	68 Holdenson Pl	Kidman	NSW	6251	12	(612)52256537	1962-11-01
420	Florenini	Chris	Z	1	64 Belcombe Cres	Seabrook	NSW	8781	12	(612)81338830	1980-01-04
421	Ritterman	Joel	I	1	179 Dalion Rd	Sunshine	SA	8061	12	(618)60048710	1967-01-26
422	Florenini	Dimitria	 	2	7 Titshall Pl	Armidale	WA	8402	12	(618)60631242	1974-12-11
423	Eggelston	Horacio	H	1	61 Chemnis Pl	Lucaston	QLD	6367	12	(617)62157875	1943-04-21
424	Keisling	Anna	G	4	187 Marzalla Cres	Montague	WA	8961	12	(618)67613484	1968-02-04
425	Morfooney	Jasmine	X	2	80 Skerry Ln	Kulpi	SA	6312	12	(618)52466628	1981-08-03
426	Barneshaw	Derryn	N	1	79 Morfooney St	Lemont	VIC	6324	12	(613)77225030	1974-12-11
427	Mellaseca	Perry	Q	1	136 Chemnis Ln	Graman	WA	8557	12	(618)87268117	1938-04-01
428	Mellili	Patty	L	4	26 Florenini Rd	Graman	WA	7875	12	(618)54426432	1955-02-24
429	Stribling	Melinda	B	4	54 Lombardi Rd	Portsea	TAS	8575	12	(613)50244777	1968-10-13
430	Dimitria	Harry	L	1	132 Taggendharf Ccl	Colac	VIC	7116	12	(613)65184760	1979-08-11
431	Keisling	Jim	Y	1	99 Chemnis Cres	Underwood	NSW	7782	12	(612)60177774	1970-03-01
432	Patton	Betty	B	4	146 Triskit Ccl	Fitzroy	TAS	7890	12	(613)81870268	1971-01-04
433	Dimitria	Jacob	Q	1	130 Dalion St	Murrabit	WA	5481	12	(618)65218820	1943-04-21
434	Morfooney	Richard	T	1	158 Lombardi Crt	Doveton	SA	7501	12	(618)52315848	1968-02-11
435	Dimitria	Joel	W	1	132 Ritterman Rd	Singleton	QLD	8549	12	(617)87131188	1966-08-01
436	Kinsala	George	R	1	183 Tonkin St	Eleker	SA	6663	12	(618)80471726	1971-01-04
437	Woodburne	Bronwyn	E	2	43 Ritterman Cres	Doveton	NSW	5000	12	(612)77275034	1943-04-04
438	Holdenson	Caitlyn	I	4	160 Pattendon Cres	Sunshine	TAS	7913	12	(613)60870221	1974-12-11
439	Chester	Jasmine	G	2	155 Woodestock Rd	Lucknow	NSW	6654	12	(612)73536510	1962-10-01
440	Marzalla	Marie	B	2	81 Pattendon Pl	Culgoa	WA	6140	12	(618)70328677	1966-08-04
441	Taggendharf	Marie	B	5	192 Marzalla Crt	Ormond	WA	8448	12	(618)86784674	1970-11-26
442	Holdenson	James	S	1	76 Mellaseca Rd	Mohogany	TAS	7645	12	(613)62472115	1980-02-11
443	Galti	Nicholas	Q	1	98 Mettaxus Pl	Kalimna	VIC	7669	12	(613)81713154	1943-04-21
444	Tonkin	Jacob	L	5	177 Mellaseca St	Portsea	QLD	7636	12	(617)81721770	1955-02-04
445	Dimitria	Karen	R	5	116 Holdenson Rd	Torquay	VIC	8236	12	(613)55336223	1966-08-26
446	Galti	Jim	B	1	167 Skerry Cres	Broadmeadows	SA	7348	12	(618)88242811	1974-12-11
447	Cassisi	Melinda	Z	2	167 Skerry Cres	Culgoa	NT	8146	12	(618)56131740	1981-10-01
448	Triskit	Larry	N	1	9 Mellaseca St	Berala	WA	6590	12	(618)72271367	1962-01-24
449	Patton	Penelope	L	2	59 Patton Crt	Lucaston	SA	8286	12	(618)71477042	1968-10-13
450	Mellaseca	Peter	C	1	96 Eggelston Ln	Mohogany	QLD	8105	12	(617)65214626	1965-08-11
451	Chemnis	Steven	R	1	28 Cassisi Crt	Tabbita	QLD	6577	12	(617)50863668	1938-04-01
452	Mettaxus	Derryn	B	1	90 Mellaseca St	Evandale	QLD	7508	12	(617)78505664	1971-01-04
453	Titshall	David	N	1	128 Keisling Cres	Mepunga	WA	6154	12	(618)75002787	1943-04-21
454	Leramonth	Caitlyn	P	4	14 Mellili Ccl	Torquay	VIC	6070	12	(613)57355415	1968-02-11
455	Nancarral	Samantha	S	4	139 Florenini St	Kulpi	NT	8256	12	(618)71286118	1966-08-01
456	Skerry	Richard	T	1	42 Marzalla Rd	Culgoa	NT	6210	12	(618)64010070	1943-04-04
457	Lombardi	Evonne	T	5	191 Marzalla Pl	Nareen	WA	8434	12	(618)64454372	1943-04-04
458	Lombardi	Megan	T	4	85 Ruscina Ln	Montague	WA	5634	12	(618)57855003	1974-12-11
459	Krennan	Rhiannon	Z	2	112 Nancarral Cres	Tarneit	QLD	8947	12	(617)80320313	1962-10-01
460	Ritterman	Larry	X	1	119 Marzalla Ln	Greensborough	NT	7436	12	(618)75853880	1955-08-14
461	Cassisi	Larry	M	1	44 Mellaseca Pl	Tottenham	NSW	5577	12	(612)63172677	1967-01-26
462	Rosenthal	William	C	1	190 Chester Ln	Legana	NSW	5996	12	(612)60357112	1974-12-11
463	Dalion	Melissa	T	2	69 Lombardi Ccl	Semaphore	SA	8000	12	(618)75767773	1970-11-21
464	Pattendon	Rhiannon	J	4	74 Pattendon Rd	Lucknow	TAS	5218	12	(613)57555477	1980-02-04
465	Mellaseca	Rhiannon	H	5	100 Rosenthal Ccl	Doveton	NT	5579	12	(618)53557482	1966-08-26
466	Galti	Hugh	M	1	2 Ritterman Cres	Ellendale	VIC	7285	12	(613)61000405	1974-12-11
467	Mockridge	Jasmine	T	2	55 Keisling Pl	Sunshine	TAS	7308	12	(613)61756664	1971-01-01
468	Mettaxus	Nicholas	B	1	177 Tonkin Rd	Tabbita	TAS	6314	12	(613)57134123	1962-02-24
469	Mettaxus	Jacob	 	1	54 Chemnis Crt	Vasey	VIC	7023	12	(613)56787183	1981-02-03
470	Chester	Peter	U	1	43 Serrong Pl	Athlone	WA	5117	12	(618)80541363	1967-01-11
471	Barneshaw	Evonne	U	2	118 Sorrenti Cres	Colac	WA	7497	12	(618)53827807	1938-04-01
472	Mellili	Robyn	W	4	133 Woodburne Ln	Umina	NSW	5581	12	(612)86364665	1943-04-04
473	Sears	Steven	I	5	13 Ritterman Crt	Kidman	VIC	5209	12	(613)77414751	1971-01-22
474	Rosenthal	Robyn	P	5	132 Leramonth St	Armidale	TAS	5161	12	(613)83588718	1968-02-11
475	Tonnibrook	Belinda	T	4	86 Patton St	Longwood	WA	6354	12	(618)68027086	1955-08-01
476	Dimitria	Larry	B	1	85 Keisling Rd	Montague	TAS	7819	12	(613)82158211	1943-04-04
477	Mettaxus	Melinda	T	5	35 Rosenthal Cres	Berala	WA	6982	12	(618)50712016	1962-10-13
478	Dimitria	Martin	E	1	94 Skerry St	Nareen	QLD	5252	12	(617)53345865	1971-01-11
479	Sorrenti	Nicholas	 	1	11 Mockridge Crt	Gray	QLD	8353	12	(617)70718711	1968-06-01
480	Patton	Joshua	B	1	45 Taggendharf Pl	Tarneit	NT	5078	12	(618)55012614	1966-08-04
481	Chester	Martin	E	5	150 Krennan Pl	Langwarrin	WA	8980	12	(618)78177372	1943-04-04
482	Sears	William	E	1	197 Florenini Cres	Tottenham	QLD	6730	12	(617)74843517	1971-01-11
483	Mellaseca	Anthony	A	1	106 Woodestock Pl	Sunbury	VIC	5065	12	(613)81826305	1943-04-21
484	Marzalla	Craig	B	1	85 Woodestock Crt	Rhodes	QLD	6193	12	(617)68501402	1968-02-04
485	Marzalla	Andrew	 	1	65 Belcombe Pl	Culgoa	NSW	7047	12	(612)74850632	1970-11-26
486	Dimitria	Joshua	W	1	192 Taggendharf Ln	Kidman	NSW	7693	12	(612)58810333	1980-02-11
487	Kinsala	Derryn	Y	1	97 Krennan Cres	Garvoc	NT	5773	12	(618)76463131	1938-04-01
488	Keisling	Betty	L	4	39 Serrong Rd	Earlwood	VIC	8445	12	(613)58257303	1967-01-24
489	Sears	Melissa	R	2	171 Florenini Ln	Garvoc	SA	5301	12	(618)75000168	1968-10-01
490	Rosenthal	Kym	 	4	94 Barneshaw Ccl	Mepunga	NSW	5338	12	(612)83886418	1955-08-11
491	Cassisi	Lynette	R	4	136 Krennan Ln	Bentley	WA	8289	12	(618)76836785	1981-10-01
492	Mellili	Hugh	R	1	90 Archibald Ccl	Ormond	VIC	5207	12	(613)88063560	1943-04-04
493	Marzalla	Melinda	R	2	120 Marzalla Ccl	Doveton	WA	8489	12	(618)65232061	1970-03-22
494	Woodburne	Dimitria	J	2	65 Belcombe Cres	McLaren	NT	6109	12	(618)63214788	1968-01-11
495	Ritterman	Rhiannon	E	5	149 Tonnibrook Crt	Kadina	QLD	6187	12	(617)68217074	1962-08-01
496	Rosenthal	Lynette	D	2	70 Ruscina Ln	Evandale	NSW	8601	12	(612)72240063	1943-04-04
497	Barneshaw	Jim	O	1	59 Woodburne Cres	Gray	VIC	6277	12	(613)71772403	1967-01-26
498	Lombardi	Cynthia	O	4	19 Eggelston Cres	Langwarrin	WA	8962	12	(618)54031172	1971-01-11
499	Marzalla	Karen	A	2	117 Sears Ln	Milvale	NSW	6537	12	(612)76827042	1968-10-01
500	Woodburne	Chris	D	1	150 Keisling Rd	Richmond	WA	6761	12	(618)78880340	1966-08-04
501	Belcombe	Andrew	D	1	113 Tonkin Pl	Tarneit	SA	7293	12	(618)58121107	1943-04-04
502	Serrong	Rhiannon	 	4	58 Rosenthal Pl	Graman	SA	5012	12	(618)83408581	1974-12-11
503	Oaton	Joel	 	1	55 Kinsala St	Hedley	NSW	5249	12	(612)78046500	1943-04-21
504	Eggelston	Joel	U	1	195 Mockridge St	Kulpi	NSW	6629	12	(612)72428778	1962-02-14
505	Nancarral	Larry	S	1	173 Oaton Pl	Gray	WA	6390	12	(618)74345416	1966-08-26
506	Archibald	Dimitria	Q	2	78 Sorrenti Ln	Athlone	VIC	7860	12	(613)50544575	1955-12-11
507	Patton	Anna	Y	2	105 Chester Pl	Graman	WA	6014	12	(618)75062450	1970-11-01
508	Ruscina	Joel	E	1	58 Woodburne Crt	Legana	WA	6341	12	(618)84713208	1980-06-24
509	Woodestock	Peter	A	1	117 Skerry Ln	Singleton	NSW	8938	12	(612)61874854	1968-01-01
510	Sorrenti	Joel	F	1	167 Rosenthal Ccl	Fernvale	NSW	5554	12	(612)63862287	1966-08-11
511	Florenini	Craig	A	1	184 Chester Rd	Kidman	TAS	8610	12	(613)77280811	1938-04-01
512	Keisling	Steven	L	1	137 Mettaxus Crt	Pambulah	VIC	6552	12	(613)84313326	1943-04-04
513	Lombardi	Michelle	 	2	92 Mellaseca Rd	Seabrook	QLD	8056	12	(617)78780113	1981-01-23
514	Krennan	Caitlyn	F	4	12 Mellaseca Ln	Milvale	TAS	7921	12	(613)80182102	1968-02-11
515	Oaton	Michael	R	1	53 Chemnis Ln	St Albans	NT	8049	12	(618)52144573	1962-08-01
516	Patton	Joshua	Q	1	58 Ruscina Pl	Berala	SA	8271	12	(618)78504083	1943-04-04
517	Mellaseca	Penelope	C	2	18 Cassisi Ln	St Albans	NSW	8634	12	(612)78501314	1922-13-26
518	Taggendharf	Betty	A	4	85 Kinsala Ccl	Chatswood	VIC	7050	12	(613)51117220	1974-12-11
519	Pattendon	Patty	A	2	47 Taggendharf Ln	Mininera	VIC	7816	12	(613)52877687	1968-06-01
520	Mellaseca	William	U	1	97 Mellaseca St	Kidman	SA	6876	12	(618)84482847	1966-08-04
521	Marzalla	Steven	D	1	4 Chemnis Crt	Evandale	WA	5459	12	(618)50826342	1955-10-13
522	Holdenson	Hugh	W	1	41 Rosenthal Ccl	Nabawa	TAS	6375	12	(613)50103457	1974-12-11
523	Chester	Robyn	O	2	161 Woodburne St	Stormlea	WA	6246	12	(618)66568207	1943-04-21
524	Titshall	James	Q	1	145 Patton Crt	Huntly	TAS	8785	12	(613)65447587	1968-01-04
525	Skerry	Rhiannon	F	4	115 Mellaseca Ln	Nyah	TAS	6990	12	(613)64740458	1966-08-26
526	Galti	Horacio	X	1	147 Krennan Ccl	Mininera	NSW	8222	12	(612)80777002	1962-02-11
527	Sears	Evonne	L	4	40 Mockridge Ln	Olinda	VIC	8626	12	(613)70542848	1938-04-01
528	Barneshaw	Caitlyn	I	2	1 Eggelston Pl	Coburg	VIC	5681	12	(613)53362261	1971-01-24
529	Mockridge	Anna	E	2	134 Archibald Pl	Belmont	VIC	6626	12	(613)85313518	1968-11-26
530	Lombardi	Penelope	R	2	99 Triskit Ln	Coburg	NT	8555	12	(618)86843674	1980-06-11
531	Nancarral	Michelle	G	4	193 Dimitria Ln	St Albans	SA	5469	12	(618)62228183	1938-04-01
532	Morfooney	Chris	U	1	89 Sears Ln	Umina	SA	6505	12	(618)82424213	1943-04-04
533	Patton	Jacob	T	1	12 Woodburne Cres	Belmont	SA	5838	12	(618)58056286	1971-01-22
534	Cassisi	Rhiannon	F	2	148 Galti Cres	Torquay	NSW	6904	12	(612)81854583	1968-02-11
535	Woodburne	Hugh	F	1	152 Nancarral Pl	Gobur	TAS	8450	12	(613)61577630	1981-08-01
536	Florenini	Melinda	A	4	175 Mettaxus St	Belmont	WA	7848	12	(618)61538776	1955-02-04
537	Marzalla	James	C	1	45 Mellili Rd	Bentley	TAS	7618	12	(613)65713875	1962-10-13
538	Stribling	Horacio	E	1	81 Woodestock Cres	Richmond	VIC	7832	12	(613)74563543	1974-12-11
539	Holdenson	Marie	N	5	21 Ruscina St	Athlone	SA	6039	12	(618)71256155	1968-01-07
540	Archibald	Bronwyn	Q	4	147 Lombardi Ln	Tabbita	NT	5590	12	(618)50376011	1966-08-04
541	Woodburne	Nicholas	I	1	24 Mettaxus Cres	Chatswood	NT	5552	12	(618)78134828	1967-01-26
542	Keisling	Joel	I	1	163 Kinsala Ln	Bentley	SA	5131	12	(618)66545722	1974-12-11
543	Mockridge	Cynthia	P	2	152 Morfooney Pl	Singleton	WA	8803	12	(618)76728818	1971-01-21
544	Mettaxus	Robyn	L	4	34 Taggendharf Pl	Gray	NT	7182	12	(618)85462878	1968-02-04
545	Mellili	Bronwyn	N	2	18 Stribling Ln	Westleigh	NT	7475	12	(618)52237811	1966-08-26
546	Woodburne	Patty	 	4	124 Cassisi Ccl	Montague	SA	7890	12	(618)53743654	1974-12-11
547	Mellili	Derryn	H	1	54 Galti Ln	Portsea	VIC	7029	12	(613)86223707	1938-04-01
548	Sorrenti	Horacio	O	1	37 Pattendon Rd	Fernvale	NT	6731	12	(618)55544438	1962-02-14
549	Sears	Rochelle	M	5	150 Nancarral Cres	Mohogany	QLD	8435	12	(617)88043727	1968-10-13
550	Florenini	Karen	Y	4	135 Galti Pl	Broadmeadows	TAS	7392	12	(613)67372606	1971-01-11
551	Skerry	Joel	 	5	153 Mellili Cres	Ormond	NSW	7753	12	(612)53560730	1955-11-01
552	Skerry	Chris	U	1	130 Chemnis Pl	Berala	NSW	7355	12	(612)71367764	1980-06-04
553	Tonkin	Melinda	I	4	198 Triskit Rd	Kalimna	NT	7603	12	(618)83165861	1943-04-21
554	Dimitria	Anthony	J	1	69 Serrong St	Armidale	NT	8986	12	(618)68388260	1968-30-11
555	Stribling	George	N	1	85 Galti Rd	Essendon	WA	5865	12	(618)80463302	1971-01-01
556	Dimitria	Rochelle	I	2	112 Mettaxus Pl	Stormlea	TAS	7103	12	(613)54142581	1943-04-04
557	Kinsala	Rochelle	Z	2	156 Mellaseca Crt	Nareen	WA	6710	12	(618)85241378	1981-02-03
558	Belcombe	Marie	K	2	50 Serrong Ccl	Lemont	QLD	7346	12	(617)56023271	1974-12-11
559	Chemnis	Horacio	C	1	1 Titshall Cres	Nareen	SA	8838	12	(618)73088055	1962-01-01
560	Dimitria	Joshua	L	1	187 Morfooney Crt	Garvoc	NT	7501	12	(618)78000538	1966-08-04
561	Chemnis	Lynette	I	2	150 Pattendon St	Seabrook	NSW	5670	12	(612)77143827	1939-10-13
562	Keisling	Penelope	Y	4	3 Titshall Rd	Kadina	TAS	5128	12	(613)67266865	1974-12-11
563	Taggendharf	George	Q	1	176 Pattendon Ln	Broadmeadows	SA	5707	12	(618)56486553	1967-01-21
564	Pattendon	Patty	 	4	114 Pattendon Crt	Mepunga	NT	5332	12	(618)57177880	1968-02-04
565	Woodestock	Joel	D	1	147 Dalion Ln	McLaren	NSW	5641	12	(612)65023622	1966-08-26
566	Keisling	Chris	S	1	73 Cassisi Pl	Nyah	VIC	5548	12	(613)76143770	1974-12-11
567	Archibald	Megan	S	2	109 Taggendharf Ln	Belmont	VIC	5579	12	(613)82330144	1955-10-01
568	Tonnibrook	Cynthia	U	2	93 Archibald Pl	Vasey	TAS	6547	12	(613)87738724	1943-04-24
569	Mockridge	Jacob	L	1	112 Serrong Cres	Mohogany	QLD	5718	12	(617)78246312	1968-10-13
570	Dimitria	Richard	H	1	43 Mellili St	Underwood	WA	8673	12	(618)78730306	1962-01-11
571	Mellaseca	Sandra	D	4	2 Eggelston Pl	St Albans	NSW	7651	12	(612)73464858	1938-04-01
572	Mellaseca	Peter	A	1	178 Kinsala Pl	Nyah	SA	6537	12	(618)50288566	1967-01-04
573	Sorrenti	Steven	J	1	48 Morfooney Ln	Westleigh	NSW	8330	12	(612)72027032	1970-11-21
574	Nancarral	Peter	Z	5	140 Leramonth St	Greensborough	NT	6249	12	(618)74231723	1980-01-11
575	Sears	Sandra	G	4	195 Cassisi Crt	Coburg	SA	8126	12	(618)76818846	1966-08-01
576	Marzalla	Perry	N	1	183 Serrong Pl	Evandale	TAS	5037	12	(613)82764087	1943-04-04
577	Nancarral	Marie	N	2	70 Cassisi Crt	Montague	QLD	7020	12	(617)88468637	1943-04-04
578	Dimitria	Robyn	Z	2	73 Pattendon Rd	Hedley	TAS	6153	12	(613)73210588	1974-12-11
579	Morfooney	Patty	X	2	93 Barneshaw St	Hedley	NT	5688	12	(618)51001815	1981-10-01
580	Eggelston	James	J	1	84 Nancarral Crt	Underwood	QLD	5443	12	(617)74414326	1966-08-04
581	Sears	Evonne	D	5	110 Woodburne Ln	Fernvale	WA	5045	12	(618)75010324	1943-04-02
582	Ritterman	Bronwyn	N	5	7 Pattendon Ln	Fitzroy	TAS	7412	12	(613)52307280	1955-02-11
583	Holdenson	Jasmine	F	2	58 Kinsala Ln	Longwood	SA	6795	12	(618)82080171	1967-01-21
584	Tonnibrook	George	D	1	153 Serrong St	Gobur	TAS	8642	12	(613)73254357	1968-02-04
585	Chemnis	Horacio	X	5	187 Archibald Cres	Coburg	SA	8143	12	(618)60773305	1966-08-03
586	Krennan	Mark	Q	1	115 Patton Rd	Singleton	VIC	5654	12	(613)55378261	1971-01-11
587	Oaton	Samantha	G	2	98 Krennan Ln	Westleigh	QLD	5147	12	(617)62604741	1938-04-01
588	Eggelston	Anthony	S	5	91 Chemnis St	Berala	SA	5266	12	(618)73606288	1943-04-24
589	Ritterman	James	N	1	32 Taggendharf Rd	Portsea	TAS	6639	12	(613)80055766	1968-10-13
590	Keisling	George	W	1	88 Ruscina Rd	Kalimna	QLD	8438	12	(617)73416706	1962-01-11
591	Kinsala	Jim	K	1	133 Serrong Ln	Tabbita	QLD	8394	12	(617)81118120	1938-04-01
592	Florenini	Melissa	Q	2	55 Serrong St	Lucaston	NSW	8611	12	(612)83134568	1965-10-14
593	Mellaseca	Nicholas	D	1	14 Archibald Pl	Milvale	VIC	6421	12	(613)83430515	1971-01-22
594	Ruscina	Mark	Z	1	15 Pattendon Rd	Kalimna	WA	7314	12	(618)85777577	1968-06-11
595	Keisling	Mark	E	1	78 Taggendharf Rd	Springfield	WA	6460	12	(618)64851473	1970-11-01
596	Patton	Kym	R	2	190 Dalion Pl	Mepunga	NT	6379	12	(618)63028174	1980-02-04
597	Galti	James	A	1	167 Keisling Crt	Lucknow	SA	5205	12	(618)62463586	1943-04-04
598	Ritterman	Perry	Q	1	10 Krennan Ccl	Alexandra	WA	5845	12	(618)58253322	1955-02-11
599	Eggelston	David	D	1	36 Skerry St	Nyah	SA	6984	12	(618)54707236	1968-10-01
600	Woodburne	Perry	O	1	88 Cassisi Cres	Karumba	SA	6025	12	(618)60521125	1966-08-04
601	Rosenthal	Karen	O	2	190 Sears Pl	Alexandra	TAS	8023	12	(613)58527662	1981-01-03
602	Leramonth	George	H	5	122 Krennan Pl	McLaren	TAS	8580	12	(613)64223438	1974-12-11
603	Tonnibrook	Chris	M	1	18 Triskit Cres	Nareen	NT	6944	12	(618)68762122	1943-04101
604	Barneshaw	Penelope	R	4	36 Chester St	Umina	VIC	6144	12	(613)81663822	1968-02-04
605	Ruscina	Kym	P	2	136 Leramonth St	Eleker	WA	5131	12	(618)60760085	1971-01-26
606	Mettaxus	Robyn	W	4	77 Taggendharf Pl	Montague	SA	8793	12	(618)87075235	1974-12-11
607	Dalion	Jim	D	1	2 Belcombe Ln	Fitzroy	SA	6262	12	(618)64836712	1938-04-01
608	Chester	Peter	V	5	189 Taggendharf St	Alexandra	VIC	6437	12	(613)82166210	1943-04-24
609	Sears	Caitlyn	G	2	165 Mellili Crt	Bentley	NSW	8198	12	(612)61511844	1968-10-13
610	Galti	Nicholas	Y	1	163 Dimitria Ccl	Graman	VIC	5084	12	(613)54523558	1966-08-11
611	Tonkin	Melinda	C	2	72 Stribling Ln	Olinda	WA	7348	12	(618)80105875	1938-04-01
612	Dimitria	Martin	F	1	32 Keisling Pl	Kalimna	VIC	5598	12	(613)70816381	1962-02-04
613	Mockridge	Lynette	W	5	55 Holdenson Rd	Montague	SA	8263	12	(618)66405581	1955-01-22
614	Tonkin	Melissa	D	2	85 Tonnibrook Ccl	Garvoc	VIC	7070	12	(613)63530037	1968-01-11
615	Morfooney	Perry	L	1	84 Taggendharf St	Huntly	NT	8181	12	(618)82123553	1966-08-01
616	Skerry	Lynette	Y	5	11 Archibald Ccl	Berala	NSW	8001	12	(612)52488518	1971-01-04
617	Sorrenti	Joel	T	1	163 Rosenthal St	Colac	NT	8650	12	(618)87576351	1970-11-01
618	Ruscina	Joshua	B	1	65 Galti Rd	Bauple	VIC	8316	12	(613)56470036	1980-02-11
619	Ruscina	Karen	K	4	114 Nancarral Cres	Nareen	VIC	7013	12	(613)78438784	1968-10-01
620	Florenini	Michelle	H	2	196 Mellaseca St	Fitzroy	NT	5085	12	(618)53884037	1971-01-04
621	Chester	Perry	S	1	147 Eggelston Rd	Alexandra	NT	5354	12	(618)64815524	1943-04-04
622	Serrong	Peter	Q	5	170 Holdenson St	Essendon	VIC	7441	12	(613)65661832	1974-12-11
623	Mockridge	Anthony	T	1	120 Belcombe Ln	Tara	NSW	6964	12	(612)62204054	1981-02-21
624	Kinsala	Caitlyn	N	4	131 Ruscina Pl	Bentley	NSW	7470	12	(612)80021501	1968-02-04
625	Lombardi	William	B	1	7 Keisling St	Mininera	VIC	7012	12	(613)51422013	1971-01-12
626	Woodburne	James	Q	1	134 Chemnis Crt	Kidman	SA	7487	12	(618)81802164	1974-12-11
627	Rosenthal	Jasmine	N	2	187 Taggendharf Crt	Rhodes	WA	5193	12	(618)58541255	1967-01-01
628	Oaton	Samantha	G	2	142 Kinsala Pl	Greensborough	QLD	7346	12	(617)88701573	1955-02-24
629	Sorrenti	Lynette	M	2	130 Eggelston Cres	Sunbury	NSW	8477	12	(612)65757214	1968-10-13
630	Archibald	Belinda	Q	2	36 Florenini Ccl	Coonawarra	WA	8833	12	(618)78101124	1966-08-11
631	Ruscina	Rochelle	K	2	172 Morfooney Ln	Garvoc	NT	6080	12	(618)58027027	1971-01-01
632	Mettaxus	Hugh	C	1	165 Holdenson St	McLaren	NSW	5021	12	(612)52374807	1943-04-04
633	Mellaseca	Horacio	X	1	124 Mettaxus St	Athlone	NT	5062	12	(618)60074577	1971-01-22
634	Pattendon	Lynette	Z	4	77 Morfooney Rd	Tottenham	WA	6516	12	(618)72402546	1962-02-11
635	Dalion	Steven	Q	1	69 Tonnibrook Ccl	Lucaston	NT	6410	12	(618)75105015	1971-01-01
636	Krennan	Harry	Q	1	161 Galti Rd	Lucknow	TAS	7372	12	(613)87255625	1967-01-14
637	Dalion	Bronwyn	H	4	103 Holdenson Crt	Kidman	QLD	8867	12	(617)88332246	1938-04-01
638	Oaton	Lynette	E	5	188 Keisling Cres	Lucaston	NSW	6891	12	(612)82503556	1974-12-11
639	Lombardi	Derryn	S	1	26 Mockridge Ln	Mepunga	NT	6272	12	(618)72200811	1968-11-01
640	Ritterman	Evonne	D	2	57 Lombardi St	Armidale	NSW	6549	12	(612)86885646	1966-08-04
641	Skerry	James	A	1	164 Galti Ccl	Halton	NT	7444	12	(618)53361482	1943-04-04
642	Dalion	Richard	E	1	37 Kinsala St	Evandale	TAS	7188	12	(613)75051408	1974-12-11
643	Taggendharf	Martin	A	1	24 Rosenthal Pl	Westleigh	QLD	7492	12	(617)74328363	1955-01-22
644	Keisling	Mark	O	1	75 Barneshaw Crt	Portsea	NT	6207	12	(618)73636150	1968-10-13
645	Holdenson	Bronwyn	C	2	27 Mettaxus Pl	Earlwood	SA	8754	12	(618)52873382	1966-08-26
646	Stribling	Michelle	Q	4	75 Patton Rd	McLaren	VIC	8810	12	(613)70463665	1943-04-04
647	Skerry	Samantha	J	2	120 Eggelston Ln	Montague	NSW	8497	12	(612)85133166	1943-04-04
648	Cassisi	Betty	Y	4	115 Morfooney Ln	Montague	VIC	7192	12	(613)63535037	1971-01-22
649	Krennan	Jim	D	1	166 Kinsala St	Pambulah	QLD	5764	12	(617)60001301	1943-04-04
650	Woodburne	Lynette	H	2	98 Marzalla St	Alexandra	SA	8412	12	(618)71264774	1943-04-04
\.


--
-- Data for Name: grape_variety; Type: TABLE DATA; Schema: public; Owner: -
--

COPY grape_variety (variety_id, variety) FROM stdin;
2	Chardonnay
3	Sauvignon
4	Blanc
5	Semillon
6	Pinot
7	Gris
8	Verdelho
9	Grenache
10	Noir
11	Cabernet
12	Shiraz
13	Merlot
14	Dessert
15	Muscat
16	Sherry
17	Port
18	Champagne
19	Sparkling
20	Red
21	White
\.


--
-- Data for Name: inventory; Type: TABLE DATA; Schema: public; Owner: -
--

COPY inventory (wine_id, inventory_id, on_hand, cost, date_added) FROM stdin;
1	1	956	21.18	2004-03-13
2	1	468	28.15	2004-03-13
3	1	601	9.94	2004-03-13
4	1	518	21.16	2004-03-13
5	1	280	6.67	2004-03-13
6	1	813	20.25	2004-03-13
7	1	713	12.20	2004-03-13
8	1	287	10.06	2004-03-13
9	1	850	24.47	2004-03-13
10	1	221	17.82	2004-03-13
11	1	381	16.48	2004-03-13
12	1	564	27.42	2004-03-13
13	1	720	17.60	2004-03-13
14	1	666	5.27	2004-03-13
15	1	836	12.33	2004-03-13
16	1	507	8.95	2004-03-13
17	1	762	25.23	2004-03-13
18	1	964	8.45	2004-03-13
19	1	33	29.22	2004-03-13
20	1	505	9.70	2004-03-13
21	1	333	19.14	2004-03-13
22	1	817	8.05	2004-03-13
23	1	247	26.56	2004-03-13
24	1	849	18.60	2004-03-13
25	1	120	14.91	2004-03-13
26	1	456	26.03	2004-03-13
27	1	408	20.87	2004-03-13
28	1	645	19.99	2004-03-13
29	1	99	20.97	2004-03-13
30	1	322	8.16	2004-03-13
31	1	30	16.74	2004-03-13
32	1	178	12.88	2004-03-13
33	1	237	18.75	2004-03-13
34	1	806	14.28	2004-03-13
35	1	407	17.64	2004-03-13
36	1	317	29.51	2004-03-13
37	1	352	14.15	2004-03-13
38	1	223	5.75	2004-03-13
39	1	220	7.38	2004-03-13
40	1	191	15.52	2004-03-13
41	1	359	16.99	2004-03-13
42	1	106	21.28	2004-03-13
43	1	995	27.82	2004-03-13
44	1	747	22.20	2004-03-13
45	1	27	7.71	2004-03-13
46	1	673	15.67	2004-03-13
47	1	192	24.04	2004-03-13
48	1	346	29.06	2004-03-13
49	1	99	10.18	2004-03-13
50	1	485	15.62	2004-03-13
51	1	10	24.09	2004-03-13
52	1	5	26.73	2004-03-13
53	1	290	26.62	2004-03-13
54	1	110	20.31	2004-03-13
55	1	316	21.41	2004-03-13
56	1	28	12.83	2004-03-13
57	1	701	17.60	2004-03-13
58	1	700	26.60	2004-03-13
59	1	801	28.81	2004-03-13
60	1	995	16.21	2004-03-13
61	1	615	16.98	2004-03-13
62	1	706	16.85	2004-03-13
63	1	445	23.72	2004-03-13
64	1	71	26.33	2004-03-13
65	1	390	9.98	2004-03-13
66	1	809	10.29	2004-03-13
67	1	646	27.80	2004-03-13
68	1	252	21.66	2004-03-13
69	1	853	16.61	2004-03-13
70	1	667	16.05	2004-03-13
71	1	887	7.50	2004-03-13
72	1	871	24.96	2004-03-13
73	1	728	10.70	2004-03-13
74	1	452	12.56	2004-03-13
75	1	791	22.73	2004-03-13
76	1	772	17.94	2004-03-13
77	1	69	8.84	2004-03-13
78	1	875	15.50	2004-03-13
79	1	536	11.97	2004-03-13
80	1	152	28.44	2004-03-13
81	1	207	25.52	2004-03-13
82	1	713	22.30	2004-03-13
83	1	676	15.54	2004-03-13
84	1	384	26.84	2004-03-13
85	1	950	21.54	2004-03-13
86	1	690	15.04	2004-03-13
87	1	632	17.19	2004-03-13
88	1	480	14.59	2004-03-13
89	1	985	27.85	2004-03-13
90	1	960	6.77	2004-03-13
91	1	401	11.58	2004-03-13
92	1	922	29.26	2004-03-13
93	1	231	21.67	2004-03-13
94	1	773	6.53	2004-03-13
95	1	619	29.19	2004-03-13
96	1	159	22.62	2004-03-13
97	1	203	12.11	2004-03-13
98	1	396	24.49	2004-03-13
99	1	992	24.40	2004-03-13
100	1	843	23.46	2004-03-13
101	1	617	8.73	2004-03-13
102	1	201	6.71	2004-03-13
103	1	466	12.37	2004-03-13
104	1	142	6.42	2004-03-13
105	1	681	22.41	2004-03-13
106	1	876	26.10	2004-03-13
107	1	498	6.41	2004-03-13
108	1	616	16.21	2004-03-13
109	1	93	16.00	2004-03-13
110	1	510	12.74	2004-03-13
111	1	973	27.88	2004-03-13
112	1	706	7.95	2004-03-13
113	1	422	19.25	2004-03-13
114	1	799	22.32	2004-03-13
115	1	219	14.02	2004-03-13
116	1	635	11.87	2004-03-13
117	1	736	24.93	2004-03-13
118	1	155	14.38	2004-03-13
119	1	372	17.82	2004-03-13
120	1	373	5.11	2004-03-13
121	1	117	29.02	2004-03-13
122	1	7	22.13	2004-03-13
123	1	963	7.70	2004-03-13
124	1	220	27.42	2004-03-13
125	1	549	8.82	2004-03-13
126	1	571	24.09	2004-03-13
127	1	445	17.86	2004-03-13
128	1	376	21.25	2004-03-13
129	1	759	5.75	2004-03-13
130	1	587	15.76	2004-03-13
131	1	705	18.74	2004-03-13
132	1	729	18.43	2004-03-13
133	1	860	17.54	2004-03-13
134	1	566	22.45	2004-03-13
135	1	988	11.58	2004-03-13
136	1	808	25.35	2004-03-13
137	1	904	12.85	2004-03-13
138	1	772	21.51	2004-03-13
139	1	30	15.54	2004-03-13
140	1	786	5.95	2004-03-13
141	1	551	22.47	2004-03-13
142	1	299	15.00	2004-03-13
143	1	735	12.49	2004-03-13
144	1	282	26.21	2004-03-13
145	1	741	26.47	2004-03-13
146	1	330	21.30	2004-03-13
147	1	685	19.58	2004-03-13
148	1	543	13.24	2004-03-13
149	1	60	14.66	2004-03-13
150	1	607	8.66	2004-03-13
151	1	560	22.37	2004-03-13
152	1	35	5.81	2004-03-13
153	1	39	23.11	2004-03-13
154	1	537	5.97	2004-03-13
155	1	749	24.83	2004-03-13
156	1	224	7.49	2004-03-13
157	1	533	24.06	2004-03-13
158	1	123	13.39	2004-03-13
159	1	369	14.76	2004-03-13
160	1	30	29.92	2004-03-13
161	1	695	18.63	2004-03-13
162	1	69	19.64	2004-03-13
163	1	882	20.38	2004-03-13
164	1	994	9.40	2004-03-13
165	1	971	27.17	2004-03-13
166	1	802	8.26	2004-03-13
167	1	296	21.05	2004-03-13
168	1	357	24.37	2004-03-13
169	1	756	9.75	2004-03-13
170	1	525	9.75	2004-03-13
171	1	451	27.30	2004-03-13
172	1	85	29.89	2004-03-13
173	1	724	26.52	2004-03-13
174	1	93	25.68	2004-03-13
175	1	514	25.60	2004-03-13
176	1	687	20.74	2004-03-13
177	1	956	27.41	2004-03-13
178	1	6	11.21	2004-03-13
179	1	381	5.38	2004-03-13
180	1	141	18.57	2004-03-13
181	1	894	5.47	2004-03-13
182	1	199	7.43	2004-03-13
183	1	93	25.27	2004-03-13
184	1	614	27.11	2004-03-13
185	1	301	15.92	2004-03-13
186	1	904	8.53	2004-03-13
187	1	647	6.44	2004-03-13
188	1	65	7.71	2004-03-13
189	1	280	16.73	2004-03-13
190	1	661	20.28	2004-03-13
191	1	228	17.29	2004-03-13
191	2	12	17.50	2004-03-13
192	1	710	22.37	2004-03-13
193	1	143	24.18	2004-03-13
194	1	408	15.10	2004-03-13
195	1	535	22.53	2004-03-13
196	1	461	20.65	2004-03-13
197	1	999	11.73	2004-03-13
198	1	447	13.81	2004-03-13
199	1	163	20.28	2004-03-13
200	1	903	11.88	2004-03-13
201	1	193	17.38	2004-03-13
202	1	484	10.08	2004-03-13
203	1	861	14.87	2004-03-13
204	1	48	24.28	2004-03-13
205	1	154	12.99	2004-03-13
206	1	692	24.60	2004-03-13
207	1	349	24.87	2004-03-13
208	1	84	10.19	2004-03-13
209	1	901	14.80	2004-03-13
210	1	183	12.07	2004-03-13
211	1	70	17.58	2004-03-13
212	1	457	29.80	2004-03-13
213	1	544	25.17	2004-03-13
214	1	423	27.95	2004-03-13
215	1	709	5.27	2004-03-13
216	1	705	23.05	2004-03-13
217	1	315	6.83	2004-03-13
218	1	187	9.06	2004-03-13
219	1	386	6.45	2004-03-13
220	1	193	11.74	2004-03-13
221	1	270	12.23	2004-03-13
222	1	856	24.20	2004-03-13
223	1	662	13.04	2004-03-13
224	1	832	23.92	2004-03-13
225	1	786	16.96	2004-03-13
226	1	60	8.68	2004-03-13
227	1	115	16.76	2004-03-13
228	1	950	21.93	2004-03-13
229	1	14	6.99	2004-03-13
230	1	468	11.24	2004-03-13
231	1	218	17.56	2004-03-13
232	1	455	14.33	2004-03-13
233	1	664	5.65	2004-03-13
234	1	706	26.40	2004-03-13
235	1	631	21.85	2004-03-13
236	1	592	22.01	2004-03-13
237	1	281	25.48	2004-03-13
238	1	49	24.15	2004-03-13
239	1	980	24.19	2004-03-13
240	1	976	11.44	2004-03-13
241	1	219	16.95	2004-03-13
242	1	781	28.32	2004-03-13
243	1	119	19.35	2004-03-13
244	1	777	12.21	2004-03-13
245	1	179	28.86	2004-03-13
246	1	34	18.57	2004-03-13
247	1	49	25.40	2004-03-13
248	1	539	26.18	2004-03-13
249	1	690	25.97	2004-03-13
250	1	890	25.23	2004-03-13
251	1	332	5.71	2004-03-13
252	1	286	25.65	2004-03-13
253	1	609	28.24	2004-03-13
254	1	964	22.55	2004-03-13
255	1	181	27.48	2004-03-13
256	1	50	28.01	2004-03-13
257	1	630	29.82	2004-03-13
258	1	37	17.68	2004-03-13
259	1	221	10.52	2004-03-13
260	1	198	7.34	2004-03-13
261	1	436	15.37	2004-03-13
262	1	152	22.19	2004-03-13
263	1	803	13.12	2004-03-13
264	1	51	12.84	2004-03-13
265	1	604	16.73	2004-03-13
266	1	92	18.66	2004-03-13
267	1	108	8.63	2004-03-13
268	1	933	9.15	2004-03-13
269	1	618	15.36	2004-03-13
270	1	141	20.46	2004-03-13
271	1	481	6.89	2004-03-13
272	1	857	9.60	2004-03-13
273	1	50	5.93	2004-03-13
274	1	179	6.22	2004-03-13
275	1	20	21.69	2004-03-13
276	1	755	23.22	2004-03-13
277	1	416	26.37	2004-03-13
278	1	55	24.56	2004-03-13
279	1	211	15.97	2004-03-13
280	1	574	18.58	2004-03-13
281	1	34	5.63	2004-03-13
282	1	925	27.83	2004-03-13
283	1	532	28.83	2004-03-13
284	1	466	8.89	2004-03-13
285	1	691	8.54	2004-03-13
286	1	388	27.29	2004-03-13
287	1	608	12.15	2004-03-13
288	1	982	14.95	2004-03-13
289	1	546	19.36	2004-03-13
290	1	522	8.91	2004-03-13
291	1	109	9.95	2004-03-13
292	1	743	7.84	2004-03-13
293	1	577	21.79	2004-03-13
294	1	413	7.91	2004-03-13
295	1	303	25.05	2004-03-13
296	1	852	9.95	2004-03-13
297	1	152	25.27	2004-03-13
298	1	312	24.64	2004-03-13
299	1	265	13.23	2004-03-13
300	1	631	26.38	2004-03-13
301	1	550	29.58	2004-03-13
302	1	505	22.70	2004-03-13
303	1	353	12.35	2004-03-13
304	1	145	22.46	2004-03-13
305	1	61	5.27	2004-03-13
306	1	120	26.47	2004-03-13
307	1	79	5.28	2004-03-13
308	1	354	13.06	2004-03-13
309	1	1	19.40	2004-03-13
310	1	232	10.26	2004-03-13
311	1	621	24.22	2004-03-13
312	1	203	11.93	2004-03-13
313	1	904	20.22	2004-03-13
314	1	255	24.92	2004-03-13
315	1	724	25.93	2004-03-13
316	1	1	28.78	2004-03-13
317	1	475	5.79	2004-03-13
318	1	755	14.12	2004-03-13
319	1	824	14.70	2004-03-13
320	1	227	20.92	2004-03-13
321	1	488	11.69	2004-03-13
322	1	717	16.97	2004-03-13
323	1	655	13.60	2004-03-13
324	1	882	20.37	2004-03-13
325	1	809	13.21	2004-03-13
326	1	742	13.16	2004-03-13
327	1	531	25.20	2004-03-13
328	1	331	19.59	2004-03-13
329	1	453	14.98	2004-03-13
330	1	163	20.60	2004-03-13
331	1	92	7.84	2004-03-13
332	1	790	25.29	2004-03-13
333	1	763	16.64	2004-03-13
334	1	364	14.25	2004-03-13
335	1	740	9.58	2004-03-13
336	1	950	27.91	2004-03-13
337	1	117	16.72	2004-03-13
338	1	318	24.28	2004-03-13
339	1	908	10.67	2004-03-13
340	1	889	14.29	2004-03-13
341	1	492	17.40	2004-03-13
342	1	339	11.22	2004-03-13
343	1	633	5.70	2004-03-13
344	1	279	15.06	2004-03-13
345	1	531	8.23	2004-03-13
346	1	114	7.14	2004-03-13
347	1	30	10.59	2004-03-13
348	1	755	18.36	2004-03-13
349	1	44	15.59	2004-03-13
350	1	723	13.99	2004-03-13
351	1	19	25.47	2004-03-13
352	1	421	9.75	2004-03-13
353	1	645	24.79	2004-03-13
354	1	348	6.19	2004-03-13
355	1	412	20.33	2004-03-13
356	1	647	22.43	2004-03-13
357	1	794	23.64	2004-03-13
358	1	387	17.17	2004-03-13
359	1	755	18.61	2004-03-13
360	1	354	11.94	2004-03-13
361	1	346	12.76	2004-03-13
362	1	823	22.92	2004-03-13
363	1	575	14.72	2004-03-13
364	1	367	22.79	2004-03-13
365	1	563	8.40	2004-03-13
366	1	903	24.12	2004-03-13
367	1	338	19.87	2004-03-13
368	1	984	28.37	2004-03-13
369	1	161	22.92	2004-03-13
370	1	777	6.20	2004-03-13
371	1	722	15.49	2004-03-13
372	1	489	10.21	2004-03-13
373	1	417	25.57	2004-03-13
374	1	141	9.28	2004-03-13
375	1	939	23.40	2004-03-13
376	1	343	21.17	2004-03-13
377	1	466	29.73	2004-03-13
378	1	340	29.68	2004-03-13
379	1	719	8.39	2004-03-13
380	1	648	27.45	2004-03-13
381	1	852	20.86	2004-03-13
382	1	349	7.09	2004-03-13
383	1	561	18.89	2004-03-13
384	1	133	21.07	2004-03-13
385	1	415	20.81	2004-03-13
386	1	817	28.45	2004-03-13
387	1	989	15.43	2004-03-13
388	1	724	24.39	2004-03-13
389	1	410	17.52	2004-03-13
390	1	803	8.43	2004-03-13
391	1	257	29.05	2004-03-13
392	1	119	13.17	2004-03-13
393	1	212	27.70	2004-03-13
394	1	111	5.38	2004-03-13
395	1	185	16.99	2004-03-13
396	1	998	11.95	2004-03-13
397	1	452	11.38	2004-03-13
398	1	886	29.14	2004-03-13
399	1	285	14.63	2004-03-13
400	1	811	9.76	2004-03-13
401	1	241	16.75	2004-03-13
402	1	298	19.08	2004-03-13
403	1	471	24.98	2004-03-13
404	1	178	27.01	2004-03-13
405	1	560	6.46	2004-03-13
406	1	558	6.72	2004-03-13
407	1	514	18.98	2004-03-13
408	1	896	15.95	2004-03-13
409	1	43	22.34	2004-03-13
410	1	675	17.54	2004-03-13
411	1	440	23.99	2004-03-13
412	1	192	6.56	2004-03-13
413	1	278	18.34	2004-03-13
414	1	263	22.23	2004-03-13
415	1	278	11.31	2004-03-13
416	1	740	25.39	2004-03-13
417	1	467	13.92	2004-03-13
418	1	447	10.33	2004-03-13
419	1	405	11.04	2004-03-13
420	1	753	28.55	2004-03-13
421	1	294	14.30	2004-03-13
422	1	286	19.36	2004-03-13
423	1	847	26.88	2004-03-13
424	1	190	8.58	2004-03-13
425	1	426	15.85	2004-03-13
426	1	742	8.67	2004-03-13
427	1	135	20.01	2004-03-13
428	1	58	6.24	2004-03-13
429	1	397	16.52	2004-03-13
430	1	437	10.47	2004-03-13
431	1	991	17.86	2004-03-13
432	1	300	25.45	2004-03-13
433	1	662	21.31	2004-03-13
434	1	548	25.44	2004-03-13
435	1	137	11.85	2004-03-13
436	1	991	5.99	2004-03-13
437	1	666	8.61	2004-03-13
438	1	679	10.67	2004-03-13
439	1	926	19.13	2004-03-13
440	1	449	5.35	2004-03-13
441	1	925	5.87	2004-03-13
442	1	794	13.67	2004-03-13
443	1	248	29.32	2004-03-13
444	1	566	18.16	2004-03-13
445	1	11	21.43	2004-03-13
446	1	862	21.44	2004-03-13
447	1	860	13.58	2004-03-13
448	1	471	5.78	2004-03-13
449	1	144	28.38	2004-03-13
450	1	283	14.42	2004-03-13
451	1	643	12.45	2004-03-13
452	1	518	7.78	2004-03-13
453	1	195	14.77	2004-03-13
454	1	521	28.31	2004-03-13
455	1	686	28.99	2004-03-13
456	1	117	25.66	2004-03-13
457	1	558	21.00	2004-03-13
458	1	142	8.01	2004-03-13
459	1	929	11.96	2004-03-13
460	1	316	19.35	2004-03-13
461	1	741	10.66	2004-03-13
462	1	195	23.78	2004-03-13
463	1	980	27.14	2004-03-13
464	1	757	15.07	2004-03-13
465	1	74	8.00	2004-03-13
466	1	70	11.81	2004-03-13
467	1	736	25.47	2004-03-13
468	1	738	28.60	2004-03-13
469	1	757	26.88	2004-03-13
470	1	717	9.44	2004-03-13
471	1	69	28.64	2004-03-13
472	1	739	8.36	2004-03-13
473	1	521	7.28	2004-03-13
474	1	664	26.72	2004-03-13
475	1	735	28.84	2004-03-13
476	1	744	5.09	2004-03-13
477	1	554	23.89	2004-03-13
478	1	25	17.83	2004-03-13
479	1	630	11.70	2004-03-13
480	1	512	28.89	2004-03-13
481	1	954	5.82	2004-03-13
482	1	600	24.95	2004-03-13
483	1	968	12.06	2004-03-13
484	1	253	28.92	2004-03-13
485	1	976	22.86	2004-03-13
486	1	824	15.87	2004-03-13
487	1	915	29.62	2004-03-13
488	1	807	5.10	2004-03-13
489	1	641	21.35	2004-03-13
490	1	349	8.31	2004-03-13
491	1	246	7.33	2004-03-13
492	1	74	22.10	2004-03-13
493	1	28	22.39	2004-03-13
494	1	90	28.35	2004-03-13
495	1	16	27.04	2004-03-13
496	1	882	23.47	2004-03-13
497	1	757	21.46	2004-03-13
498	1	379	16.48	2004-03-13
499	1	487	7.12	2004-03-13
500	1	837	16.28	2004-03-13
501	1	147	22.39	2004-03-13
502	1	487	24.11	2004-03-13
503	1	596	24.79	2004-03-13
504	1	995	27.72	2004-03-13
505	1	349	27.63	2004-03-13
506	1	394	24.37	2004-03-13
507	1	873	18.02	2004-03-13
508	1	342	28.48	2004-03-13
509	1	825	15.58	2004-03-13
510	1	955	11.03	2004-03-13
511	1	242	14.72	2004-03-13
512	1	642	27.49	2004-03-13
513	1	754	17.29	2004-03-13
514	1	548	16.18	2004-03-13
515	1	275	25.09	2004-03-13
516	1	913	10.87	2004-03-13
517	1	287	7.42	2004-03-13
518	1	311	15.35	2004-03-13
519	1	793	11.76	2004-03-13
520	1	376	10.01	2004-03-13
521	1	407	16.41	2004-03-13
522	1	134	7.30	2004-03-13
523	1	631	26.78	2004-03-13
524	1	927	15.26	2004-03-13
525	1	112	22.09	2004-03-13
526	1	203	19.31	2004-03-13
527	1	83	27.40	2004-03-13
528	1	509	9.84	2004-03-13
529	1	861	29.10	2004-03-13
530	1	627	14.32	2004-03-13
531	1	644	24.78	2004-03-13
532	1	60	27.88	2004-03-13
533	1	362	18.74	2004-03-13
534	1	147	16.15	2004-03-13
535	1	410	19.83	2004-03-13
536	1	500	24.42	2004-03-13
537	1	811	26.30	2004-03-13
538	1	217	14.50	2004-03-13
539	1	60	10.48	2004-03-13
540	1	166	26.85	2004-03-13
541	1	489	17.62	2004-03-13
542	1	273	19.52	2004-03-13
543	1	160	28.06	2004-03-13
544	1	905	6.53	2004-03-13
545	1	55	17.10	2004-03-13
546	1	887	29.88	2004-03-13
547	1	314	24.75	2004-03-13
548	1	622	5.12	2004-03-13
549	1	124	6.36	2004-03-13
550	1	613	11.22	2004-03-13
551	1	466	23.73	2004-03-13
552	1	89	5.30	2004-03-13
553	1	617	9.23	2004-03-13
554	1	672	6.30	2004-03-13
555	1	204	8.79	2004-03-13
556	1	323	5.78	2004-03-13
557	1	188	13.58	2004-03-13
558	1	965	28.59	2004-03-13
559	1	91	6.67	2004-03-13
560	1	453	22.91	2004-03-13
561	1	678	8.64	2004-03-13
562	1	411	11.65	2004-03-13
563	1	712	26.82	2004-03-13
564	1	942	18.89	2004-03-13
565	1	250	24.02	2004-03-13
566	1	44	7.04	2004-03-13
567	1	827	12.37	2004-03-13
568	1	408	11.32	2004-03-13
569	1	5	13.40	2004-03-13
570	1	680	25.09	2004-03-13
571	1	597	17.25	2004-03-13
572	1	247	27.49	2004-03-13
573	1	903	20.36	2004-03-13
574	1	726	18.91	2004-03-13
575	1	622	25.67	2004-03-13
576	1	646	26.76	2004-03-13
577	1	686	21.06	2004-03-13
578	1	57	7.32	2004-03-13
579	1	381	7.68	2004-03-13
580	1	783	24.49	2004-03-13
581	1	877	14.98	2004-03-13
582	1	128	20.88	2004-03-13
583	1	627	18.49	2004-03-13
584	1	991	11.05	2004-03-13
585	1	904	26.85	2004-03-13
586	1	581	17.70	2004-03-13
587	1	345	10.38	2004-03-13
588	1	409	11.46	2004-03-13
589	1	737	21.56	2004-03-13
590	1	421	15.33	2004-03-13
591	1	914	12.70	2004-03-13
592	1	896	15.32	2004-03-13
593	1	779	6.84	2004-03-13
594	1	274	6.08	2004-03-13
595	1	324	28.95	2004-03-13
596	1	180	18.47	2004-03-13
597	1	886	27.67	2004-03-13
598	1	270	12.93	2004-03-13
599	1	740	5.57	2004-03-13
600	1	301	16.99	2004-03-13
601	1	635	14.29	2004-03-13
602	1	339	11.72	2004-03-13
603	1	659	5.78	2004-03-13
604	1	500	12.83	2004-03-13
605	1	588	20.78	2004-03-13
606	1	544	25.31	2004-03-13
607	1	16	22.19	2004-03-13
608	1	689	29.48	2004-03-13
609	1	622	22.51	2004-03-13
610	1	194	5.96	2004-03-13
611	1	190	21.32	2004-03-13
612	1	486	29.58	2004-03-13
613	1	211	21.62	2004-03-13
614	1	993	8.93	2004-03-13
615	1	823	23.57	2004-03-13
616	1	608	12.39	2004-03-13
617	1	927	12.09	2004-03-13
618	1	126	29.07	2004-03-13
619	1	615	20.79	2004-03-13
620	1	132	25.80	2004-03-13
621	1	96	29.46	2004-03-13
622	1	423	17.45	2004-03-13
623	1	11	22.10	2004-03-13
624	1	637	22.25	2004-03-13
625	1	576	17.99	2004-03-13
626	1	153	8.28	2004-03-13
627	1	548	6.58	2004-03-13
628	1	648	9.57	2004-03-13
629	1	723	24.19	2004-03-13
630	1	513	28.00	2004-03-13
631	1	320	5.00	2004-03-13
632	1	780	16.48	2004-03-13
633	1	233	16.18	2004-03-13
634	1	337	7.87	2004-03-13
635	1	594	21.39	2004-03-13
636	1	592	25.51	2004-03-13
637	1	876	14.56	2004-03-13
638	1	462	5.73	2004-03-13
639	1	41	23.53	2004-03-13
640	1	234	11.45	2004-03-13
641	1	237	19.47	2004-03-13
642	1	240	13.91	2004-03-13
643	1	728	19.37	2004-03-13
644	1	914	28.46	2004-03-13
645	1	790	18.13	2004-03-13
646	1	181	7.95	2004-03-13
647	1	694	29.32	2004-03-13
648	1	256	16.34	2004-03-13
649	1	994	17.09	2004-03-13
650	1	525	23.34	2004-03-13
651	1	979	29.92	2004-03-13
652	1	453	10.28	2004-03-13
653	1	372	28.87	2004-03-13
654	1	747	26.80	2004-03-13
655	1	215	21.32	2004-03-13
656	1	352	25.43	2004-03-13
657	1	311	17.43	2004-03-13
658	1	640	24.88	2004-03-13
659	1	990	24.38	2004-03-13
660	1	81	12.11	2004-03-13
661	1	428	11.08	2004-03-13
662	1	352	23.29	2004-03-13
663	1	633	7.23	2004-03-13
664	1	505	26.08	2004-03-13
665	1	306	6.72	2004-03-13
666	1	630	15.55	2004-03-13
667	1	70	23.07	2004-03-13
668	1	503	14.85	2004-03-13
669	1	390	13.62	2004-03-13
670	1	774	16.91	2004-03-13
671	1	890	26.19	2004-03-13
672	1	976	10.92	2004-03-13
673	1	270	26.02	2004-03-13
674	1	503	14.87	2004-03-13
675	1	589	21.93	2004-03-13
676	1	319	12.35	2004-03-13
677	1	874	14.08	2004-03-13
678	1	949	26.45	2004-03-13
679	1	799	11.81	2004-03-13
680	1	420	5.25	2004-03-13
681	1	337	28.12	2004-03-13
682	1	529	27.34	2004-03-13
683	1	450	9.19	2004-03-13
684	1	473	22.55	2004-03-13
685	1	345	11.22	2004-03-13
686	1	283	19.52	2004-03-13
687	1	637	16.89	2004-03-13
688	1	719	15.77	2004-03-13
689	1	170	10.03	2004-03-13
690	1	981	20.07	2004-03-13
691	1	232	29.00	2004-03-13
692	1	352	17.90	2004-03-13
693	1	694	28.08	2004-03-13
694	1	892	15.76	2004-03-13
695	1	452	24.03	2004-03-13
696	1	680	6.62	2004-03-13
697	1	207	22.09	2004-03-13
698	1	104	15.50	2004-03-13
699	1	875	19.71	2004-03-13
700	1	909	19.35	2004-03-13
701	1	982	24.32	2004-03-13
702	1	985	8.82	2004-03-13
703	1	894	26.77	2004-03-13
704	1	979	23.19	2004-03-13
705	1	744	9.97	2004-03-13
706	1	779	26.02	2004-03-13
707	1	405	6.71	2004-03-13
708	1	495	9.12	2004-03-13
709	1	0	6.71	2004-03-13
710	1	717	29.27	2004-03-13
711	1	68	28.07	2004-03-13
712	1	389	28.21	2004-03-13
713	1	212	25.06	2004-03-13
714	1	987	25.79	2004-03-13
715	1	186	18.98	2004-03-13
716	1	157	13.85	2004-03-13
717	1	182	23.92	2004-03-13
718	1	899	10.10	2004-03-13
719	1	27	10.16	2004-03-13
720	1	738	28.12	2004-03-13
721	1	12	5.07	2004-03-13
722	1	82	15.00	2004-03-13
723	1	301	20.83	2004-03-13
724	1	362	20.63	2004-03-13
725	1	159	24.18	2004-03-13
726	1	143	28.51	2004-03-13
727	1	995	8.25	2004-03-13
728	1	922	5.48	2004-03-13
729	1	29	6.71	2004-03-13
730	1	191	24.66	2004-03-13
731	1	317	25.68	2004-03-13
732	1	391	27.09	2004-03-13
733	1	397	9.71	2004-03-13
734	1	231	18.31	2004-03-13
735	1	871	22.11	2004-03-13
736	1	60	26.06	2004-03-13
737	1	580	26.19	2004-03-13
738	1	836	24.22	2004-03-13
739	1	323	25.28	2004-03-13
740	1	42	5.08	2004-03-13
741	1	709	12.89	2004-03-13
742	1	394	22.79	2004-03-13
743	1	485	7.98	2004-03-13
744	1	197	13.54	2004-03-13
745	1	514	10.26	2004-03-13
746	1	626	11.12	2004-03-13
747	1	687	26.98	2004-03-13
748	1	883	22.76	2004-03-13
749	1	111	5.57	2004-03-13
750	1	399	6.57	2004-03-13
751	1	486	24.89	2004-03-13
752	1	384	8.59	2004-03-13
753	1	177	21.70	2004-03-13
754	1	165	25.04	2004-03-13
755	1	712	13.25	2004-03-13
756	1	977	29.52	2004-03-13
757	1	104	21.37	2004-03-13
758	1	88	18.06	2004-03-13
759	1	852	21.07	2004-03-13
760	1	424	6.12	2004-03-13
761	1	997	18.88	2004-03-13
762	1	887	28.04	2004-03-13
763	1	487	7.68	2004-03-13
764	1	193	11.52	2004-03-13
765	1	237	29.88	2004-03-13
766	1	491	29.86	2004-03-13
767	1	15	6.53	2004-03-13
768	1	390	8.08	2004-03-13
769	1	452	5.74	2004-03-13
770	1	524	15.81	2004-03-13
771	1	649	15.35	2004-03-13
772	1	537	13.29	2004-03-13
773	1	581	25.40	2004-03-13
774	1	268	5.68	2004-03-13
775	1	795	10.66	2004-03-13
776	1	677	23.63	2004-03-13
777	1	539	23.70	2004-03-13
778	1	71	8.86	2004-03-13
779	1	155	19.87	2004-03-13
780	1	903	7.31	2004-03-13
781	1	713	24.74	2004-03-13
782	1	72	12.06	2004-03-13
783	1	365	27.87	2004-03-13
784	1	768	8.87	2004-03-13
785	1	449	28.11	2004-03-13
786	1	118	20.84	2004-03-13
787	1	754	9.59	2004-03-13
788	1	460	10.73	2004-03-13
789	1	326	14.31	2004-03-13
790	1	203	12.43	2004-03-13
791	1	515	5.19	2004-03-13
792	1	262	6.49	2004-03-13
793	1	330	16.37	2004-03-13
794	1	206	10.52	2004-03-13
795	1	84	21.46	2004-03-13
796	1	202	24.09	2004-03-13
797	1	50	18.25	2004-03-13
798	1	645	29.48	2004-03-13
799	1	376	13.87	2004-03-13
800	1	696	18.16	2004-03-13
801	1	128	5.17	2004-03-13
802	1	323	12.24	2004-03-13
803	1	817	20.69	2004-03-13
804	1	193	25.90	2004-03-13
805	1	717	28.56	2004-03-13
806	1	187	8.34	2004-03-13
807	1	277	9.87	2004-03-13
808	1	530	25.86	2004-03-13
809	1	16	9.19	2004-03-13
810	1	891	27.71	2004-03-13
811	1	58	5.75	2004-03-13
812	1	231	13.05	2004-03-13
813	1	480	9.68	2004-03-13
814	1	173	13.52	2004-03-13
815	1	76	6.44	2004-03-13
816	1	639	11.57	2004-03-13
817	1	982	18.75	2004-03-13
818	1	857	6.57	2004-03-13
819	1	727	7.77	2004-03-13
820	1	917	29.66	2004-03-13
821	1	631	5.69	2004-03-13
822	1	384	11.03	2004-03-13
823	1	440	23.20	2004-03-13
824	1	127	28.56	2004-03-13
825	1	654	29.63	2004-03-13
826	1	103	20.90	2004-03-13
827	1	679	20.44	2004-03-13
828	1	431	16.51	2004-03-13
829	1	6	10.60	2004-03-13
830	1	780	8.58	2004-03-13
831	1	449	23.61	2004-03-13
832	1	142	8.88	2004-03-13
833	1	182	19.63	2004-03-13
834	1	386	22.56	2004-03-13
835	1	949	14.56	2004-03-13
836	1	588	16.19	2004-03-13
837	1	489	8.82	2004-03-13
838	1	932	19.30	2004-03-13
839	1	446	19.50	2004-03-13
840	1	48	14.63	2004-03-13
841	1	629	25.38	2004-03-13
842	1	248	28.53	2004-03-13
843	1	136	8.11	2004-03-13
844	1	774	18.82	2004-03-13
845	1	335	16.18	2004-03-13
846	1	895	6.65	2004-03-13
847	1	95	18.79	2004-03-13
848	1	917	17.37	2004-03-13
849	1	219	28.96	2004-03-13
850	1	727	29.32	2004-03-13
851	1	361	20.13	2004-03-13
852	1	750	26.09	2004-03-13
853	1	281	29.23	2004-03-13
854	1	394	24.75	2004-03-13
855	1	216	7.23	2004-03-13
856	1	401	5.29	2004-03-13
857	1	54	7.84	2004-03-13
858	1	853	5.33	2004-03-13
859	1	229	10.26	2004-03-13
860	1	189	29.24	2004-03-13
861	1	514	13.85	2004-03-13
862	1	653	6.71	2004-03-13
863	1	747	13.71	2004-03-13
864	1	980	23.48	2004-03-13
865	1	739	29.58	2004-03-13
866	1	972	14.59	2004-03-13
867	1	268	15.68	2004-03-13
868	1	117	11.70	2004-03-13
869	1	451	17.93	2004-03-13
870	1	718	22.89	2004-03-13
871	1	965	10.22	2004-03-13
872	1	823	26.29	2004-03-13
873	1	541	20.65	2004-03-13
874	1	254	19.40	2004-03-13
875	1	737	13.36	2004-03-13
876	1	326	12.73	2004-03-13
877	1	917	19.15	2004-03-13
878	1	605	22.43	2004-03-13
879	1	309	8.93	2004-03-13
880	1	247	26.62	2004-03-13
881	1	147	25.83	2004-03-13
882	1	369	21.69	2004-03-13
883	1	666	9.30	2004-03-13
884	1	318	16.89	2004-03-13
885	1	719	9.22	2004-03-13
886	1	87	17.53	2004-03-13
887	1	821	15.10	2004-03-13
888	1	977	5.78	2004-03-13
889	1	408	13.92	2004-03-13
890	1	343	19.78	2004-03-13
891	1	972	23.60	2004-03-13
892	1	810	7.25	2004-03-13
893	1	509	24.44	2004-03-13
894	1	818	7.88	2004-03-13
895	1	717	25.06	2004-03-13
896	1	946	27.56	2004-03-13
897	1	476	22.99	2004-03-13
898	1	577	22.02	2004-03-13
899	1	366	7.35	2004-03-13
900	1	294	16.94	2004-03-13
901	1	79	15.67	2004-03-13
902	1	687	10.77	2004-03-13
903	1	942	23.54	2004-03-13
904	1	384	18.63	2004-03-13
905	1	268	11.44	2004-03-13
906	1	332	10.33	2004-03-13
907	1	901	22.05	2004-03-13
908	1	893	11.74	2004-03-13
909	1	914	21.84	2004-03-13
910	1	273	28.29	2004-03-13
911	1	217	5.71	2004-03-13
912	1	894	22.94	2004-03-13
913	1	673	21.68	2004-03-13
914	1	221	5.23	2004-03-13
915	1	856	12.51	2004-03-13
916	1	298	20.73	2004-03-13
917	1	634	18.43	2004-03-13
918	1	686	15.70	2004-03-13
919	1	432	13.35	2004-03-13
920	1	279	20.28	2004-03-13
921	1	573	25.30	2004-03-13
922	1	286	23.00	2004-03-13
923	1	228	14.64	2004-03-13
924	1	909	20.67	2004-03-13
925	1	148	16.97	2004-03-13
926	1	663	24.60	2004-03-13
927	1	399	15.13	2004-03-13
928	1	833	7.56	2004-03-13
929	1	542	18.90	2004-03-13
930	1	894	25.51	2004-03-13
931	1	181	14.34	2004-03-13
932	1	484	21.07	2004-03-13
933	1	489	27.47	2004-03-13
934	1	139	25.09	2004-03-13
935	1	817	28.56	2004-03-13
936	1	793	20.34	2004-03-13
937	1	400	5.75	2004-03-13
938	1	538	26.85	2004-03-13
939	1	103	14.37	2004-03-13
940	1	7	12.18	2004-03-13
941	1	525	16.57	2004-03-13
942	1	874	28.23	2004-03-13
943	1	947	16.13	2004-03-13
944	1	419	19.94	2004-03-13
945	1	512	15.89	2004-03-13
946	1	80	9.51	2004-03-13
947	1	424	19.53	2004-03-13
948	1	178	8.38	2004-03-13
949	1	28	10.77	2004-03-13
950	1	407	11.06	2004-03-13
951	1	595	24.53	2004-03-13
952	1	165	21.48	2004-03-13
953	1	404	19.93	2004-03-13
954	1	539	5.51	2004-03-13
955	1	481	18.91	2004-03-13
956	1	379	20.11	2004-03-13
957	1	894	5.70	2004-03-13
958	1	427	10.83	2004-03-13
959	1	810	10.07	2004-03-13
960	1	587	21.31	2004-03-13
961	1	652	10.08	2004-03-13
962	1	186	15.72	2004-03-13
963	1	675	25.32	2004-03-13
964	1	608	13.33	2004-03-13
965	1	125	25.85	2004-03-13
966	1	41	10.45	2004-03-13
967	1	152	7.48	2004-03-13
968	1	857	18.69	2004-03-13
969	1	694	13.93	2004-03-13
970	1	768	28.57	2004-03-13
971	1	86	19.81	2004-03-13
972	1	172	24.21	2004-03-13
973	1	902	16.02	2004-03-13
974	1	223	6.37	2004-03-13
975	1	554	9.20	2004-03-13
976	1	409	10.41	2004-03-13
977	1	868	14.51	2004-03-13
978	1	355	10.05	2004-03-13
979	1	49	28.39	2004-03-13
980	1	980	22.32	2004-03-13
981	1	553	24.90	2004-03-13
982	1	366	28.67	2004-03-13
983	1	583	26.11	2004-03-13
984	1	609	6.17	2004-03-13
985	1	983	20.34	2004-03-13
986	1	189	6.56	2004-03-13
987	1	603	27.94	2004-03-13
988	1	122	11.17	2004-03-13
989	1	173	12.09	2004-03-13
990	1	565	12.78	2004-03-13
991	1	888	26.56	2004-03-13
992	1	792	10.51	2004-03-13
993	1	465	24.37	2004-03-13
994	1	583	15.44	2004-03-13
995	1	754	14.08	2004-03-13
996	1	103	19.12	2004-03-13
997	1	672	9.03	2004-03-13
998	1	613	12.30	2004-03-13
999	1	784	17.61	2004-03-13
1000	1	879	6.91	2004-03-13
1001	1	666	28.76	2004-03-13
1002	1	232	23.97	2004-03-13
1003	1	750	9.77	2004-03-13
1004	1	122	24.18	2004-03-13
1005	1	572	18.57	2004-03-13
1006	1	654	20.16	2004-03-13
1007	1	778	12.34	2004-03-13
1008	1	812	28.65	2004-03-13
1009	1	112	22.13	2004-03-13
1010	1	728	19.18	2004-03-13
1011	1	639	11.12	2004-03-13
1012	1	400	17.85	2004-03-13
1013	1	377	10.04	2004-03-13
1014	1	154	13.82	2004-03-13
1015	1	340	28.35	2004-03-13
1016	1	918	19.99	2004-03-13
1017	1	79	14.30	2004-03-13
1018	1	425	9.22	2004-03-13
1019	1	81	12.93	2004-03-13
1020	1	219	5.51	2004-03-13
1021	1	404	21.77	2004-03-13
1022	1	526	6.25	2004-03-13
1023	1	378	15.49	2004-03-13
1024	1	424	19.85	2004-03-13
1025	1	249	28.78	2004-03-13
1026	1	606	26.36	2004-03-13
1027	1	600	28.73	2004-03-13
1028	1	902	13.90	2004-03-13
1029	1	127	15.19	2004-03-13
1030	1	60	21.21	2004-03-13
1031	1	578	6.87	2004-03-13
1032	1	349	19.93	2004-03-13
1033	1	606	29.37	2004-03-13
1034	1	98	14.96	2004-03-13
1035	1	950	20.89	2004-03-13
1036	1	733	11.74	2004-03-13
1037	1	101	26.67	2004-03-13
1038	1	834	23.02	2004-03-13
1039	1	743	26.46	2004-03-13
1040	1	942	28.12	2004-03-13
1041	1	159	17.18	2004-03-13
1042	1	34	26.76	2004-03-13
1043	1	968	16.73	2004-03-13
1044	1	339	19.12	2004-03-13
1045	1	302	18.76	2004-03-13
1046	1	196	11.00	2004-03-13
1047	1	940	13.85	2004-03-13
1048	1	614	9.60	2004-03-13
\.


--
-- Name: item_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('item_id_seq', 1, false);


--
-- Data for Name: items; Type: TABLE DATA; Schema: public; Owner: -
--

COPY items (cust_id, order_id, item_id, wine_id, qty, price) FROM stdin;
1	1	1	888	2	11.56
1	2	1	206	3	73.80
1	2	2	517	12	81.62
1	2	3	521	3	49.23
1	2	4	332	3	75.87
1	2	5	225	3	50.88
1	2	6	697	2	44.18
1	3	1	706	1	26.02
1	3	2	390	3	25.29
1	4	1	883	12	102.30
1	4	2	460	2	38.70
1	4	3	842	12	313.83
1	4	4	965	1	25.85
1	4	5	559	1	6.67
2	1	1	636	12	280.61
2	1	2	57	3	52.80
2	1	3	178	3	33.63
2	2	1	652	12	113.08
2	2	2	105	12	246.51
2	2	3	731	1	25.68
2	2	4	919	12	146.85
2	3	1	801	12	56.87
2	3	2	224	2	47.84
2	3	3	696	2	13.24
2	3	4	49	12	111.98
2	3	5	882	1	21.69
2	4	1	1031	12	75.57
2	5	1	46	2	31.34
2	5	2	380	2	54.90
2	5	3	90	1	6.77
2	5	4	260	2	14.68
2	5	5	521	12	180.51
2	5	6	593	3	20.52
3	1	1	706	12	286.22
3	1	2	615	3	70.71
3	1	3	988	2	22.34
3	1	4	507	12	198.22
3	2	1	327	1	25.20
3	2	2	458	3	24.03
3	3	1	526	1	19.31
3	4	1	227	3	50.28
3	5	1	964	2	26.66
3	5	2	153	1	23.11
3	5	3	791	12	57.09
3	5	4	937	1	5.75
3	5	5	977	1	14.51
3	5	6	204	3	72.84
4	1	1	627	12	72.38
4	1	2	567	1	12.37
4	2	1	340	12	157.19
4	2	2	103	2	24.74
4	2	3	703	12	294.47
4	2	4	393	12	304.70
4	2	5	824	3	85.68
4	2	6	1014	2	27.64
5	1	1	862	2	13.42
5	2	1	47	1	24.04
5	2	2	948	12	92.18
5	2	3	157	2	48.12
5	2	4	663	1	7.23
5	3	1	35	1	17.64
5	4	1	376	1	21.17
5	5	1	18	2	16.90
6	1	1	921	1	25.30
6	1	2	557	12	149.38
6	1	3	188	2	15.42
6	2	1	492	1	22.10
6	2	2	346	12	78.54
6	2	3	138	2	43.02
6	2	4	467	2	50.94
6	2	5	481	12	64.02
7	1	1	427	1	20.01
7	1	2	25	2	29.82
7	1	3	348	2	36.72
7	2	1	686	3	58.56
7	2	2	565	3	72.06
7	3	1	314	12	274.12
7	3	2	478	2	35.66
7	3	3	524	2	30.52
7	3	4	854	12	272.25
7	4	1	890	3	59.34
7	4	2	875	2	26.72
7	4	3	862	2	13.42
7	4	4	1021	2	43.54
8	1	1	85	1	21.54
8	1	2	738	2	48.44
9	1	1	410	12	192.94
9	1	2	293	1	21.79
9	1	3	502	1	24.11
9	1	4	666	2	31.10
9	2	1	1014	12	152.02
9	2	2	897	1	22.99
9	2	3	906	12	113.63
9	2	4	962	2	31.44
9	2	5	447	1	13.58
9	3	1	421	3	42.90
9	3	2	179	12	59.18
9	3	3	826	12	229.90
9	3	4	189	2	33.46
9	3	5	434	12	279.84
9	3	6	73	3	32.10
9	4	1	636	12	280.61
9	4	2	796	1	24.09
9	4	3	896	1	27.56
10	1	1	927	3	45.39
10	1	2	502	3	72.33
10	1	3	940	12	133.98
10	1	4	108	3	48.63
10	1	5	983	12	287.21
10	2	1	126	1	24.09
10	2	2	485	1	22.86
10	2	3	30	3	24.48
10	2	4	57	3	52.80
10	2	5	512	2	54.98
10	3	1	812	12	143.55
10	3	2	737	12	288.09
10	3	3	711	3	84.21
10	3	4	720	2	56.24
10	3	5	460	1	19.35
10	3	6	494	12	311.85
10	4	1	358	2	34.34
10	4	2	409	12	245.74
10	4	3	807	1	9.87
10	4	4	34	1	14.28
10	4	5	431	1	17.86
10	5	1	618	2	58.14
10	5	2	23	1	26.56
10	5	3	654	2	53.60
10	5	4	1027	1	28.73
10	5	5	698	1	15.50
10	6	1	496	2	46.94
11	1	1	678	3	79.35
11	1	2	295	2	50.10
11	1	3	265	3	50.19
11	1	4	449	2	56.76
12	1	1	615	1	23.57
12	1	2	556	1	5.78
12	1	3	12	1	27.42
12	1	4	490	1	8.31
12	1	5	533	12	206.14
12	1	6	566	1	7.04
12	2	1	180	3	55.71
12	2	2	980	2	44.64
12	2	3	18	12	92.95
12	2	4	745	3	30.78
12	3	1	785	12	309.21
12	3	2	903	1	23.54
12	3	3	21	2	38.28
12	3	4	111	3	83.64
12	3	5	522	3	21.90
12	3	6	854	1	24.75
13	1	1	772	12	146.19
13	1	2	71	2	15.00
13	1	3	414	1	22.23
13	2	1	462	3	71.34
13	3	1	468	12	314.60
13	3	2	228	3	65.79
13	3	3	153	1	23.11
13	4	1	623	3	66.30
13	4	2	461	2	21.32
14	1	1	152	1	5.81
14	1	2	475	2	57.68
14	1	3	540	12	295.35
14	1	4	941	2	33.14
14	1	5	161	2	37.26
14	2	1	314	12	274.12
15	1	1	550	1	11.22
15	2	1	729	3	20.13
15	2	2	694	12	173.36
15	3	1	286	1	27.29
15	3	2	189	12	184.03
15	3	3	117	2	49.86
15	4	1	905	1	11.44
15	5	1	361	2	25.52
15	5	2	88	3	43.77
15	5	3	589	12	237.16
15	5	4	875	2	26.72
15	5	5	195	1	22.53
15	5	6	648	2	32.68
16	1	1	993	12	268.07
16	1	2	383	1	18.89
16	1	3	338	12	267.08
16	1	4	1021	3	65.31
17	1	1	471	2	57.28
17	1	2	2	3	84.45
17	1	3	518	3	46.05
17	1	4	1	3	63.54
17	2	1	927	2	30.26
17	2	2	331	12	86.24
17	2	3	641	1	19.47
17	2	4	984	1	6.17
17	2	5	795	3	64.38
17	2	6	743	3	23.94
17	3	1	425	1	15.85
17	4	1	168	3	73.11
17	4	2	450	1	14.42
17	4	3	935	3	85.68
18	1	1	381	12	229.46
18	1	2	745	3	30.78
18	1	3	11	2	32.96
19	1	1	538	1	14.50
19	1	2	280	3	55.74
19	1	3	789	3	42.93
19	1	4	751	3	74.67
19	1	5	461	3	31.98
19	2	1	461	1	10.66
19	2	2	973	1	16.02
20	1	1	460	3	58.05
20	1	2	102	3	20.13
20	1	3	1007	12	135.74
20	1	4	357	3	70.92
21	1	1	577	12	231.66
21	1	2	186	12	93.83
21	1	3	65	1	9.98
21	1	4	11	1	16.48
21	1	5	539	1	10.48
21	1	6	300	12	290.18
21	2	1	887	2	30.20
21	2	2	88	3	43.77
21	2	3	777	1	23.70
21	2	4	626	12	91.08
21	2	5	208	1	10.19
21	3	1	460	3	58.05
21	3	2	26	12	286.33
21	3	3	880	2	53.24
21	3	4	796	3	72.27
21	3	5	526	1	19.31
21	3	6	745	2	20.52
22	1	1	347	3	31.77
22	1	2	843	2	16.22
22	1	3	433	12	234.41
22	1	4	23	1	26.56
22	2	1	95	2	58.38
22	2	2	467	3	76.41
22	2	3	1006	3	60.48
22	2	4	374	2	18.56
22	2	5	242	12	311.52
22	3	1	357	12	260.04
22	3	2	46	12	172.37
22	3	3	755	1	13.25
22	3	4	1039	1	26.46
22	4	1	757	12	235.07
23	1	1	867	1	15.68
23	1	2	961	3	30.24
23	1	3	159	3	44.28
23	1	4	970	12	314.27
23	2	1	821	2	11.38
23	2	2	327	1	25.20
23	2	3	866	2	29.18
23	3	1	924	12	227.37
23	3	2	86	3	45.12
23	3	3	125	12	97.02
23	4	1	849	12	318.56
23	4	2	591	2	25.40
23	4	3	200	2	23.76
23	4	4	886	3	52.59
23	4	5	413	3	55.02
23	4	6	892	1	7.25
23	5	1	918	1	15.70
23	5	2	151	12	246.07
23	5	3	179	2	10.76
24	1	1	118	3	43.14
24	1	2	949	2	21.54
24	1	3	578	2	14.64
24	1	4	894	1	7.88
24	1	5	270	1	20.46
24	2	1	1016	12	219.89
24	2	2	769	12	63.14
24	2	3	62	1	16.85
24	2	4	237	12	280.28
25	1	1	308	12	143.66
25	1	2	188	3	23.13
25	1	3	833	2	39.26
25	1	4	1003	12	107.47
25	2	1	167	1	21.05
25	2	2	500	1	16.28
25	2	3	105	3	67.23
25	2	4	364	12	250.69
25	2	5	493	1	22.39
25	3	1	283	3	86.49
25	3	2	988	1	11.17
25	4	1	194	2	30.20
25	4	2	46	2	31.34
25	5	1	668	12	163.35
25	5	2	54	2	40.62
25	5	3	802	1	12.24
26	1	1	87	1	17.19
26	1	2	383	1	18.89
26	2	1	240	1	11.44
26	2	2	941	2	33.14
26	3	1	462	2	47.56
26	3	2	843	3	24.33
26	3	3	415	12	124.41
26	4	1	917	1	18.43
26	5	1	894	2	15.76
26	5	2	891	1	23.60
26	5	3	515	3	75.27
26	5	4	714	1	25.79
26	5	5	35	1	17.64
26	5	6	339	12	117.37
27	1	1	288	3	44.85
27	1	2	712	2	56.42
27	1	3	984	2	12.34
27	1	4	248	12	287.98
27	1	5	312	2	23.86
27	2	1	76	2	35.88
27	2	2	258	1	17.68
27	3	1	640	2	22.90
27	3	2	371	12	170.39
27	3	3	979	1	28.39
27	3	4	299	12	145.53
27	4	1	607	1	22.19
27	4	2	225	12	186.56
27	4	3	395	2	33.98
27	4	4	87	3	51.57
27	4	5	502	3	72.33
27	5	1	446	12	235.84
28	1	1	980	3	66.96
28	1	2	470	12	103.84
28	1	3	212	3	89.40
28	1	4	1018	1	9.22
28	1	5	1036	2	23.48
28	1	6	118	3	43.14
28	2	1	237	12	280.28
28	2	2	812	1	13.05
28	2	3	971	1	19.81
28	3	1	663	3	21.69
28	4	1	1024	12	218.35
28	5	1	732	1	27.09
28	5	2	230	12	123.64
28	5	3	992	2	21.02
28	6	1	834	12	248.16
29	1	1	458	12	88.11
29	1	2	228	3	65.79
29	1	3	100	3	70.38
30	1	1	958	12	119.13
30	2	1	109	3	48.00
30	2	2	780	1	7.31
30	2	3	1048	1	9.60
30	2	4	187	2	12.88
30	3	1	851	12	221.43
30	3	2	1038	3	69.06
30	4	1	794	2	21.04
30	4	2	809	3	27.57
30	4	3	1019	12	142.23
30	4	4	148	3	39.72
30	4	5	986	1	6.56
30	5	1	595	3	86.85
30	5	2	142	2	30.00
30	5	3	454	1	28.31
30	5	4	995	2	28.16
30	5	5	924	3	62.01
30	5	6	214	3	83.85
31	1	1	95	1	29.19
31	1	2	551	1	23.73
31	1	3	128	3	63.75
31	1	4	880	3	79.86
32	1	1	990	12	140.58
32	1	2	629	12	266.09
32	2	1	472	3	25.08
32	3	1	213	12	276.87
32	3	2	625	1	17.99
32	3	3	985	2	40.68
32	3	4	680	3	15.75
32	3	5	104	3	19.26
32	3	6	1031	1	6.87
32	4	1	134	12	246.95
32	4	2	567	3	37.11
32	4	3	622	1	17.45
32	5	1	105	2	44.82
32	5	2	542	1	19.52
32	5	3	651	1	29.92
32	6	1	661	12	121.88
32	6	2	772	12	146.19
32	6	3	354	12	68.09
33	1	1	235	1	21.85
33	1	2	633	3	48.54
33	1	3	152	12	63.91
33	1	4	122	1	22.13
33	1	5	592	2	30.64
33	1	6	1016	2	39.98
33	2	1	862	3	20.13
33	2	2	298	3	73.92
33	2	3	403	1	24.98
33	2	4	132	12	202.73
33	2	5	160	1	29.92
33	2	6	583	1	18.49
33	3	1	974	1	6.37
33	4	1	756	1	29.52
33	4	2	390	3	25.29
33	4	3	717	3	71.76
33	5	1	450	12	158.62
33	5	2	979	1	28.39
33	5	3	624	12	244.75
33	5	4	812	1	13.05
34	1	1	141	2	44.94
34	1	2	1003	2	19.54
35	1	1	931	1	14.34
35	2	1	128	12	233.75
35	2	2	558	12	314.49
35	2	3	74	12	138.16
35	3	1	157	3	72.18
35	3	2	124	1	27.42
35	3	3	1002	12	263.67
35	3	4	115	1	14.02
35	4	1	694	12	173.36
36	1	1	442	12	150.37
36	2	1	580	3	73.47
36	2	2	294	3	23.73
36	2	3	656	1	25.43
36	3	1	743	2	15.96
36	3	2	442	2	27.34
36	3	3	517	1	7.42
36	3	4	1023	3	46.47
36	4	1	172	1	29.89
37	1	1	1006	3	60.48
37	2	1	921	2	50.60
37	3	1	1004	12	265.98
37	3	2	15	1	12.33
37	3	3	955	3	56.73
37	3	4	878	3	67.29
37	3	5	342	1	11.22
37	3	6	757	2	42.74
37	4	1	123	3	23.10
37	4	2	1044	12	210.32
37	4	3	97	1	12.11
37	4	4	890	1	19.78
37	4	5	282	1	27.83
37	4	6	654	1	26.80
38	1	1	75	1	22.73
38	1	2	683	2	18.38
38	1	3	510	2	22.06
38	1	4	501	12	246.29
38	1	5	920	1	20.28
38	1	6	711	12	308.77
39	1	1	984	3	18.51
39	2	1	198	2	27.62
39	2	2	288	2	29.90
39	3	1	298	2	49.28
39	3	2	347	1	10.59
39	3	3	15	2	24.66
39	4	1	80	1	28.44
39	4	2	625	3	53.97
39	4	3	277	1	26.37
39	5	1	816	1	11.57
39	5	2	881	2	51.66
39	5	3	566	1	7.04
39	5	4	818	12	72.27
39	5	5	174	12	282.48
40	1	1	513	3	51.87
40	1	2	6	1	20.25
40	1	3	834	1	22.56
40	2	1	943	3	48.39
40	2	2	319	2	29.40
40	2	3	263	2	26.24
41	1	1	613	12	237.82
41	1	2	102	12	73.81
41	1	3	418	3	30.99
41	1	4	1047	2	27.70
42	1	1	447	3	40.74
42	1	2	136	3	76.05
42	1	3	36	2	59.02
42	2	1	591	2	25.40
42	2	2	1037	2	53.34
42	2	3	591	12	139.70
42	3	1	304	1	22.46
42	3	2	463	1	27.14
42	3	3	475	12	317.24
43	1	1	534	1	16.15
43	1	2	286	12	300.19
43	1	3	624	3	66.75
43	2	1	244	1	12.21
43	2	2	1037	3	80.01
43	2	3	420	1	28.55
43	2	4	162	2	39.28
43	2	5	500	1	16.28
43	2	6	274	1	6.22
43	3	1	71	12	82.50
43	3	2	837	3	26.46
43	3	3	517	3	22.26
43	3	4	764	12	126.72
43	3	5	612	3	88.74
43	3	6	304	2	44.92
43	4	1	298	12	271.04
43	4	2	826	3	62.70
43	4	3	1045	2	37.52
43	4	4	646	1	7.95
43	4	5	823	3	69.60
43	5	1	178	1	11.21
43	5	2	568	12	124.52
43	5	3	815	12	70.84
43	5	4	211	3	52.74
43	5	5	244	3	36.63
44	1	1	33	2	37.50
44	1	2	500	1	16.28
44	1	3	37	1	14.15
44	1	4	574	3	56.73
44	1	5	889	12	153.12
44	1	6	184	1	27.11
44	2	1	313	12	222.42
44	2	2	160	12	329.12
44	2	3	85	1	21.54
44	2	4	34	1	14.28
44	2	5	525	1	22.09
44	2	6	1024	2	39.70
45	1	1	853	2	58.46
45	1	2	150	1	8.66
45	1	3	42	2	42.56
45	1	4	399	1	14.63
45	1	5	613	2	43.24
45	1	6	51	12	264.99
46	1	1	961	2	20.16
46	1	2	259	1	10.52
46	1	3	740	1	5.08
46	1	4	168	12	268.07
46	1	5	53	12	292.82
46	1	6	981	12	273.90
46	2	1	158	12	147.29
46	2	2	588	1	11.46
46	2	3	72	12	274.56
46	2	4	892	2	14.50
47	1	1	129	2	11.50
47	1	2	831	1	23.61
47	1	3	431	3	53.58
47	1	4	27	2	41.74
48	1	1	261	2	30.74
48	1	2	893	2	48.88
48	1	3	218	3	27.18
48	1	4	789	2	28.62
48	1	5	423	3	80.64
48	2	1	467	2	50.94
48	3	1	477	1	23.89
48	3	2	367	2	39.74
48	3	3	983	3	78.33
48	3	4	827	12	224.84
48	3	5	959	12	110.77
48	3	6	516	3	32.61
48	4	1	595	3	86.85
48	4	2	425	1	15.85
48	4	3	14	12	57.97
48	5	1	970	12	314.27
48	5	2	1025	12	316.58
48	5	3	384	12	231.77
48	5	4	684	2	45.10
48	6	1	1000	12	76.01
48	6	2	242	3	84.96
48	6	3	485	3	68.58
48	6	4	560	2	45.82
48	6	5	492	3	66.30
48	6	6	874	1	19.40
49	1	1	582	1	20.88
49	1	2	84	12	295.24
49	1	3	1006	1	20.16
49	2	1	152	3	17.43
49	2	2	335	12	105.38
49	2	3	283	12	317.13
49	2	4	262	12	244.09
49	2	5	659	2	48.76
49	2	6	757	2	42.74
49	3	1	841	2	50.76
49	3	2	265	12	184.03
49	3	3	592	1	15.32
49	4	1	815	2	12.88
49	4	2	704	1	23.19
49	4	3	96	12	248.82
49	4	4	999	3	52.83
49	4	5	183	3	75.81
49	4	6	1036	12	129.14
49	5	1	445	2	42.86
49	5	2	19	12	321.42
50	1	1	662	12	256.19
50	1	2	168	3	73.11
50	1	3	950	1	11.06
50	2	1	457	12	231.00
50	3	1	975	3	27.60
50	3	2	768	3	24.24
50	3	3	299	2	26.46
50	3	4	627	12	72.38
50	3	5	856	1	5.29
50	3	6	647	12	322.52
51	1	1	434	12	279.84
51	1	2	209	1	14.80
51	1	3	637	3	43.68
51	1	4	183	2	50.54
51	1	5	692	3	53.70
51	2	1	975	2	18.40
51	2	2	243	1	19.35
51	2	3	997	1	9.03
51	3	1	408	12	175.45
51	3	2	470	3	28.32
51	3	3	228	1	21.93
51	4	1	146	12	234.30
51	4	2	930	3	76.53
51	4	3	402	12	209.88
51	4	4	5	3	20.01
51	5	1	58	3	79.80
51	5	2	101	2	17.46
51	5	3	677	3	42.24
51	5	4	33	12	206.25
51	6	1	896	2	55.12
52	1	1	958	12	119.13
52	1	2	602	2	23.44
52	1	3	922	1	23.00
52	1	4	419	3	33.12
52	2	1	219	2	12.90
52	2	2	612	3	88.74
52	2	3	466	3	35.43
52	3	1	674	12	163.57
52	3	2	203	3	44.61
52	3	3	728	12	60.28
52	3	4	944	3	59.82
52	4	1	16	2	17.90
52	5	1	101	12	96.03
52	5	2	381	1	20.86
52	5	3	518	2	30.70
52	5	4	902	1	10.77
52	5	5	197	1	11.73
52	6	1	592	1	15.32
52	6	2	37	12	155.65
52	6	3	293	1	21.79
52	6	4	206	12	270.60
52	6	5	286	12	300.19
52	6	6	113	1	19.25
53	1	1	169	2	19.50
53	1	2	779	1	19.87
53	1	3	53	3	79.86
53	1	4	39	3	22.14
53	2	1	254	3	67.65
53	2	2	730	1	24.66
53	2	3	935	3	85.68
53	2	4	263	12	144.32
53	3	1	1023	2	30.98
53	3	2	786	3	62.52
53	3	3	705	12	109.67
53	4	1	544	3	19.59
53	5	1	163	12	224.18
53	5	2	247	2	50.80
53	5	3	999	2	35.22
53	5	4	766	1	29.86
53	5	5	838	3	57.90
53	6	1	827	1	20.44
54	1	1	77	12	97.24
54	1	2	567	12	136.07
54	1	3	1031	2	13.74
54	2	1	764	2	23.04
54	2	2	892	1	7.25
54	3	1	790	2	24.86
54	3	2	720	1	28.12
54	3	3	180	3	55.71
54	3	4	576	1	26.76
54	4	1	36	3	88.53
54	4	2	775	3	31.98
54	4	3	13	2	35.20
54	4	4	147	12	215.38
54	4	5	651	12	329.12
55	1	1	12	12	301.62
56	1	1	98	3	73.47
56	1	2	213	1	25.17
56	1	3	465	12	88.00
56	2	1	397	3	34.14
56	2	2	764	2	23.04
56	2	3	228	1	21.93
56	2	4	239	1	24.19
56	2	5	68	1	21.66
56	2	6	420	3	85.65
57	1	1	461	1	10.66
57	2	1	858	1	5.33
57	2	2	714	2	51.58
57	3	1	452	1	7.78
57	3	2	588	12	126.06
57	3	3	55	2	42.82
57	3	4	1013	2	20.08
57	3	5	574	1	18.91
57	3	6	515	12	275.99
57	4	1	325	12	145.31
57	4	2	578	12	80.52
57	4	3	380	2	54.90
57	4	4	102	12	73.81
58	1	1	81	12	280.72
58	1	2	812	2	26.10
58	1	3	428	12	68.64
58	1	4	1019	2	25.86
58	1	5	185	12	175.12
59	1	1	314	2	49.84
59	1	2	1027	2	57.46
59	1	3	234	1	26.40
59	1	4	869	1	17.93
59	1	5	88	3	43.77
59	2	1	946	1	9.51
59	2	2	602	2	23.44
59	2	3	319	3	44.10
59	2	4	62	12	185.35
59	3	1	132	3	55.29
59	3	2	602	2	23.44
59	4	1	621	3	88.38
59	4	2	1032	12	219.23
59	4	3	143	12	137.39
59	5	1	887	1	15.10
59	5	2	234	3	79.20
60	1	1	920	12	223.08
60	1	2	238	12	265.65
60	1	3	359	3	55.83
61	1	1	807	3	29.61
61	1	2	1	1	21.18
61	1	3	957	12	62.70
61	1	4	241	1	16.95
61	1	5	598	2	25.86
61	2	1	863	1	13.71
61	3	1	821	2	11.38
61	3	2	268	1	9.15
61	3	3	1020	1	5.51
61	3	4	962	12	172.92
61	3	5	38	12	63.25
61	4	1	119	3	53.46
61	4	2	847	1	18.79
61	4	3	735	3	66.33
61	4	4	238	12	265.65
61	4	5	445	3	64.29
61	4	6	966	12	114.95
62	1	1	717	1	23.92
62	1	2	949	3	32.31
62	1	3	888	1	5.78
62	1	4	505	1	27.63
62	1	5	789	3	42.93
62	1	6	103	12	136.07
62	2	1	1007	12	135.74
62	2	2	792	3	19.47
62	2	3	753	2	43.40
62	2	4	318	12	155.32
62	2	5	200	1	11.88
62	2	6	893	1	24.44
62	3	1	1008	12	315.15
62	3	2	264	1	12.84
62	3	3	789	2	28.62
62	3	4	1012	12	196.35
62	3	5	329	3	44.94
62	4	1	260	3	22.02
62	4	2	552	2	10.60
62	4	3	600	2	33.98
62	4	4	522	2	14.60
62	4	5	269	2	30.72
62	5	1	3	1	9.94
62	5	2	24	12	204.60
62	5	3	63	2	47.44
62	5	4	492	1	22.10
63	1	1	466	1	11.81
63	1	2	217	12	75.13
63	1	3	595	3	86.85
63	1	4	82	1	22.30
63	2	1	5	2	13.34
63	3	1	388	12	268.29
63	3	2	295	3	75.15
63	3	3	728	12	60.28
63	3	4	142	1	15.00
63	3	5	1032	2	39.86
63	3	6	1007	3	37.02
64	1	1	332	3	75.87
64	2	1	687	12	185.79
64	2	2	930	2	51.02
64	2	3	188	2	15.42
64	2	4	132	1	18.43
64	3	1	276	1	23.22
64	3	2	768	2	16.16
64	3	3	436	1	5.99
64	3	4	483	3	36.18
64	3	5	737	3	78.57
64	4	1	200	1	11.88
64	5	1	565	12	264.22
64	5	2	173	1	26.52
64	5	3	584	2	22.10
64	5	4	393	2	55.40
64	5	5	825	12	325.93
64	5	6	503	1	24.79
64	6	1	803	3	62.07
64	6	2	523	12	294.58
64	6	3	689	1	10.03
65	1	1	869	2	35.86
65	1	2	446	2	42.88
65	1	3	908	3	35.22
65	1	4	45	3	23.13
65	1	5	203	2	29.74
65	1	6	186	2	17.06
65	2	1	1020	3	16.53
66	1	1	9	12	269.17
66	1	2	252	3	76.95
66	1	3	266	2	37.32
66	2	1	560	12	252.01
66	2	2	307	2	10.56
66	3	1	799	2	27.74
66	3	2	406	3	20.16
66	3	3	152	2	11.62
66	4	1	20	2	19.40
66	4	2	202	1	10.08
66	4	3	584	1	11.05
66	4	4	251	1	5.71
66	5	1	42	1	21.28
67	1	1	831	2	47.22
67	1	2	407	2	37.96
67	2	1	267	3	25.89
67	2	2	986	1	6.56
67	2	3	289	2	38.72
67	2	4	669	3	40.86
67	2	5	924	3	62.01
67	2	6	644	12	313.06
67	3	1	999	12	193.71
67	3	2	1025	2	57.56
67	3	3	300	12	290.18
67	3	4	401	3	50.25
67	4	1	79	2	23.94
67	4	2	388	3	73.17
67	4	3	888	2	11.56
67	4	4	455	2	57.98
67	4	5	174	3	77.04
67	4	6	918	3	47.10
68	1	1	787	2	19.18
68	1	2	290	3	26.73
68	1	3	481	12	64.02
68	1	4	790	2	24.86
68	1	5	801	12	56.87
68	1	6	453	12	162.47
68	2	1	228	3	65.79
68	2	2	216	12	253.55
68	3	1	265	3	50.19
68	3	2	818	3	19.71
68	3	3	138	1	21.51
68	4	1	691	12	319.00
68	5	1	68	1	21.66
68	5	2	929	12	207.90
68	6	1	234	1	26.40
68	6	2	70	2	32.10
68	6	3	454	12	311.41
68	6	4	185	3	47.76
68	6	5	616	2	24.78
68	6	6	676	1	12.35
69	1	1	732	2	54.18
69	2	1	292	1	7.84
69	2	2	575	2	51.34
70	1	1	1033	1	29.37
70	1	2	812	1	13.05
70	1	3	814	1	13.52
70	1	4	1039	2	52.92
70	2	1	886	2	35.06
70	2	2	145	1	26.47
70	2	3	787	2	19.18
70	2	4	754	1	25.04
70	2	5	530	3	42.96
70	2	6	797	3	54.75
71	1	1	217	1	6.83
71	1	2	296	3	29.85
71	2	1	128	12	233.75
71	2	2	868	2	23.40
71	2	3	656	1	25.43
71	2	4	11	3	49.44
71	2	5	651	3	89.76
71	3	1	162	2	39.28
71	3	2	945	2	31.78
71	3	3	534	1	16.15
71	3	4	1026	2	52.72
71	4	1	1026	3	79.08
71	4	2	198	3	41.43
71	4	3	702	1	8.82
71	4	4	281	2	11.26
71	4	5	984	3	18.51
71	4	6	614	3	26.79
71	5	1	864	2	46.96
71	5	2	957	12	62.70
71	5	3	751	2	49.78
71	5	4	530	12	157.52
71	5	5	196	3	61.95
71	6	1	494	2	56.70
71	6	2	1040	2	56.24
71	6	3	161	3	55.89
71	6	4	2	2	56.30
71	6	5	929	3	56.70
72	1	1	814	1	13.52
72	1	2	503	3	74.37
72	1	3	458	1	8.01
72	1	4	7	3	36.60
72	1	5	711	1	28.07
72	1	6	831	12	259.71
72	2	1	706	12	286.22
72	2	2	204	2	48.56
73	1	1	827	1	20.44
73	1	2	180	2	37.14
73	1	3	524	2	30.52
73	1	4	339	1	10.67
73	1	5	984	2	12.34
74	1	1	143	2	24.98
74	1	2	498	2	32.96
74	1	3	798	3	88.44
74	1	4	916	3	62.19
74	2	1	473	12	80.08
74	2	2	921	3	75.90
74	2	3	162	12	216.04
74	3	1	463	3	81.42
74	3	2	230	3	33.72
74	3	3	993	3	73.11
74	3	4	952	3	64.44
74	4	1	683	3	27.57
74	5	1	487	1	29.62
74	5	2	943	12	177.43
75	1	1	454	3	84.93
75	2	1	429	3	49.56
75	2	2	104	12	70.62
75	2	3	637	3	43.68
75	3	1	847	1	18.79
75	3	2	932	2	42.14
75	4	1	130	1	15.76
75	4	2	394	2	10.76
75	4	3	550	1	11.22
75	4	4	889	12	153.12
75	4	5	770	12	173.91
75	4	6	794	2	21.04
75	5	1	407	2	37.96
75	5	2	386	2	56.90
76	1	1	793	3	49.11
76	1	2	233	1	5.65
76	1	3	28	2	39.98
76	1	4	17	12	277.53
76	1	5	196	12	227.15
76	1	6	752	3	25.77
76	2	1	169	2	19.50
76	2	2	882	2	43.38
76	2	3	193	3	72.54
76	3	1	930	3	76.53
76	3	2	387	12	169.73
76	4	1	892	1	7.25
76	4	2	92	1	29.26
76	4	3	998	1	12.30
76	4	4	640	1	11.45
76	4	5	448	2	11.56
76	5	1	33	1	18.75
76	5	2	8	12	110.66
76	5	3	685	2	22.44
76	5	4	870	3	68.67
76	5	5	797	3	54.75
77	1	1	442	12	150.37
78	1	1	623	12	243.10
78	1	2	263	3	39.36
78	1	3	898	2	44.04
78	2	1	256	3	84.03
78	3	1	874	2	38.80
78	3	2	825	1	29.63
78	4	1	592	2	30.64
78	4	2	339	12	117.37
78	4	3	924	12	227.37
78	5	1	724	2	41.26
78	5	2	854	3	74.25
78	5	3	295	2	50.10
79	1	1	720	3	84.36
79	1	2	126	1	24.09
79	1	3	325	2	26.42
79	2	1	463	12	298.54
79	2	2	738	12	266.42
79	2	3	640	1	11.45
79	3	1	493	3	67.17
79	3	2	695	12	264.33
79	3	3	158	2	26.78
79	3	4	679	2	23.62
79	4	1	713	3	75.18
79	4	2	705	1	9.97
79	5	1	821	3	17.07
79	5	2	242	1	28.32
80	1	1	427	3	60.03
80	1	2	218	1	9.06
81	1	1	468	12	314.60
81	2	1	201	2	34.76
81	3	1	782	12	132.66
81	4	1	242	1	28.32
81	4	2	827	2	40.88
82	1	1	984	2	12.34
82	1	2	730	1	24.66
82	2	1	596	12	203.17
82	2	2	489	3	64.05
82	2	3	163	3	61.14
82	2	4	971	2	39.62
82	3	1	937	1	5.75
82	3	2	540	1	26.85
82	3	3	315	2	51.86
82	3	4	487	12	325.82
82	4	1	20	2	19.40
82	4	2	600	2	33.98
82	4	3	973	3	48.06
82	4	4	213	12	276.87
82	4	5	106	1	26.10
83	1	1	598	3	38.79
83	1	2	496	1	23.47
83	2	1	285	12	93.94
83	2	2	307	3	15.84
83	3	1	138	2	43.02
83	3	2	619	3	62.37
83	4	1	412	1	6.56
83	4	2	978	12	110.55
83	4	3	828	3	49.53
83	4	4	439	12	210.43
83	4	5	1022	3	18.75
83	4	6	950	2	22.12
83	5	1	1006	1	20.16
83	6	1	229	3	20.97
83	6	2	433	3	63.93
83	6	3	759	2	42.14
83	6	4	496	3	70.41
84	1	1	844	2	37.64
84	1	2	577	2	42.12
84	1	3	374	3	27.84
84	1	4	415	3	33.93
84	1	5	983	12	287.21
85	1	1	75	12	250.03
85	2	1	16	1	8.95
85	2	2	775	12	117.26
85	2	3	6	1	20.25
86	1	1	172	1	29.89
86	1	2	25	3	44.73
86	1	3	712	2	56.42
86	1	4	831	3	70.83
86	1	5	582	3	62.64
86	2	1	575	12	282.37
86	2	2	752	2	17.18
86	2	3	948	12	92.18
86	2	4	815	2	12.88
86	2	5	226	12	95.48
86	2	6	192	1	22.37
86	3	1	494	12	311.85
86	3	2	177	1	27.41
86	3	3	437	2	17.22
87	1	1	514	3	48.54
87	1	2	853	2	58.46
87	1	3	389	1	17.52
87	1	4	517	1	7.42
87	1	5	359	3	55.83
87	1	6	913	2	43.36
88	1	1	827	1	20.44
88	1	2	557	12	149.38
88	2	1	617	2	24.18
88	2	2	306	12	291.17
88	2	3	776	2	47.26
88	3	1	425	2	31.70
88	3	2	1015	3	85.05
88	3	3	161	12	204.93
88	3	4	802	12	134.64
88	3	5	45	2	15.42
88	3	6	895	3	75.18
88	4	1	597	12	304.37
88	4	2	182	2	14.86
88	5	1	531	2	49.56
88	5	2	701	2	48.64
88	6	1	646	12	87.45
88	6	2	573	1	20.36
88	6	3	88	2	29.18
88	6	4	251	3	17.13
88	6	5	600	12	186.89
88	6	6	701	3	72.96
89	1	1	338	3	72.84
89	1	2	193	12	265.98
89	1	3	529	1	29.10
89	1	4	73	1	10.70
90	1	1	913	3	65.04
90	1	2	118	2	28.76
90	1	3	494	12	311.85
90	2	1	187	3	19.32
90	2	2	632	12	181.28
90	3	1	893	3	73.32
90	3	2	976	1	10.41
90	3	3	127	2	35.72
90	3	4	558	12	314.49
90	4	1	277	1	26.37
90	5	1	742	2	45.58
90	5	2	308	2	26.12
90	5	3	881	3	77.49
90	5	4	38	3	17.25
90	5	5	277	12	290.07
90	5	6	883	2	18.60
91	1	1	454	12	311.41
91	1	2	741	3	38.67
91	1	3	920	1	20.28
91	2	1	313	1	20.22
91	2	2	78	1	15.50
92	1	1	76	12	197.34
92	1	2	1048	12	105.60
92	1	3	696	2	13.24
92	1	4	331	3	23.52
92	1	5	156	12	82.39
92	1	6	463	1	27.14
92	2	1	814	1	13.52
92	2	2	940	12	133.98
92	2	3	254	3	67.65
92	2	4	142	2	30.00
92	2	5	285	12	93.94
93	1	1	107	12	70.51
93	1	2	885	12	101.42
93	1	3	446	1	21.44
93	1	4	698	12	170.50
93	1	5	222	2	48.40
93	2	1	360	3	35.82
93	2	2	117	12	274.23
93	2	3	953	12	219.23
93	2	4	715	1	18.98
93	2	5	887	2	30.20
93	2	6	437	3	25.83
93	3	1	1043	2	33.46
93	3	2	174	1	25.68
93	4	1	701	3	72.96
93	4	2	293	12	239.69
93	4	3	505	1	27.63
93	4	4	756	1	29.52
93	5	1	976	2	20.82
93	5	2	876	3	38.19
93	5	3	597	12	304.37
93	5	4	985	3	61.02
93	5	5	39	3	22.14
93	5	6	289	2	38.72
93	6	1	435	12	130.35
94	1	1	119	2	35.64
94	1	2	870	2	45.78
94	1	3	148	2	26.48
94	1	4	1032	12	219.23
94	1	5	390	3	25.29
94	1	6	9	12	269.17
94	2	1	437	1	8.61
94	2	2	12	2	54.84
94	2	3	389	1	17.52
94	2	4	376	1	21.17
95	1	1	944	1	19.94
95	1	2	551	12	261.03
95	1	3	398	1	29.14
95	1	4	922	2	46.00
95	2	1	656	2	50.86
95	2	2	844	12	207.02
95	2	3	794	12	115.72
95	2	4	305	12	57.97
95	2	5	836	2	32.38
95	3	1	133	2	35.08
95	3	2	812	3	39.15
95	3	3	774	3	17.04
95	3	4	827	12	224.84
95	3	5	896	1	27.56
95	3	6	181	1	5.47
95	4	1	757	12	235.07
95	4	2	448	3	17.34
95	4	3	225	3	50.88
95	4	4	65	3	29.94
95	5	1	854	12	272.25
95	5	2	4	3	63.48
95	5	3	859	12	112.86
95	6	1	765	1	29.88
96	1	1	1046	3	33.00
96	2	1	586	2	35.40
96	2	2	983	2	52.22
96	2	3	362	3	68.76
96	3	1	111	12	306.68
96	4	1	628	3	28.71
96	4	2	761	1	18.88
96	4	3	765	1	29.88
96	4	4	967	1	7.48
96	5	1	241	2	33.90
96	5	2	219	3	19.35
97	1	1	635	1	21.39
97	1	2	465	1	8.00
97	1	3	775	1	10.66
97	2	1	367	3	59.61
97	2	2	198	1	13.81
97	2	3	766	2	59.72
97	2	4	460	1	19.35
97	2	5	655	12	234.52
98	1	1	359	1	18.61
98	1	2	87	2	34.38
98	1	3	993	1	24.37
98	1	4	319	12	161.70
98	2	1	265	2	33.46
98	2	2	298	3	73.92
98	2	3	797	2	36.50
98	2	4	308	12	143.66
99	1	1	468	2	57.20
99	2	1	635	12	235.29
99	2	2	93	3	65.01
99	3	1	266	12	205.26
99	4	1	58	12	292.60
99	4	2	873	2	41.30
99	4	3	1018	3	27.66
100	1	1	236	12	242.11
100	1	2	264	1	12.84
100	1	3	386	1	28.45
100	1	4	774	3	17.04
100	2	1	418	1	10.33
100	3	1	453	3	44.31
100	3	2	284	12	97.79
100	3	3	197	1	11.73
100	4	1	312	1	11.93
100	4	2	443	12	322.52
100	4	3	490	3	24.93
100	4	4	720	1	28.12
100	4	5	854	1	24.75
100	4	6	315	1	25.93
100	5	1	582	2	41.76
100	5	2	795	3	64.38
100	5	3	432	2	50.90
100	5	4	163	3	61.14
100	5	5	142	3	45.00
101	1	1	649	3	51.27
101	1	2	32	3	38.64
102	1	1	134	3	67.35
102	1	2	956	12	221.21
102	1	3	512	2	54.98
102	1	4	217	3	20.49
103	1	1	316	3	86.34
103	2	1	602	3	35.16
103	2	2	1037	12	293.37
103	3	1	931	1	14.34
103	3	2	621	12	324.06
103	3	3	481	12	64.02
103	3	4	695	12	264.33
103	3	5	334	3	42.75
103	3	6	986	3	19.68
103	4	1	773	3	76.20
103	4	2	169	3	29.25
103	4	3	666	12	171.05
103	4	4	396	3	35.85
103	5	1	611	1	21.32
103	5	2	865	1	29.58
103	5	3	72	1	24.96
103	5	4	925	1	16.97
103	5	5	800	1	18.16
103	5	6	189	2	33.46
103	6	1	78	12	170.50
103	6	2	82	2	44.60
104	1	1	842	12	313.83
104	1	2	419	1	11.04
104	1	3	1013	3	30.12
104	1	4	976	12	114.51
104	2	1	427	1	20.01
104	3	1	106	1	26.10
104	3	2	732	12	297.99
104	3	3	264	2	25.68
104	3	4	390	1	8.43
104	4	1	649	12	187.99
104	4	2	469	1	26.88
105	1	1	752	1	8.59
105	1	2	159	2	29.52
105	1	3	170	1	9.75
105	1	4	47	12	264.44
105	2	1	173	2	53.04
105	2	2	657	2	34.86
105	2	3	38	3	17.25
105	2	4	947	2	39.06
105	3	1	998	12	135.30
106	1	1	801	3	15.51
106	1	2	217	3	20.49
106	1	3	770	12	173.91
106	1	4	814	1	13.52
106	1	5	445	1	21.43
106	2	1	799	3	41.61
107	1	1	31	3	50.22
107	1	2	728	3	16.44
107	1	3	326	2	26.32
107	2	1	976	2	20.82
107	2	2	232	1	14.33
108	1	1	681	1	28.12
108	1	2	982	2	57.34
108	1	3	405	2	12.92
108	1	4	444	12	199.76
108	1	5	28	1	19.99
108	1	6	255	2	54.96
108	2	1	833	1	19.63
108	3	1	194	12	166.10
108	3	2	419	12	121.44
108	3	3	938	2	53.70
108	3	4	240	2	22.88
108	4	1	994	3	46.32
108	5	1	163	1	20.38
108	6	1	618	2	58.14
108	6	2	400	12	107.36
108	6	3	594	3	18.24
109	1	1	1000	12	76.01
109	1	2	853	2	58.46
109	1	3	907	3	66.15
109	1	4	755	12	145.75
109	1	5	890	12	217.58
109	1	6	968	3	56.07
109	2	1	515	1	25.09
109	3	1	378	2	59.36
109	3	2	879	2	17.86
109	3	3	243	3	58.05
109	3	4	791	1	5.19
109	3	5	363	2	29.44
109	3	6	61	1	16.98
109	4	1	748	3	68.28
109	4	2	239	1	24.19
110	1	1	783	2	55.74
110	1	2	333	1	16.64
110	1	3	580	12	269.39
110	2	1	721	1	5.07
110	2	2	150	2	17.32
110	2	3	680	2	10.50
110	2	4	4	2	42.32
110	3	1	998	12	135.30
110	4	1	602	2	23.44
110	4	2	106	12	287.10
110	4	3	213	3	75.51
110	4	4	512	12	302.39
110	4	5	874	3	58.20
110	5	1	185	12	175.12
110	5	2	1004	2	48.36
110	5	3	1013	12	110.44
110	6	1	74	12	138.16
110	6	2	918	1	15.70
110	6	3	843	1	8.11
111	1	1	350	12	153.89
111	1	2	473	3	21.84
111	1	3	131	2	37.48
111	1	4	716	2	27.70
111	1	5	379	3	25.17
111	1	6	665	12	73.92
111	2	1	445	2	42.86
111	2	2	329	3	44.94
111	2	3	140	3	17.85
111	2	4	494	3	85.05
111	3	1	712	12	310.31
111	3	2	289	2	38.72
111	3	3	325	3	39.63
111	3	4	1000	12	76.01
111	3	5	486	1	15.87
111	3	6	117	3	74.79
111	4	1	614	1	8.93
111	4	2	159	1	14.76
111	4	3	1036	3	35.22
111	4	4	710	12	321.97
111	5	1	703	12	294.47
111	5	2	946	3	28.53
112	1	1	768	3	24.24
112	1	2	841	2	50.76
112	1	3	242	1	28.32
112	1	4	450	2	28.84
112	1	5	32	1	12.88
112	2	1	21	2	38.28
112	2	2	603	12	63.58
112	2	3	393	1	27.70
112	2	4	689	3	30.09
112	2	5	1025	3	86.34
112	2	6	712	2	56.42
112	3	1	228	12	241.23
112	3	2	297	1	25.27
112	3	3	29	1	20.97
112	3	4	113	1	19.25
112	3	5	884	3	50.67
112	3	6	475	3	86.52
112	4	1	42	2	42.56
112	5	1	280	3	55.74
112	5	2	600	2	33.98
112	5	3	629	12	266.09
112	5	4	766	3	89.58
113	1	1	261	1	15.37
114	1	1	19	1	29.22
114	1	2	418	1	10.33
114	2	1	922	1	23.00
114	3	1	186	3	25.59
114	3	2	947	3	58.59
114	3	3	112	2	15.90
114	3	4	343	3	17.10
114	3	5	503	3	74.37
114	3	6	2	2	56.30
114	4	1	225	1	16.96
114	4	2	997	1	9.03
114	4	3	208	1	10.19
114	4	4	849	1	28.96
114	4	5	410	12	192.94
115	1	1	148	12	145.64
115	1	2	108	12	178.31
115	1	3	894	3	23.64
115	1	4	626	2	16.56
115	2	1	913	3	65.04
115	2	2	191	1	17.29
115	2	3	456	2	51.32
115	2	4	482	2	49.90
115	2	5	901	3	47.01
115	2	6	540	12	295.35
115	3	1	281	12	61.93
115	3	2	255	12	302.28
115	3	3	23	3	79.68
115	3	4	1026	2	52.72
116	1	1	976	12	114.51
116	1	2	311	2	48.44
116	1	3	878	1	22.43
116	1	4	871	3	30.66
116	1	5	998	1	12.30
116	1	6	852	2	52.18
116	2	1	545	12	188.10
116	2	2	776	12	259.93
116	2	3	393	3	83.10
116	2	4	765	12	328.68
116	2	5	426	1	8.67
116	3	1	600	2	33.98
116	3	2	1027	3	86.19
117	1	1	558	12	314.49
117	2	1	669	3	40.86
117	3	1	718	12	111.10
117	3	2	302	3	68.10
117	3	3	433	12	234.41
117	3	4	478	2	35.66
117	3	5	885	12	101.42
117	3	6	610	12	65.56
118	1	1	213	1	25.17
118	1	2	46	3	47.01
118	1	3	435	1	11.85
118	2	1	206	12	270.60
118	2	2	140	12	65.45
118	2	3	372	3	30.63
118	2	4	830	2	17.16
118	2	5	150	3	25.98
118	3	1	244	2	24.42
118	3	2	898	2	44.04
118	3	3	236	2	44.02
118	3	4	392	3	39.51
118	3	5	205	12	142.89
118	4	1	260	2	14.68
118	4	2	1033	1	29.37
118	4	3	782	1	12.06
118	5	1	768	2	16.16
118	6	1	57	1	17.60
118	6	2	187	1	6.44
118	6	3	186	2	17.06
118	6	4	480	1	28.89
119	1	1	494	2	56.70
119	1	2	294	2	15.82
119	1	3	632	2	32.96
119	2	1	897	3	68.97
119	2	2	671	12	288.09
119	2	3	766	12	328.46
119	2	4	53	1	26.62
119	3	1	350	12	153.89
119	3	2	119	12	196.02
119	3	3	389	2	35.04
119	3	4	222	12	266.20
119	3	5	870	2	45.78
119	4	1	427	2	40.02
119	4	2	536	1	24.42
119	4	3	106	3	78.30
119	4	4	354	2	12.38
119	5	1	402	1	19.08
119	6	1	783	2	55.74
119	6	2	587	3	31.14
119	6	3	128	1	21.25
120	1	1	1030	1	21.21
121	1	1	949	12	118.47
121	1	2	1033	1	29.37
121	1	3	736	2	52.12
121	2	1	151	2	44.74
121	2	2	867	12	172.48
121	3	1	769	1	5.74
121	3	2	209	1	14.80
121	3	3	1042	2	53.52
122	1	1	570	2	50.18
122	1	2	560	3	68.73
122	1	3	189	12	184.03
122	2	1	308	3	39.18
122	2	2	718	3	30.30
122	2	3	296	3	29.85
122	2	4	897	2	45.98
122	3	1	801	1	5.17
122	3	2	1025	2	57.56
122	3	3	662	3	69.87
122	3	4	868	12	128.70
122	4	1	543	12	308.66
123	1	1	728	1	5.48
123	1	2	178	2	22.42
123	1	3	506	3	73.11
123	1	4	133	3	52.62
123	2	1	791	2	10.38
123	2	2	566	3	21.12
123	2	3	1020	3	16.53
123	2	4	261	12	169.07
123	3	1	975	3	27.60
123	3	2	758	1	18.06
123	3	3	969	1	13.93
123	3	4	830	1	8.58
123	4	1	182	3	22.29
123	4	2	659	3	73.14
123	5	1	976	1	10.41
123	5	2	293	12	239.69
123	5	3	616	1	12.39
123	5	4	376	2	42.34
123	5	5	323	2	27.20
123	5	6	540	1	26.85
124	1	1	396	1	11.95
124	1	2	471	1	28.64
124	1	3	317	1	5.79
124	1	4	490	12	91.41
124	1	5	561	1	8.64
124	2	1	652	1	10.28
124	2	2	536	12	268.62
125	1	1	652	2	20.56
125	1	2	789	3	42.93
125	1	3	838	12	212.30
125	2	1	893	3	73.32
125	2	2	509	1	15.58
125	2	3	464	3	45.21
125	3	1	103	12	136.07
125	3	2	979	3	85.17
125	3	3	370	3	18.60
125	3	4	1025	1	28.78
125	3	5	257	12	328.02
125	3	6	994	1	15.44
125	4	1	438	12	117.37
125	4	2	1027	12	316.03
125	4	3	708	3	27.36
125	4	4	401	12	184.25
125	4	5	375	2	46.80
125	4	6	714	1	25.79
126	1	1	1015	12	311.85
126	2	1	755	2	26.50
126	3	1	269	3	46.08
126	3	2	237	12	280.28
126	3	3	810	12	304.81
126	3	4	624	12	244.75
126	3	5	51	3	72.27
126	4	1	879	1	8.93
126	4	2	58	2	53.20
126	4	3	184	12	298.21
126	4	4	989	3	36.27
126	4	5	506	2	48.74
126	4	6	6	12	222.75
127	1	1	684	12	248.05
127	1	2	870	2	45.78
127	1	3	524	1	15.26
127	1	4	1028	1	13.90
127	1	5	327	12	277.20
127	2	1	577	3	63.18
127	2	2	295	12	275.55
127	2	3	750	1	6.57
127	2	4	570	2	50.18
127	2	5	281	3	16.89
127	3	1	247	12	279.40
127	4	1	897	1	22.99
127	4	2	174	12	282.48
127	4	3	858	3	15.99
127	4	4	644	1	28.46
128	1	1	879	2	17.86
128	1	2	386	12	312.95
128	2	1	932	1	21.07
128	2	2	797	2	36.50
128	2	3	109	12	176.00
128	2	4	642	1	13.91
128	2	5	628	2	19.14
128	3	1	479	1	11.70
128	3	2	952	2	42.96
128	3	3	569	2	26.80
128	3	4	116	2	23.74
128	3	5	692	1	17.90
128	4	1	500	12	179.08
128	4	2	443	1	29.32
128	4	3	826	1	20.90
128	4	4	436	2	11.98
128	5	1	768	12	88.88
129	1	1	75	1	22.73
129	1	2	690	1	20.07
129	1	3	829	2	21.20
129	1	4	606	3	75.93
129	1	5	757	3	64.11
129	2	1	129	1	5.75
129	3	1	191	12	190.19
129	3	2	439	12	210.43
129	3	3	598	2	25.86
130	1	1	874	3	58.20
130	1	2	1040	12	309.32
130	2	1	858	2	10.66
130	2	2	37	2	28.30
130	2	3	567	3	37.11
130	2	4	688	2	31.54
130	2	5	985	3	61.02
130	3	1	197	3	35.19
130	3	2	954	1	5.51
130	3	3	341	12	191.40
130	3	4	980	12	245.52
130	4	1	306	2	52.94
130	4	2	235	12	240.35
130	4	3	811	12	63.25
130	4	4	174	3	77.04
130	4	5	424	1	8.58
130	5	1	853	12	321.53
130	5	2	386	1	28.45
130	5	3	706	2	52.04
130	6	1	742	12	250.69
130	6	2	839	12	214.50
130	6	3	691	3	87.00
130	6	4	850	12	322.52
131	1	1	1005	12	204.27
131	1	2	135	3	34.74
131	2	1	559	2	13.34
131	2	2	830	1	8.58
132	1	1	778	1	8.86
133	1	1	652	12	113.08
133	1	2	1045	12	206.36
133	1	3	194	3	45.30
133	1	4	95	3	87.57
133	1	5	900	3	50.82
133	2	1	575	1	25.67
133	3	1	777	3	71.10
133	3	2	733	12	106.81
133	4	1	314	2	49.84
133	5	1	227	3	50.28
133	5	2	344	2	30.12
133	5	3	618	12	319.77
133	5	4	47	3	72.12
133	5	5	657	12	191.73
133	6	1	769	3	17.22
133	6	2	53	2	53.24
133	6	3	537	2	52.60
133	6	4	907	2	44.10
133	6	5	286	1	27.29
133	6	6	927	12	166.43
134	1	1	440	3	16.05
134	1	2	626	2	16.56
134	1	3	388	12	268.29
134	1	4	131	12	206.14
134	1	5	129	1	5.75
134	1	6	766	12	328.46
134	2	1	422	2	38.72
134	2	2	539	2	20.96
134	2	3	195	1	22.53
134	2	4	439	3	57.39
134	3	1	660	2	24.22
134	3	2	734	12	201.41
134	3	3	287	12	133.65
134	3	4	1037	1	26.67
134	3	5	710	3	87.81
134	4	1	725	1	24.18
134	4	2	647	12	322.52
134	5	1	44	12	244.20
134	5	2	375	3	70.20
134	5	3	283	1	28.83
134	5	4	447	3	40.74
135	1	1	255	12	302.28
135	2	1	221	12	134.53
135	2	2	725	1	24.18
135	3	1	763	1	7.68
135	3	2	379	3	25.17
135	3	3	623	12	243.10
135	3	4	172	3	89.67
135	3	5	249	3	77.91
135	4	1	223	2	26.08
135	4	2	791	2	10.38
135	4	3	258	2	35.36
135	4	4	484	12	318.12
135	5	1	497	3	64.38
135	6	1	730	2	49.32
135	6	2	244	2	24.42
135	6	3	315	2	51.86
135	6	4	931	3	43.02
135	6	5	791	3	15.57
136	1	1	286	1	27.29
136	1	2	678	2	52.90
136	1	3	742	2	45.58
136	2	1	298	12	271.04
136	2	2	20	3	29.10
136	2	3	184	2	54.22
136	2	4	541	3	52.86
136	2	5	77	1	8.84
136	3	1	368	2	56.74
136	3	2	199	12	223.08
136	3	3	827	12	224.84
136	3	4	296	12	109.45
136	3	5	706	1	26.02
136	3	6	98	2	48.98
136	4	1	228	3	65.79
136	4	2	487	2	59.24
136	4	3	885	1	9.22
136	4	4	381	1	20.86
136	4	5	108	2	32.42
136	5	1	152	12	63.91
136	5	2	323	12	149.60
136	6	1	188	12	84.81
136	6	2	204	2	48.56
136	6	3	501	3	67.17
137	1	1	947	1	19.53
137	1	2	138	1	21.51
137	1	3	825	2	59.26
137	1	4	311	12	266.42
137	2	1	852	1	26.09
137	2	2	659	3	73.14
137	2	3	292	1	7.84
137	3	1	550	1	11.22
137	3	2	426	2	17.34
137	3	3	263	3	39.36
137	3	4	607	1	22.19
137	3	5	1046	12	121.00
137	4	1	404	12	297.11
137	4	2	426	1	8.67
137	5	1	314	3	74.76
138	1	1	1003	12	107.47
138	1	2	739	12	278.08
138	1	3	635	3	64.17
138	1	4	431	3	53.58
138	1	5	917	3	55.29
138	2	1	152	2	11.62
138	2	2	761	1	18.88
138	2	3	777	1	23.70
138	2	4	134	3	67.35
138	2	5	950	2	22.12
138	2	6	854	12	272.25
138	3	1	45	12	84.81
138	3	2	271	12	75.79
138	3	3	716	1	13.85
138	3	4	822	12	121.33
138	3	5	127	1	17.86
138	4	1	64	3	78.99
138	4	2	408	12	175.45
138	4	3	189	1	16.73
138	4	4	829	2	21.20
138	4	5	887	3	45.30
138	5	1	161	3	55.89
138	5	2	762	12	308.44
138	5	3	592	12	168.52
138	5	4	195	1	22.53
138	6	1	993	3	73.11
138	6	2	833	1	19.63
138	6	3	742	3	68.37
138	6	4	610	3	17.88
138	6	5	165	1	27.17
139	1	1	739	1	25.28
139	2	1	279	1	15.97
139	3	1	497	3	64.38
139	3	2	24	3	55.80
139	3	3	885	12	101.42
139	4	1	1006	12	221.76
139	4	2	986	3	19.68
139	5	1	188	2	15.42
140	1	1	378	12	326.48
140	1	2	211	2	35.16
140	1	3	31	12	184.14
141	1	1	55	3	64.23
141	1	2	803	1	20.69
141	1	3	461	1	10.66
141	1	4	245	3	86.58
141	1	5	428	2	12.48
141	2	1	793	1	16.37
141	2	2	626	1	8.28
141	2	3	240	3	34.32
141	3	1	538	2	29.00
142	1	1	1013	1	10.04
142	1	2	411	3	71.97
142	1	3	121	12	319.22
142	1	4	1002	3	71.91
142	1	5	570	1	25.09
142	2	1	368	3	85.11
142	3	1	919	12	146.85
142	3	2	642	2	27.82
142	3	3	514	3	48.54
142	3	4	647	12	322.52
142	3	5	90	3	20.31
142	3	6	703	1	26.77
142	4	1	873	2	41.30
143	1	1	777	12	260.70
143	1	2	349	3	46.77
143	1	3	558	12	314.49
144	1	1	410	12	192.94
144	1	2	455	3	86.97
144	2	1	651	1	29.92
144	2	2	106	12	287.10
144	2	3	152	3	17.43
144	2	4	690	1	20.07
144	3	1	35	12	194.04
144	4	1	916	3	62.19
144	4	2	144	1	26.21
144	4	3	82	12	245.30
144	5	1	287	2	24.30
144	5	2	351	2	50.94
144	5	3	1018	1	9.22
144	6	1	750	1	6.57
144	6	2	573	2	40.72
145	1	1	682	3	82.02
145	1	2	354	2	12.38
145	1	3	1043	12	184.03
145	2	1	18	2	16.90
145	2	2	203	3	44.61
145	2	3	686	2	39.04
145	2	4	324	3	61.11
145	3	1	440	1	5.35
146	1	1	229	3	20.97
146	1	2	894	3	23.64
146	1	3	894	2	15.76
146	1	4	604	1	12.83
146	1	5	643	1	19.37
146	1	6	816	1	11.57
146	2	1	219	2	12.90
146	2	2	1009	2	44.26
146	2	3	861	1	13.85
146	2	4	241	3	50.85
146	3	1	895	12	275.66
147	1	1	159	1	14.76
147	2	1	309	3	58.20
147	2	2	721	3	15.21
147	2	3	234	1	26.40
147	2	4	630	2	56.00
147	3	1	188	1	7.71
147	3	2	49	12	111.98
147	3	3	815	1	6.44
147	3	4	897	12	252.89
147	3	5	303	1	12.35
147	3	6	669	3	40.86
147	4	1	394	2	10.76
148	1	1	315	3	77.79
148	1	2	840	2	29.26
148	1	3	797	3	54.75
148	1	4	172	12	328.79
149	1	1	448	2	11.56
149	1	2	145	2	52.94
149	2	1	381	3	62.58
149	3	1	383	3	56.67
149	3	2	279	12	175.67
150	1	1	927	12	166.43
150	1	2	393	2	55.40
150	1	3	282	1	27.83
150	1	4	74	12	138.16
150	1	5	731	1	25.68
150	1	6	128	2	42.50
150	2	1	270	1	20.46
150	2	2	101	1	8.73
150	2	3	800	3	54.48
150	2	4	314	1	24.92
150	2	5	51	3	72.27
150	3	1	488	1	5.10
150	3	2	515	1	25.09
150	3	3	187	3	19.32
150	3	4	543	1	28.06
150	3	5	753	12	238.70
150	4	1	969	2	27.86
150	4	2	185	1	15.92
150	4	3	532	1	27.88
150	4	4	950	1	11.06
151	1	1	947	1	19.53
151	1	2	744	3	40.62
151	1	3	624	12	244.75
151	1	4	245	2	57.72
152	1	1	1025	3	86.34
152	1	2	900	2	33.88
152	1	3	998	12	135.30
152	1	4	736	1	26.06
152	2	1	730	3	73.98
152	2	2	992	1	10.51
152	3	1	274	12	68.42
152	3	2	40	12	170.72
152	3	3	666	1	15.55
153	1	1	612	1	29.58
153	1	2	212	2	59.60
153	1	3	639	2	47.06
153	2	1	891	2	47.20
153	2	2	20	2	19.40
153	2	3	756	1	29.52
153	2	4	379	3	25.17
153	2	5	684	2	45.10
153	3	1	943	3	48.39
153	4	1	967	2	14.96
154	1	1	175	12	281.60
154	1	2	850	12	322.52
154	2	1	639	2	47.06
154	2	2	923	12	161.04
154	3	1	456	1	25.66
154	3	2	971	3	59.43
154	3	3	465	2	16.00
154	3	4	747	12	296.78
154	3	5	798	12	324.28
155	1	1	33	2	37.50
155	1	2	541	2	35.24
155	1	3	371	2	30.98
155	1	4	699	2	39.42
155	1	5	265	3	50.19
155	1	6	325	1	13.21
155	2	1	23	12	292.16
155	2	2	602	12	128.92
155	2	3	681	1	28.12
155	2	4	627	12	72.38
155	2	5	631	2	10.00
155	3	1	473	2	14.56
155	3	2	129	3	17.25
155	3	3	870	2	45.78
155	3	4	887	1	15.10
155	3	5	185	1	15.92
155	3	6	591	2	25.40
155	4	1	101	1	8.73
155	4	2	279	3	47.91
155	4	3	278	2	49.12
155	5	1	421	12	157.30
155	5	2	403	1	24.98
155	5	3	193	1	24.18
155	5	4	487	2	59.24
155	5	5	481	1	5.82
155	6	1	906	2	20.66
155	6	2	795	1	21.46
155	6	3	154	1	5.97
155	6	4	437	2	17.22
155	6	5	607	1	22.19
156	1	1	799	12	152.57
156	1	2	861	1	13.85
156	1	3	352	12	107.25
156	2	1	83	1	15.54
156	3	1	863	1	13.71
156	3	2	160	3	89.76
156	3	3	416	2	50.78
156	4	1	269	2	30.72
157	1	1	226	2	17.36
157	1	2	636	2	51.02
157	1	3	143	1	12.49
158	1	1	979	3	85.17
158	1	2	659	12	268.18
158	1	3	485	1	22.86
158	1	4	38	3	17.25
158	1	5	406	1	6.72
158	1	6	864	12	258.28
158	2	1	155	3	74.49
158	2	2	403	2	49.96
158	3	1	691	1	29.00
158	3	2	884	2	33.78
158	4	1	889	3	41.76
158	5	1	713	12	275.66
158	5	2	254	1	22.55
158	5	3	579	1	7.68
158	5	4	516	2	21.74
158	5	5	896	3	82.68
158	5	6	688	3	47.31
158	6	1	869	12	197.23
158	6	2	910	3	84.87
158	6	3	656	3	76.29
158	6	4	701	2	48.64
158	6	5	129	1	5.75
159	1	1	452	2	15.56
159	1	2	279	1	15.97
159	1	3	1004	12	265.98
159	2	1	95	3	87.57
159	2	2	49	12	111.98
159	3	1	696	3	19.86
159	3	2	701	12	267.52
159	4	1	919	2	26.70
159	4	2	744	2	27.08
159	4	3	966	1	10.45
159	4	4	624	3	66.75
159	4	5	359	3	55.83
159	4	6	365	2	16.80
160	1	1	310	1	10.26
160	1	2	335	1	9.58
160	1	3	525	12	242.99
160	1	4	499	2	14.24
160	2	1	315	1	25.93
160	3	1	1015	12	311.85
160	3	2	717	2	47.84
160	3	3	549	1	6.36
160	3	4	1045	12	206.36
160	3	5	263	2	26.24
161	1	1	878	12	246.73
161	1	2	552	12	58.30
161	1	3	202	3	30.24
161	2	1	472	1	8.36
161	2	2	400	12	107.36
161	2	3	679	3	35.43
161	2	4	831	2	47.22
161	2	5	880	1	26.62
161	3	1	220	2	23.48
161	3	2	365	3	25.20
161	3	3	217	3	20.49
161	4	1	385	3	62.43
161	4	2	125	3	26.46
161	4	3	451	12	136.95
161	4	4	354	1	6.19
161	4	5	244	1	12.21
161	4	6	348	3	55.08
161	5	1	489	2	42.70
161	5	2	465	3	24.00
161	5	3	1031	12	75.57
161	5	4	460	3	58.05
161	5	5	713	1	25.06
161	6	1	588	3	34.38
161	6	2	166	12	90.86
161	6	3	772	2	26.58
161	6	4	515	3	75.27
161	6	5	191	1	17.29
161	6	6	2	2	56.30
162	1	1	964	2	26.66
162	2	1	757	3	64.11
162	2	2	814	2	27.04
163	1	1	547	12	272.25
163	1	2	439	1	19.13
163	2	1	86	1	15.04
163	3	1	691	3	87.00
163	3	2	588	2	22.92
163	4	1	649	1	17.09
163	4	2	887	12	166.10
163	4	3	731	12	282.48
163	5	1	942	1	28.23
163	5	2	507	2	36.04
163	5	3	60	2	32.42
163	5	4	772	3	39.87
163	5	5	967	2	14.96
163	5	6	192	1	22.37
164	1	1	640	2	22.90
164	1	2	1002	3	71.91
164	2	1	529	1	29.10
164	2	2	727	12	90.75
164	3	1	31	12	184.14
164	3	2	973	2	32.04
164	3	3	880	1	26.62
164	3	4	135	2	23.16
164	3	5	396	3	35.85
164	3	6	910	2	56.58
164	4	1	90	1	6.77
164	4	2	528	12	108.24
164	4	3	176	3	62.22
164	5	1	760	2	12.24
164	5	2	117	3	74.79
164	5	3	431	1	17.86
164	5	4	1042	12	294.36
164	5	5	1025	12	316.58
164	6	1	252	2	51.30
164	6	2	135	3	34.74
164	6	3	144	2	52.42
164	6	4	345	12	90.53
164	6	5	167	3	63.15
164	6	6	672	1	10.92
165	1	1	313	3	60.66
165	1	2	749	1	5.57
165	1	3	719	12	111.76
165	1	4	585	12	295.35
165	1	5	772	12	146.19
166	1	1	260	2	14.68
166	2	1	811	2	11.50
166	2	2	1025	3	86.34
166	3	1	138	12	236.61
166	3	2	330	12	226.60
166	3	3	956	3	60.33
166	4	1	460	12	212.85
166	4	2	1031	2	13.74
166	4	3	251	1	5.71
166	4	4	497	1	21.46
166	5	1	232	12	157.63
166	5	2	828	2	33.02
167	1	1	386	1	28.45
167	1	2	532	12	306.68
167	1	3	570	2	50.18
167	2	1	778	1	8.86
167	2	2	52	3	80.19
167	2	3	1044	2	38.24
167	2	4	1003	12	107.47
167	2	5	151	2	44.74
167	3	1	300	1	26.38
167	4	1	1022	1	6.25
167	4	2	843	3	24.33
167	4	3	802	3	36.72
167	4	4	991	12	292.16
167	5	1	748	3	68.28
167	5	2	168	2	48.74
167	5	3	432	12	279.95
167	5	4	404	3	81.03
167	6	1	786	3	62.52
167	6	2	464	3	45.21
167	6	3	448	1	5.78
168	1	1	525	1	22.09
168	1	2	955	1	18.91
169	1	1	689	12	110.33
169	1	2	426	2	17.34
169	2	1	847	12	206.69
169	2	2	867	1	15.68
169	2	3	264	1	12.84
169	2	4	843	2	16.22
169	2	5	540	12	295.35
170	1	1	98	2	48.98
170	1	2	910	12	311.19
170	2	1	1034	2	29.92
170	2	2	509	12	171.38
170	2	3	298	3	73.92
170	2	4	185	2	31.84
170	2	5	499	3	21.36
170	3	1	341	12	191.40
170	3	2	674	2	29.74
170	3	3	246	3	55.71
171	1	1	206	2	49.20
171	1	2	186	1	8.53
171	1	3	804	12	284.90
171	1	4	312	1	11.93
171	1	5	341	12	191.40
171	2	1	860	2	58.48
171	2	2	360	1	11.94
171	2	3	589	12	237.16
171	2	4	582	3	62.64
171	2	5	674	12	163.57
172	1	1	212	2	59.60
172	1	2	489	2	42.70
172	2	1	835	1	14.56
172	2	2	784	1	8.87
172	2	3	483	12	132.66
172	2	4	717	2	47.84
172	2	5	568	2	22.64
172	2	6	1004	2	48.36
172	3	1	29	1	20.97
172	3	2	68	2	43.32
172	3	3	342	3	33.66
172	3	4	926	2	49.20
172	3	5	172	1	29.89
172	4	1	866	12	160.49
172	4	2	823	2	46.40
172	4	3	78	3	46.50
173	1	1	457	1	21.00
173	1	2	7	12	134.20
173	1	3	520	3	30.03
173	1	4	852	12	286.99
173	1	5	944	12	219.34
173	1	6	904	12	204.93
173	2	1	134	1	22.45
173	2	2	966	12	114.95
173	2	3	902	12	118.47
173	2	4	441	1	5.87
173	2	5	136	12	278.85
173	2	6	240	1	11.44
173	3	1	908	3	35.22
173	4	1	988	1	11.17
173	5	1	376	1	21.17
173	5	2	879	1	8.93
173	5	3	444	12	199.76
173	5	4	475	12	317.24
173	5	5	1034	3	44.88
173	6	1	230	3	33.72
173	6	2	955	12	208.01
174	1	1	260	1	7.34
174	1	2	231	2	35.12
174	1	3	920	2	40.56
174	1	4	908	12	129.14
174	1	5	902	3	32.31
174	1	6	811	3	17.25
174	2	1	322	2	33.94
174	2	2	224	1	23.92
174	3	1	892	2	14.50
174	3	2	1030	1	21.21
174	3	3	916	12	228.03
174	3	4	501	3	67.17
174	3	5	678	2	52.90
174	3	6	449	2	56.76
174	4	1	134	2	44.90
174	4	2	271	2	13.78
174	4	3	472	2	16.72
174	4	4	428	2	12.48
174	4	5	1004	2	48.36
175	1	1	833	3	58.89
175	1	2	408	1	15.95
175	1	3	124	12	301.62
175	1	4	1026	12	289.96
175	1	5	840	12	160.93
175	2	1	753	2	43.40
175	2	2	687	1	16.89
175	3	1	648	12	179.74
175	4	1	549	3	19.08
176	1	1	577	12	231.66
176	1	2	842	1	28.53
176	1	3	990	1	12.78
176	2	1	567	2	24.74
176	2	2	616	1	12.39
176	3	1	173	12	291.72
176	3	2	570	2	50.18
176	3	3	79	3	35.91
176	3	4	918	3	47.10
176	3	5	367	3	59.61
176	4	1	786	12	229.24
176	4	2	858	2	10.66
176	4	3	404	2	54.02
176	4	4	397	1	11.38
176	5	1	295	2	50.10
176	5	2	883	2	18.60
176	5	3	980	3	66.96
176	6	1	679	3	35.43
176	6	2	564	2	37.78
176	6	3	453	2	29.54
176	6	4	720	12	309.32
176	6	5	798	1	29.48
176	6	6	1029	2	30.38
177	1	1	721	2	10.14
177	1	2	108	2	32.42
177	2	1	643	3	58.11
178	1	1	859	12	112.86
178	1	2	1008	12	315.15
178	1	3	493	3	67.17
178	1	4	690	2	40.14
178	1	5	692	12	196.90
178	2	1	488	2	10.20
178	2	2	272	2	19.20
178	3	1	742	3	68.37
178	3	2	380	3	82.35
178	3	3	680	2	10.50
178	3	4	382	1	7.09
178	4	1	816	12	127.27
178	4	2	1037	1	26.67
178	4	3	59	2	57.62
178	4	4	395	2	33.98
179	1	1	776	3	70.89
179	1	2	145	2	52.94
179	1	3	325	2	26.42
179	1	4	662	2	46.58
179	1	5	476	3	15.27
179	2	1	792	2	12.98
179	3	1	175	1	25.60
179	3	2	746	1	11.12
179	3	3	1023	2	30.98
179	3	4	861	1	13.85
179	3	5	754	1	25.04
179	4	1	531	3	74.34
179	5	1	958	3	32.49
179	5	2	24	12	204.60
179	5	3	278	1	24.56
179	5	4	144	12	288.31
179	5	5	366	1	24.12
179	5	6	449	1	28.38
179	6	1	526	2	38.62
179	6	2	1010	1	19.18
179	6	3	1047	12	152.35
179	6	4	832	12	97.68
179	6	5	545	12	188.10
180	1	1	989	12	132.99
180	1	2	990	1	12.78
180	1	3	197	1	11.73
180	1	4	296	2	19.90
180	2	1	917	2	36.86
180	2	2	575	3	77.01
180	2	3	140	2	11.90
180	2	4	272	1	9.60
180	3	1	676	3	37.05
180	3	2	427	3	60.03
180	3	3	797	3	54.75
180	3	4	737	1	26.19
180	4	1	983	12	287.21
180	5	1	257	3	89.46
180	5	2	638	1	5.73
180	5	3	630	2	56.00
180	5	4	794	1	10.52
180	6	1	720	3	84.36
180	6	2	2	2	56.30
180	6	3	43	1	27.82
180	6	4	565	12	264.22
180	6	5	993	12	268.07
180	6	6	485	3	68.58
181	1	1	690	1	20.07
181	1	2	497	12	236.06
181	1	3	252	3	76.95
181	1	4	773	1	25.40
181	1	5	416	2	50.78
181	2	1	72	12	274.56
181	2	2	982	1	28.67
181	2	3	427	3	60.03
181	2	4	603	12	63.58
181	2	5	41	12	186.89
181	2	6	393	1	27.70
181	3	1	214	2	55.90
181	3	2	42	3	63.84
181	3	3	699	12	216.81
181	3	4	716	3	41.55
181	3	5	989	1	12.09
181	3	6	587	12	114.18
181	4	1	712	2	56.42
181	4	2	687	1	16.89
181	4	3	561	1	8.64
181	4	4	153	2	46.22
181	4	5	983	3	78.33
181	4	6	989	12	132.99
182	1	1	918	2	31.40
182	1	2	164	12	103.40
182	1	3	720	3	84.36
182	1	4	709	1	6.71
182	2	1	56	12	141.13
182	2	2	471	2	57.28
182	3	1	614	3	26.79
182	3	2	502	12	265.21
182	3	3	229	2	13.98
182	3	4	198	2	27.62
182	4	1	813	12	106.48
182	4	2	454	2	56.62
182	4	3	167	2	42.10
182	4	4	97	3	36.33
182	4	5	865	12	325.38
182	4	6	804	1	25.90
182	5	1	217	3	20.49
182	5	2	816	2	23.14
182	5	3	989	12	132.99
182	6	1	459	1	11.96
182	6	2	262	3	66.57
182	6	3	639	2	47.06
182	6	4	214	2	55.90
182	6	5	143	12	137.39
182	6	6	570	3	75.27
183	1	1	719	12	111.76
183	1	2	141	3	67.41
183	2	1	138	1	21.51
183	2	2	447	3	40.74
183	3	1	461	3	31.98
183	3	2	81	12	280.72
183	3	3	994	3	46.32
183	3	4	869	2	35.86
183	3	5	932	3	63.21
183	4	1	868	3	35.10
183	4	2	801	12	56.87
183	4	3	897	1	22.99
183	4	4	719	3	30.48
183	5	1	242	12	311.52
183	5	2	232	1	14.33
184	1	1	138	3	64.53
184	1	2	259	3	31.56
184	1	3	419	12	121.44
184	1	4	506	2	48.74
184	1	5	774	3	17.04
184	2	1	122	12	243.43
184	2	2	428	12	68.64
184	3	1	997	2	18.06
184	3	2	345	1	8.23
184	3	3	933	3	82.41
184	3	4	840	2	29.26
184	4	1	612	2	59.16
184	4	2	1032	1	19.93
184	4	3	552	1	5.30
184	4	4	902	12	118.47
185	1	1	655	3	63.96
185	1	2	925	3	50.91
185	1	3	629	2	48.38
185	1	4	532	12	306.68
186	1	1	601	3	42.87
186	1	2	421	1	14.30
186	2	1	64	1	26.33
186	2	2	797	3	54.75
186	2	3	891	2	47.20
186	3	1	896	2	55.12
186	3	2	1028	3	41.70
186	3	3	752	3	25.77
186	3	4	973	2	32.04
186	3	5	445	2	42.86
186	3	6	408	1	15.95
186	4	1	208	1	10.19
186	4	2	418	1	10.33
186	4	3	728	12	60.28
186	4	4	634	12	86.57
186	4	5	890	3	59.34
186	5	1	429	1	16.52
186	5	2	234	12	290.40
186	5	3	399	3	43.89
186	5	4	76	12	197.34
186	5	5	34	1	14.28
186	5	6	727	1	8.25
186	6	1	808	12	284.46
186	6	2	327	3	75.60
186	6	3	550	1	11.22
186	6	4	354	1	6.19
187	1	1	629	1	24.19
187	1	2	367	3	59.61
187	2	1	2	3	84.45
187	2	2	534	3	48.45
188	1	1	152	2	11.62
188	1	2	111	2	55.76
188	1	3	864	2	46.96
188	1	4	769	1	5.74
188	1	5	970	12	314.27
188	1	6	474	2	53.44
188	2	1	124	1	27.42
188	3	1	441	12	64.57
188	3	2	852	3	78.27
188	3	3	248	12	287.98
188	3	4	936	1	20.34
188	3	5	255	3	82.44
188	3	6	172	1	29.89
189	1	1	969	1	13.93
189	1	2	858	1	5.33
189	1	3	803	2	41.38
189	1	4	575	12	282.37
189	1	5	11	1	16.48
189	2	1	931	1	14.34
189	2	2	629	3	72.57
189	2	3	398	12	320.54
189	3	1	294	12	87.01
190	1	1	451	2	24.90
190	1	2	25	2	29.82
190	1	3	870	1	22.89
190	1	4	528	1	9.84
190	2	1	98	12	269.39
190	2	2	400	3	29.28
191	1	1	663	12	79.53
191	1	2	523	1	26.78
191	1	3	247	12	279.40
191	1	4	261	3	46.11
191	1	5	754	2	50.08
191	2	1	262	12	244.09
191	2	2	1044	1	19.12
191	2	3	307	3	15.84
191	2	4	79	12	131.67
191	2	5	650	3	70.02
191	2	6	525	3	66.27
191	3	1	654	12	294.80
191	3	2	790	2	24.86
191	4	1	575	1	25.67
191	4	2	764	3	34.56
191	4	3	448	1	5.78
191	4	4	222	3	72.60
191	4	5	834	12	248.16
191	5	1	279	2	31.94
191	5	2	450	12	158.62
191	6	1	700	2	38.70
191	6	2	198	1	13.81
191	6	3	643	12	213.07
192	1	1	29	2	41.94
192	1	2	63	3	71.16
192	1	3	108	2	32.42
192	1	4	865	2	59.16
192	1	5	783	3	83.61
192	2	1	649	12	187.99
192	3	1	590	1	15.33
192	4	1	802	1	12.24
192	4	2	259	1	10.52
192	4	3	946	12	104.61
192	5	1	423	1	26.88
192	5	2	770	1	15.81
192	5	3	668	12	163.35
193	1	1	734	2	36.62
193	1	2	832	1	8.88
193	1	3	191	12	190.19
193	1	4	386	2	56.90
193	1	5	893	2	48.88
193	1	6	120	3	15.33
193	2	1	797	1	18.25
193	2	2	396	3	35.85
193	2	3	982	3	86.01
193	2	4	694	2	31.52
193	2	5	726	2	57.02
193	3	1	411	3	71.97
193	3	2	64	1	26.33
193	3	3	16	1	8.95
193	3	4	971	1	19.81
193	3	5	749	2	11.14
193	4	1	168	2	48.74
193	4	2	466	12	129.91
193	4	3	663	1	7.23
193	4	4	335	3	28.74
193	4	5	824	3	85.68
193	4	6	226	1	8.68
193	5	1	139	12	170.94
193	5	2	827	12	224.84
193	5	3	189	3	50.19
193	5	4	615	2	47.14
194	1	1	779	3	59.61
195	1	1	310	2	20.52
195	1	2	553	1	9.23
195	1	3	847	3	56.37
195	1	4	991	3	79.68
195	2	1	391	2	58.10
195	2	2	444	2	36.32
195	3	1	819	2	15.54
196	1	1	117	1	24.93
196	1	2	204	3	72.84
196	1	3	281	2	11.26
196	1	4	742	12	250.69
196	2	1	837	12	97.02
196	3	1	1025	12	316.58
196	3	2	634	12	86.57
196	3	3	350	12	153.89
196	3	4	385	2	41.62
196	4	1	723	3	62.49
196	5	1	772	2	26.58
196	5	2	477	3	71.67
196	5	3	930	2	51.02
196	5	4	123	12	84.70
196	5	5	304	3	67.38
196	5	6	664	3	78.24
197	1	1	939	3	43.11
197	1	2	211	3	52.74
197	1	3	1029	1	15.19
197	1	4	533	12	206.14
197	2	1	614	3	26.79
197	2	2	477	2	47.78
197	2	3	980	1	22.32
197	3	1	789	12	157.41
197	3	2	162	3	58.92
197	3	3	610	2	11.92
197	3	4	206	3	73.80
197	4	1	700	3	58.05
197	4	2	613	2	43.24
198	1	1	911	2	11.42
198	1	2	89	3	83.55
198	1	3	15	12	135.63
198	1	4	700	3	58.05
198	1	5	543	1	28.06
198	1	6	900	1	16.94
199	1	1	351	3	76.41
199	1	2	473	2	14.56
199	2	1	848	2	34.74
199	2	2	1034	2	29.92
199	2	3	493	12	246.29
199	2	4	1036	1	11.74
199	2	5	462	1	23.78
199	2	6	1039	3	79.38
199	3	1	297	2	50.54
199	3	2	237	3	76.44
199	3	3	771	12	168.85
200	1	1	912	12	252.34
200	1	2	434	12	279.84
200	1	3	991	3	79.68
200	1	4	245	12	317.46
200	1	5	564	12	207.79
200	1	6	423	1	26.88
200	2	1	494	1	28.35
200	3	1	349	3	46.77
200	3	2	505	1	27.63
200	3	3	525	12	242.99
200	3	4	1043	2	33.46
200	4	1	867	3	47.04
200	5	1	390	3	25.29
200	5	2	1027	2	57.46
200	5	3	196	12	227.15
200	5	4	1006	3	60.48
200	5	5	453	2	29.54
200	5	6	757	12	235.07
200	6	1	973	3	48.06
200	6	2	871	12	112.42
200	6	3	148	1	13.24
200	6	4	364	12	250.69
200	6	5	821	12	62.59
201	1	1	750	3	19.71
201	1	2	1030	1	21.21
201	1	3	398	12	320.54
201	1	4	518	1	15.35
201	2	1	905	3	34.32
201	2	2	837	12	97.02
201	2	3	75	3	68.19
201	2	4	760	1	6.12
202	1	1	876	3	38.19
202	1	2	630	12	308.00
202	1	3	85	3	64.62
202	1	4	510	1	11.03
202	2	1	687	12	185.79
202	2	2	356	3	67.29
202	2	3	1028	1	13.90
202	2	4	337	1	16.72
202	3	1	436	2	11.98
202	3	2	715	1	18.98
202	3	3	344	12	165.66
202	3	4	559	12	73.37
202	3	5	984	2	12.34
202	3	6	600	1	16.99
202	4	1	645	1	18.13
202	5	1	483	1	12.06
202	6	1	446	3	64.32
202	6	2	543	12	308.66
202	6	3	184	3	81.33
202	6	4	1040	3	84.36
203	1	1	113	12	211.75
204	1	1	1013	2	20.08
204	2	1	559	1	6.67
204	2	2	192	1	22.37
205	1	1	952	3	64.44
205	1	2	128	3	63.75
205	1	3	73	3	32.10
205	1	4	955	3	56.73
205	1	5	109	1	16.00
205	1	6	566	2	14.08
206	1	1	52	12	294.03
206	1	2	270	1	20.46
207	1	1	290	3	26.73
207	1	2	288	2	29.90
207	1	3	83	1	15.54
207	1	4	964	1	13.33
207	2	1	426	12	95.37
207	2	2	301	3	88.74
207	2	3	49	1	10.18
207	2	4	889	2	27.84
207	3	1	17	12	277.53
207	3	2	374	2	18.56
207	3	3	117	2	49.86
207	3	4	498	1	16.48
207	3	5	648	3	49.02
207	4	1	385	12	228.91
207	4	2	252	2	51.30
207	4	3	983	1	26.11
207	4	4	705	12	109.67
207	5	1	874	2	38.80
207	5	2	575	12	282.37
208	1	1	741	1	12.89
208	1	2	151	2	44.74
208	1	3	469	12	295.68
208	1	4	623	12	243.10
208	2	1	783	1	27.87
208	2	2	976	1	10.41
208	2	3	12	2	54.84
208	2	4	580	12	269.39
208	2	5	129	3	17.25
208	2	6	975	3	27.60
208	3	1	478	12	196.13
208	4	1	248	2	52.36
208	4	2	552	12	58.30
208	4	3	490	2	16.62
208	4	4	1036	2	23.48
208	5	1	409	12	245.74
208	5	2	737	2	52.38
208	5	3	900	1	16.94
209	1	1	695	12	264.33
209	2	1	942	3	84.69
209	2	2	884	2	33.78
210	1	1	326	3	39.48
210	1	2	911	3	17.13
210	1	3	458	1	8.01
210	1	4	1035	12	229.79
210	2	1	763	12	84.48
210	2	2	144	1	26.21
210	2	3	895	3	75.18
210	3	1	350	2	27.98
210	3	2	825	3	88.89
211	1	1	355	1	20.33
212	1	1	395	2	33.98
212	1	2	403	3	74.94
212	1	3	407	1	18.98
212	1	4	465	2	16.00
213	1	1	472	3	25.08
213	1	2	993	12	268.07
213	1	3	716	2	27.70
213	1	4	981	12	273.90
213	1	5	39	3	22.14
213	2	1	431	12	196.46
213	2	2	687	3	50.67
213	3	1	749	1	5.57
213	3	2	312	1	11.93
213	3	3	706	1	26.02
213	3	4	70	2	32.10
214	1	1	470	1	9.44
214	1	2	977	3	43.53
214	1	3	546	12	328.68
214	1	4	389	1	17.52
214	2	1	904	2	37.26
214	3	1	101	3	26.19
214	4	1	66	2	20.58
214	4	2	465	12	88.00
214	4	3	696	3	19.86
214	4	4	275	1	21.69
214	4	5	805	12	314.16
214	4	6	170	1	9.75
215	1	1	197	1	11.73
215	2	1	559	3	20.01
215	2	2	35	12	194.04
215	2	3	992	12	115.61
215	2	4	512	3	82.47
215	2	5	266	3	55.98
215	2	6	321	1	11.69
215	3	1	816	12	127.27
215	3	2	926	12	270.60
215	3	3	1041	1	17.18
215	3	4	832	12	97.68
215	3	5	1010	3	57.54
215	4	1	1004	3	72.54
215	4	2	344	1	15.06
215	4	3	103	3	37.11
215	4	4	225	12	186.56
215	4	5	100	1	23.46
215	5	1	943	1	16.13
215	6	1	760	1	6.12
216	1	1	825	2	59.26
216	1	2	300	12	290.18
216	1	3	524	3	45.78
216	1	4	41	3	50.97
217	1	1	734	2	36.62
217	1	2	9	12	269.17
217	1	3	447	3	40.74
217	1	4	977	2	29.02
217	2	1	735	1	22.11
217	2	2	840	3	43.89
217	2	3	687	3	50.67
217	2	4	652	2	20.56
217	3	1	34	3	42.84
217	3	2	1009	1	22.13
217	3	3	517	2	14.84
217	3	4	810	2	55.42
217	4	1	884	12	185.79
217	5	1	664	3	78.24
217	5	2	259	1	10.52
217	6	1	247	1	25.40
218	1	1	386	2	56.90
218	1	2	776	3	70.89
218	1	3	62	1	16.85
218	1	4	522	1	7.30
218	1	5	129	3	17.25
218	1	6	913	3	65.04
218	2	1	333	1	16.64
218	2	2	65	2	19.96
218	2	3	963	3	75.96
218	2	4	309	1	19.40
218	2	5	594	3	18.24
218	3	1	386	1	28.45
218	3	2	468	2	57.20
218	4	1	962	12	172.92
218	4	2	623	1	22.10
218	4	3	919	3	40.05
218	4	4	635	12	235.29
218	4	5	396	12	131.45
218	5	1	733	12	106.81
218	5	2	53	1	26.62
218	5	3	41	2	33.98
218	5	4	585	12	295.35
218	5	5	852	12	286.99
218	5	6	491	1	7.33
218	6	1	510	12	121.33
218	6	2	308	12	143.66
218	6	3	772	2	26.58
219	1	1	876	1	12.73
219	1	2	706	2	52.04
219	1	3	611	2	42.64
219	1	4	151	2	44.74
219	2	1	469	1	26.88
219	2	2	501	2	44.78
219	2	3	542	2	39.04
219	2	4	21	12	210.54
219	2	5	156	1	7.49
219	2	6	746	2	22.24
219	3	1	695	12	264.33
219	3	2	684	12	248.05
219	4	1	765	3	89.64
219	5	1	731	1	25.68
219	5	2	386	1	28.45
219	5	3	125	12	97.02
219	5	4	493	3	67.17
220	1	1	67	3	83.40
220	2	1	773	3	76.20
221	1	1	1003	12	107.47
221	2	1	963	3	75.96
221	2	2	420	12	314.05
221	3	1	839	1	19.50
221	3	2	494	12	311.85
221	3	3	354	1	6.19
221	3	4	453	2	29.54
221	3	5	957	2	11.40
221	3	6	426	12	95.37
221	4	1	657	12	191.73
221	4	2	965	1	25.85
221	5	1	319	3	44.10
221	5	2	995	12	154.88
221	5	3	308	2	26.12
221	5	4	891	3	70.80
221	5	5	635	12	235.29
221	5	6	176	1	20.74
221	6	1	211	12	193.38
221	6	2	847	1	18.79
221	6	3	7	3	36.60
221	6	4	348	1	18.36
221	6	5	198	12	151.91
221	6	6	749	1	5.57
222	1	1	970	2	57.14
222	1	2	424	12	94.38
223	1	1	256	12	308.11
223	1	2	95	2	58.38
223	1	3	626	2	16.56
223	1	4	273	12	65.23
223	1	5	204	12	267.08
223	1	6	913	2	43.36
224	1	1	298	2	49.28
224	2	1	785	12	309.21
224	2	2	528	3	29.52
225	1	1	1039	12	291.06
225	1	2	192	12	246.07
225	1	3	77	12	97.24
225	1	4	125	12	97.02
225	1	5	874	2	38.80
225	1	6	188	1	7.71
226	1	1	426	1	8.67
226	1	2	641	2	38.94
226	1	3	906	12	113.63
226	1	4	1008	12	315.15
226	1	5	71	3	22.50
226	1	6	908	1	11.74
226	2	1	809	2	18.38
226	2	2	779	12	218.57
226	2	3	725	2	48.36
226	2	4	393	1	27.70
226	3	1	715	3	56.94
226	3	2	893	3	73.32
226	3	3	520	12	110.11
226	3	4	303	2	24.70
226	3	5	1024	12	218.35
226	3	6	431	12	196.46
227	1	1	33	1	18.75
227	2	1	493	12	246.29
227	2	2	152	1	5.81
227	2	3	665	12	73.92
227	3	1	481	3	17.46
227	3	2	741	2	25.78
227	3	3	666	12	171.05
227	4	1	253	3	84.72
227	4	2	647	1	29.32
227	4	3	69	1	16.61
227	4	4	944	1	19.94
227	4	5	184	3	81.33
227	4	6	1046	12	121.00
228	1	1	807	3	29.61
228	1	2	196	3	61.95
228	1	3	351	3	76.41
228	1	4	523	1	26.78
229	1	1	808	3	77.58
229	1	2	764	12	126.72
229	1	3	144	3	78.63
229	1	4	739	1	25.28
229	2	1	256	2	56.02
229	2	2	602	12	128.92
229	2	3	820	2	59.32
229	3	1	732	12	297.99
229	3	2	167	2	42.10
229	3	3	514	12	177.98
229	3	4	434	3	76.32
230	1	1	635	3	64.17
230	2	1	823	1	23.20
230	2	2	643	12	213.07
230	2	3	269	2	30.72
230	3	1	274	12	68.42
230	3	2	522	1	7.30
230	3	3	345	1	8.23
230	4	1	447	2	27.16
230	4	2	316	12	316.58
230	4	3	829	1	10.60
231	1	1	365	12	92.40
231	2	1	264	1	12.84
231	2	2	200	2	23.76
231	2	3	618	2	58.14
231	2	4	586	1	17.70
231	3	1	123	2	15.40
231	3	2	961	12	110.88
231	3	3	742	2	45.58
231	3	4	884	1	16.89
231	3	5	297	1	25.27
231	4	1	102	2	13.42
231	4	2	565	3	72.06
231	5	1	771	3	46.05
231	5	2	1003	12	107.47
231	6	1	461	3	31.98
231	6	2	423	12	295.68
231	6	3	184	2	54.22
231	6	4	945	2	31.78
232	1	1	854	2	49.50
232	1	2	543	1	28.06
232	1	3	60	1	16.21
232	2	1	3	1	9.94
232	3	1	146	3	63.90
232	3	2	404	3	81.03
232	3	3	821	1	5.69
232	4	1	486	1	15.87
232	4	2	965	2	51.70
232	4	3	1021	12	239.47
232	4	4	951	2	49.06
232	4	5	933	12	302.17
232	4	6	579	2	15.36
233	1	1	681	2	56.24
233	1	2	346	3	21.42
233	1	3	316	2	57.56
233	1	4	223	1	13.04
233	2	1	273	3	17.79
233	2	2	591	3	38.10
233	2	3	670	2	33.82
233	2	4	277	1	26.37
233	3	1	873	12	227.15
233	4	1	415	12	124.41
233	4	2	861	3	41.55
233	4	3	272	3	28.80
233	4	4	353	3	74.37
233	4	5	516	12	119.57
234	1	1	997	1	9.03
234	2	1	175	3	76.80
234	2	2	362	12	252.12
234	2	3	69	2	33.22
234	2	4	847	3	56.37
235	1	1	371	12	170.39
235	1	2	350	12	153.89
235	2	1	729	2	13.42
235	2	2	798	2	58.96
235	2	3	441	2	11.74
235	2	4	665	2	13.44
235	3	1	655	12	234.52
235	3	2	320	12	230.12
235	4	1	202	1	10.08
235	4	2	373	12	281.27
235	4	3	845	1	16.18
235	4	4	254	3	67.65
235	4	5	357	3	70.92
236	1	1	52	1	26.73
236	1	2	84	12	295.24
236	1	3	384	1	21.07
236	2	1	699	2	39.42
236	2	2	239	3	72.57
236	2	3	160	2	59.84
236	2	4	940	3	36.54
236	2	5	648	3	49.02
236	2	6	752	3	25.77
236	3	1	10	1	17.82
236	3	2	858	2	10.66
236	3	3	90	3	20.31
236	3	4	419	12	121.44
236	3	5	854	12	272.25
236	3	6	100	3	70.38
236	4	1	328	12	215.49
236	5	1	199	12	223.08
236	5	2	158	1	13.39
236	6	1	464	12	165.77
236	6	2	23	1	26.56
236	6	3	403	2	49.96
236	6	4	47	3	72.12
236	6	5	61	1	16.98
236	6	6	703	2	53.54
237	1	1	870	2	45.78
237	1	2	984	1	6.17
237	1	3	237	12	280.28
237	1	4	629	2	48.38
237	1	5	560	3	68.73
238	1	1	397	3	34.14
238	2	1	1016	1	19.99
238	2	2	277	12	290.07
238	2	3	1023	3	46.47
238	2	4	730	1	24.66
238	2	5	800	3	54.48
238	2	6	632	1	16.48
239	1	1	114	2	44.64
239	1	2	805	12	314.16
239	1	3	790	12	136.73
240	1	1	959	3	30.21
240	1	2	919	3	40.05
240	1	3	603	3	17.34
240	2	1	986	2	13.12
240	2	2	711	2	56.14
240	2	3	446	1	21.44
240	3	1	698	1	15.50
240	4	1	429	12	181.72
240	4	2	1043	3	50.19
240	4	3	612	2	59.16
240	4	4	984	2	12.34
240	4	5	844	12	207.02
240	5	1	339	1	10.67
240	5	2	726	2	57.02
240	5	3	336	2	55.82
240	6	1	558	12	314.49
241	1	1	279	12	175.67
241	1	2	166	2	16.52
241	1	3	18	3	25.35
241	1	4	817	2	37.50
241	1	5	8	12	110.66
241	2	1	478	3	53.49
241	3	1	24	3	55.80
241	3	2	332	1	25.29
241	3	3	261	3	46.11
241	3	4	888	2	11.56
241	3	5	60	12	178.31
241	3	6	96	12	248.82
241	4	1	102	1	6.71
241	5	1	555	3	26.37
241	5	2	302	3	68.10
241	5	3	151	3	67.11
241	6	1	934	1	25.09
241	6	2	694	12	173.36
242	1	1	55	12	235.51
242	1	2	455	1	28.99
242	1	3	702	12	97.02
242	1	4	669	1	13.62
242	2	1	232	3	42.99
242	2	2	119	3	53.46
242	2	3	488	1	5.10
242	2	4	194	2	30.20
242	2	5	110	2	25.48
242	2	6	899	1	7.35
242	3	1	544	12	71.83
242	3	2	497	1	21.46
242	3	3	1039	3	79.38
242	3	4	363	2	29.44
242	3	5	247	1	25.40
242	4	1	431	1	17.86
242	4	2	675	3	65.79
242	5	1	279	1	15.97
242	5	2	77	12	97.24
242	5	3	834	3	67.68
242	6	1	324	2	40.74
242	6	2	801	1	5.17
242	6	3	691	3	87.00
243	1	1	300	1	26.38
243	1	2	242	3	84.96
243	2	1	899	2	14.70
243	2	2	527	1	27.40
244	1	1	606	12	278.41
244	1	2	13	2	35.20
244	2	1	125	3	26.46
244	2	2	265	12	184.03
244	2	3	826	2	41.80
244	2	4	171	1	27.30
244	2	5	190	3	60.84
244	2	6	528	1	9.84
244	3	1	442	2	27.34
244	3	2	181	2	10.94
244	4	1	395	1	16.99
244	4	2	260	3	22.02
244	4	3	19	1	29.22
244	4	4	999	1	17.61
244	4	5	39	1	7.38
244	5	1	67	3	83.40
244	5	2	786	2	41.68
244	5	3	202	12	110.88
245	1	1	366	3	72.36
245	1	2	815	12	70.84
245	1	3	743	12	87.78
245	1	4	976	12	114.51
245	2	1	261	1	15.37
245	2	2	435	12	130.35
245	2	3	910	12	311.19
245	3	1	353	12	272.69
245	4	1	622	1	17.45
245	4	2	33	3	56.25
245	5	1	195	2	45.06
245	5	2	431	1	17.86
245	5	3	481	1	5.82
245	5	4	945	3	47.67
245	6	1	676	3	37.05
245	6	2	261	12	169.07
245	6	3	976	12	114.51
245	6	4	61	12	186.78
246	1	1	355	1	20.33
246	1	2	222	2	48.40
246	1	3	624	2	44.50
246	1	4	340	2	28.58
246	1	5	1025	3	86.34
246	2	1	747	1	26.98
246	2	2	175	3	76.80
246	3	1	694	2	31.52
246	3	2	150	12	95.26
246	3	3	620	2	51.60
247	1	1	652	12	113.08
247	1	2	107	1	6.41
247	1	3	232	2	28.66
247	1	4	836	2	32.38
247	2	1	7	1	12.20
247	3	1	251	2	11.42
247	3	2	1017	3	42.90
248	1	1	838	3	57.90
248	1	2	877	2	38.30
248	1	3	995	2	28.16
249	1	1	587	3	31.14
249	1	2	914	1	5.23
249	1	3	946	1	9.51
249	1	4	518	12	168.85
249	1	5	899	3	22.05
249	2	1	504	3	83.16
249	2	2	632	12	181.28
249	2	3	681	1	28.12
249	2	4	752	1	8.59
249	2	5	361	12	140.36
249	3	1	397	1	11.38
249	4	1	798	3	88.44
249	4	2	960	12	234.41
249	5	1	940	1	12.18
249	6	1	735	12	243.21
249	6	2	735	12	243.21
250	1	1	969	1	13.93
250	1	2	690	2	40.14
250	1	3	551	1	23.73
250	1	4	269	3	46.08
250	1	5	459	2	23.92
250	2	1	1026	1	26.36
250	2	2	363	12	161.92
250	2	3	889	2	27.84
250	2	4	775	2	21.32
250	3	1	63	12	260.92
250	3	2	943	3	48.39
250	3	3	136	1	25.35
250	3	4	304	3	67.38
250	4	1	865	2	59.16
250	5	1	865	3	88.74
250	5	2	950	3	33.18
250	5	3	620	1	25.80
250	6	1	550	12	123.42
250	6	2	686	2	39.04
251	1	1	617	1	12.09
251	2	1	409	2	44.68
251	2	2	667	3	69.21
251	2	3	947	3	58.59
251	3	1	81	3	76.56
251	3	2	244	3	36.63
251	3	3	47	1	24.04
251	4	1	998	12	135.30
251	4	2	638	3	17.19
251	4	3	434	12	279.84
251	4	4	909	1	21.84
251	4	5	535	12	218.13
251	5	1	119	3	53.46
251	5	2	937	2	11.50
251	6	1	823	1	23.20
252	1	1	490	12	91.41
252	1	2	1035	1	20.89
252	2	1	117	1	24.93
252	2	2	191	2	34.58
252	2	3	999	3	52.83
252	2	4	874	12	213.40
252	2	5	402	12	209.88
252	2	6	599	2	11.14
252	3	1	920	2	40.56
252	3	2	529	12	320.10
252	3	3	582	2	41.76
252	3	4	797	2	36.50
252	4	1	800	2	36.32
252	4	2	926	3	73.80
252	5	1	188	2	15.42
253	1	1	996	2	38.24
253	1	2	366	1	24.12
253	1	3	564	12	207.79
253	1	4	194	12	166.10
253	1	5	424	2	17.16
253	1	6	667	12	253.77
253	2	1	1035	1	20.89
253	2	2	473	2	14.56
253	3	1	886	12	192.83
253	3	2	554	1	6.30
253	4	1	602	2	23.44
253	4	2	836	12	178.09
253	4	3	549	2	12.72
253	4	4	575	3	77.01
253	5	1	285	2	17.08
253	5	2	494	1	28.35
253	5	3	568	2	22.64
253	5	4	917	1	18.43
254	1	1	677	2	28.16
254	1	2	232	2	28.66
254	1	3	968	3	56.07
254	2	1	852	12	286.99
254	2	2	759	1	21.07
254	2	3	321	12	128.59
254	3	1	741	2	25.78
254	3	2	1001	3	86.28
254	3	3	875	3	40.08
254	3	4	156	1	7.49
254	3	5	53	1	26.62
254	3	6	655	1	21.32
254	4	1	953	12	219.23
254	4	2	656	1	25.43
254	4	3	611	2	42.64
255	1	1	934	2	50.18
255	1	2	24	2	37.20
255	2	1	543	1	28.06
255	2	2	887	3	45.30
255	3	1	275	3	65.07
255	3	2	875	2	26.72
255	3	3	545	3	51.30
255	3	4	264	1	12.84
255	4	1	182	12	81.73
255	4	2	632	12	181.28
255	4	3	7	1	12.20
255	4	4	984	12	67.87
255	4	5	746	1	11.12
255	4	6	193	1	24.18
255	5	1	754	12	275.44
255	5	2	130	12	173.36
255	5	3	201	12	191.18
255	5	4	119	12	196.02
255	6	1	716	12	152.35
255	6	2	384	3	63.21
256	1	1	599	2	11.14
256	1	2	727	1	8.25
256	2	1	724	12	226.93
256	2	2	1017	3	42.90
256	3	1	202	1	10.08
256	4	1	449	2	56.76
256	4	2	426	3	26.01
256	4	3	544	1	6.53
256	4	4	753	3	65.10
256	4	5	1024	12	218.35
256	5	1	927	1	15.13
256	5	2	299	2	26.46
256	5	3	141	2	44.94
256	5	4	327	2	50.40
256	5	5	462	1	23.78
257	1	1	107	1	6.41
257	1	2	1001	3	86.28
257	2	1	800	1	18.16
257	3	1	254	2	45.10
257	3	2	787	1	9.59
257	3	3	656	12	279.73
257	3	4	664	2	52.16
257	4	1	707	2	13.42
257	4	2	1008	2	57.30
257	4	3	911	2	11.42
257	5	1	314	12	274.12
257	5	2	58	3	79.80
257	5	3	139	12	170.94
257	5	4	653	12	317.57
257	6	1	514	1	16.18
257	6	2	329	1	14.98
257	6	3	613	2	43.24
257	6	4	562	2	23.30
257	6	5	681	12	309.32
257	6	6	91	1	11.58
258	1	1	1	3	63.54
258	1	2	993	12	268.07
258	2	1	565	12	264.22
258	2	2	58	3	79.80
258	2	3	407	2	37.96
258	2	4	252	12	282.15
258	2	5	1027	1	28.73
259	1	1	357	3	70.92
259	2	1	400	3	29.28
259	2	2	266	1	18.66
259	2	3	272	1	9.60
259	2	4	186	12	93.83
259	2	5	466	3	35.43
259	3	1	656	12	279.73
259	3	2	354	12	68.09
259	3	3	920	3	60.84
259	3	4	360	12	131.34
259	3	5	795	12	236.06
259	3	6	804	2	51.80
259	4	1	804	1	25.90
259	4	2	446	1	21.44
259	4	3	550	3	33.66
259	5	1	460	2	38.70
259	5	2	165	2	54.34
259	5	3	1026	2	52.72
259	6	1	832	12	97.68
259	6	2	204	3	72.84
259	6	3	483	12	132.66
260	1	1	397	12	125.18
260	1	2	532	3	83.64
260	1	3	266	12	205.26
260	1	4	912	3	68.82
260	2	1	499	2	14.24
260	2	2	410	2	35.08
260	2	3	560	1	22.91
260	2	4	979	2	56.78
260	3	1	919	3	40.05
260	4	1	1027	3	86.19
260	4	2	539	3	31.44
260	4	3	362	2	45.84
260	4	4	104	12	70.62
260	4	5	744	2	27.08
261	1	1	56	2	25.66
261	1	2	774	1	5.68
261	1	3	268	2	18.30
261	2	1	850	12	322.52
261	2	2	285	2	17.08
261	2	3	188	3	23.13
261	2	4	432	12	279.95
261	2	5	367	12	218.57
262	1	1	698	12	170.50
262	1	2	494	12	311.85
262	1	3	209	12	162.80
262	1	4	759	12	231.77
262	1	5	836	12	178.09
262	2	1	1029	2	30.38
262	2	2	216	12	253.55
262	2	3	856	1	5.29
262	2	4	287	2	24.30
262	2	5	550	12	123.42
263	1	1	527	12	301.40
263	1	2	306	2	52.94
263	1	3	692	1	17.90
263	1	4	12	3	82.26
263	2	1	515	2	50.18
263	2	2	789	3	42.93
263	2	3	699	1	19.71
263	2	4	129	2	11.50
263	2	5	943	2	32.26
263	2	6	858	2	10.66
263	3	1	1011	12	122.32
263	3	2	11	12	181.28
263	3	3	484	3	86.76
263	3	4	490	12	91.41
263	3	5	129	12	63.25
263	3	6	318	1	14.12
263	4	1	860	12	321.64
263	4	2	237	3	76.44
263	4	3	253	2	56.48
263	4	4	798	3	88.44
263	4	5	964	2	26.66
263	4	6	763	12	84.48
263	5	1	958	3	32.49
263	5	2	570	1	25.09
263	5	3	719	12	111.76
263	5	4	956	2	40.22
263	5	5	659	2	48.76
264	1	1	518	1	15.35
264	2	1	579	2	15.36
264	2	2	237	2	50.96
264	2	3	375	12	257.40
264	2	4	928	3	22.68
264	2	5	565	3	72.06
264	3	1	879	3	26.79
264	3	2	613	2	43.24
264	3	3	860	1	29.24
264	3	4	576	12	294.36
264	3	5	1002	2	47.94
264	3	6	752	1	8.59
265	1	1	1009	1	22.13
265	1	2	58	3	79.80
265	1	3	721	1	5.07
265	1	4	981	2	49.80
265	2	1	380	12	301.95
265	2	2	412	1	6.56
265	2	3	959	12	110.77
265	2	4	973	1	16.02
265	2	5	660	2	24.22
265	2	6	874	12	213.40
265	3	1	533	2	37.48
265	3	2	724	2	41.26
265	3	3	907	2	44.10
265	3	4	286	1	27.29
265	4	1	406	12	73.92
265	4	2	782	2	24.12
265	4	3	16	2	17.90
265	4	4	714	12	283.69
265	4	5	289	3	58.08
265	4	6	795	12	236.06
265	5	1	611	12	234.52
265	5	2	870	12	251.79
265	5	3	1030	3	63.63
265	5	4	701	1	24.32
266	1	1	1005	1	18.57
266	1	2	240	12	125.84
266	1	3	449	12	312.18
266	1	4	593	1	6.84
267	1	1	571	2	34.50
267	1	2	809	3	27.57
267	1	3	1018	2	18.44
267	1	4	578	1	7.32
267	1	5	300	3	79.14
267	1	6	97	3	36.33
267	2	1	473	1	7.28
267	2	2	895	3	75.18
268	1	1	756	3	88.56
268	1	2	78	1	15.50
268	2	1	334	12	156.75
268	2	2	541	2	35.24
268	2	3	400	3	29.28
268	2	4	713	3	75.18
268	3	1	871	12	112.42
269	1	1	889	1	13.92
269	1	2	520	3	30.03
269	1	3	465	1	8.00
269	1	4	1014	2	27.64
269	1	5	690	2	40.14
269	1	6	760	1	6.12
269	2	1	515	12	275.99
269	2	2	129	2	11.50
270	1	1	934	3	75.27
270	1	2	973	3	48.06
270	1	3	83	1	15.54
270	2	1	567	1	12.37
270	2	2	877	2	38.30
270	2	3	1013	1	10.04
270	2	4	995	12	154.88
270	2	5	263	2	26.24
270	2	6	240	2	22.88
271	1	1	153	3	69.33
271	1	2	856	1	5.29
271	1	3	92	1	29.26
271	1	4	989	12	132.99
271	1	5	403	12	274.78
272	1	1	933	1	27.47
272	1	2	766	1	29.86
272	2	1	31	2	33.48
272	2	2	517	3	22.26
272	2	3	321	3	35.07
272	3	1	895	2	50.12
272	3	2	155	1	24.83
272	3	3	460	12	212.85
272	4	1	103	2	24.74
272	5	1	677	1	14.08
272	5	2	484	1	28.92
272	5	3	701	1	24.32
272	5	4	125	1	8.82
272	5	5	609	12	247.61
272	6	1	701	1	24.32
272	6	2	941	1	16.57
272	6	3	224	12	263.12
273	1	1	869	3	53.79
273	1	2	753	1	21.70
273	2	1	137	2	25.70
273	3	1	504	2	55.44
273	3	2	652	12	113.08
273	3	3	763	2	15.36
273	3	4	263	1	13.12
274	1	1	17	3	75.69
274	1	2	242	2	56.64
274	1	3	784	2	17.74
274	1	4	575	1	25.67
274	1	5	699	2	39.42
274	2	1	676	2	24.70
274	2	2	480	1	28.89
274	2	3	537	3	78.90
274	2	4	799	2	27.74
274	2	5	160	1	29.92
274	3	1	818	1	6.57
274	3	2	220	2	23.48
274	3	3	201	12	191.18
274	3	4	713	2	50.12
274	3	5	286	3	81.87
274	4	1	55	3	64.23
274	4	2	590	12	168.63
274	4	3	991	3	79.68
274	4	4	59	12	316.91
274	4	5	64	12	289.63
274	4	6	390	2	16.86
275	1	1	222	12	266.20
275	1	2	577	3	63.18
275	2	1	878	1	22.43
275	2	2	440	3	16.05
275	2	3	122	1	22.13
275	2	4	741	12	141.79
275	2	5	204	3	72.84
275	2	6	281	3	16.89
276	1	1	591	2	25.40
276	2	1	409	3	67.02
276	2	2	167	3	63.15
276	3	1	21	12	210.54
276	3	2	45	3	23.13
276	3	3	317	12	63.69
276	4	1	1046	1	11.00
276	4	2	297	3	75.81
276	4	3	899	1	7.35
276	5	1	508	1	28.48
276	5	2	360	1	11.94
276	5	3	561	2	17.28
276	5	4	943	3	48.39
276	5	5	61	1	16.98
277	1	1	701	2	48.64
277	1	2	598	1	12.93
277	2	1	351	12	280.17
277	2	2	933	2	54.94
277	2	3	258	12	194.48
277	3	1	411	2	47.98
277	3	2	686	1	19.52
277	3	3	89	1	27.85
277	3	4	557	12	149.38
277	3	5	614	1	8.93
277	3	6	1003	12	107.47
278	1	1	871	1	10.22
278	1	2	253	12	310.64
278	1	3	1035	12	229.79
278	1	4	656	3	76.29
278	1	5	541	3	52.86
278	1	6	566	12	77.44
278	2	1	912	12	252.34
278	2	2	924	12	227.37
278	2	3	587	12	114.18
278	2	4	359	12	204.71
278	2	5	1039	2	52.92
278	2	6	373	12	281.27
278	3	1	936	3	61.02
278	3	2	200	2	23.76
278	3	3	523	2	53.56
278	3	4	657	3	52.29
278	4	1	616	3	37.17
278	4	2	159	12	162.36
278	5	1	372	1	10.21
278	5	2	563	2	53.64
279	1	1	181	3	16.41
279	1	2	418	3	30.99
279	1	3	22	1	8.05
279	2	1	596	12	203.17
279	2	2	622	3	52.35
279	2	3	243	12	212.85
279	2	4	53	2	53.24
279	2	5	332	2	50.58
279	3	1	101	1	8.73
279	3	2	994	2	30.88
279	4	1	701	1	24.32
279	4	2	417	1	13.92
279	4	3	47	12	264.44
279	4	4	977	2	29.02
279	5	1	1013	1	10.04
279	5	2	463	12	298.54
279	5	3	317	3	17.37
279	5	4	197	2	23.46
279	5	5	33	12	206.25
279	5	6	229	3	20.97
280	1	1	342	1	11.22
280	1	2	313	3	60.66
280	1	3	2	2	56.30
280	2	1	382	3	21.27
280	2	2	556	12	63.58
281	1	1	819	12	85.47
281	1	2	618	1	29.07
281	1	3	78	12	170.50
281	2	1	414	2	44.46
281	2	2	569	2	26.80
281	3	1	85	3	64.62
281	3	2	369	1	22.92
281	3	3	156	2	14.98
281	3	4	566	12	77.44
281	4	1	353	2	49.58
281	4	2	903	12	258.94
281	4	3	722	2	30.00
281	4	4	154	1	5.97
281	5	1	414	2	44.46
281	5	2	363	3	44.16
281	5	3	484	2	57.84
281	5	4	94	1	6.53
281	5	5	444	2	36.32
281	5	6	137	2	25.70
281	6	1	441	12	64.57
281	6	2	198	3	41.43
281	6	3	93	12	238.37
281	6	4	733	1	9.71
281	6	5	15	3	36.99
281	6	6	409	12	245.74
282	1	1	117	1	24.93
282	1	2	382	3	21.27
282	2	1	576	2	53.52
282	2	2	759	1	21.07
282	2	3	459	2	23.92
282	2	4	854	3	74.25
282	3	1	946	12	104.61
282	3	2	240	3	34.32
282	3	3	162	2	39.28
282	3	4	695	3	72.09
282	4	1	347	12	116.49
282	4	2	35	1	17.64
282	4	3	933	2	54.94
282	4	4	566	3	21.12
282	4	5	1045	1	18.76
282	5	1	545	1	17.10
282	5	2	873	3	61.95
282	6	1	507	3	54.06
282	6	2	247	12	279.40
282	6	3	299	2	26.46
282	6	4	910	1	28.29
283	1	1	205	1	12.99
283	1	2	795	2	42.92
283	1	3	802	12	134.64
283	1	4	221	2	24.46
283	1	5	428	3	18.72
283	2	1	586	3	53.10
283	2	2	747	12	296.78
283	2	3	586	12	194.70
283	3	1	258	12	194.48
283	3	2	202	1	10.08
283	3	3	266	12	205.26
283	3	4	962	2	31.44
283	3	5	414	2	44.46
283	3	6	179	1	5.38
283	4	1	140	1	5.95
283	4	2	842	3	85.59
283	4	3	1014	2	27.64
283	4	4	691	3	87.00
283	4	5	700	1	19.35
283	4	6	432	3	76.35
283	5	1	4	1	21.16
283	5	2	591	1	12.70
284	1	1	328	3	58.77
284	2	1	771	3	46.05
284	2	2	530	1	14.32
284	2	3	1011	2	22.24
284	2	4	392	3	39.51
284	2	5	77	3	26.52
284	2	6	532	1	27.88
285	1	1	21	12	210.54
285	1	2	953	3	59.79
285	1	3	860	12	321.64
285	1	4	127	1	17.86
285	2	1	32	12	141.68
285	2	2	772	3	39.87
285	2	3	864	2	46.96
286	1	1	279	3	47.91
286	1	2	19	1	29.22
286	1	3	674	3	44.61
286	1	4	52	3	80.19
286	1	5	761	1	18.88
286	1	6	138	1	21.51
287	1	1	778	1	8.86
287	1	2	932	12	231.77
287	1	3	104	2	12.84
287	1	4	527	3	82.20
288	1	1	194	2	30.20
288	2	1	211	12	193.38
288	3	1	978	2	20.10
288	3	2	831	2	47.22
288	3	3	871	3	30.66
288	4	1	167	1	21.05
288	4	2	42	1	21.28
288	4	3	599	3	16.71
288	5	1	31	2	33.48
288	5	2	893	1	24.44
288	5	3	750	12	72.27
288	5	4	484	3	86.76
288	5	5	230	2	22.48
289	1	1	859	3	30.78
289	1	2	264	12	141.24
290	1	1	1020	1	5.51
290	1	2	62	12	185.35
290	1	3	405	12	71.06
290	1	4	637	12	160.16
290	1	5	295	12	275.55
290	2	1	547	1	24.75
290	2	2	321	1	11.69
290	2	3	344	1	15.06
291	1	1	437	1	8.61
291	1	2	1040	1	28.12
291	1	3	468	12	314.60
291	1	4	889	2	27.84
291	1	5	49	3	30.54
291	1	6	455	2	57.98
291	2	1	54	1	20.31
291	2	2	65	1	9.98
291	2	3	608	2	58.96
291	2	4	438	2	21.34
291	2	5	621	12	324.06
291	3	1	296	2	19.90
291	3	2	1001	2	57.52
291	3	3	43	2	55.64
291	3	4	758	12	198.66
292	1	1	303	3	37.05
292	1	2	795	3	64.38
292	2	1	437	2	17.22
292	2	2	804	1	25.90
292	3	1	8	1	10.06
292	3	2	803	2	41.38
292	3	3	487	12	325.82
292	3	4	761	12	207.68
292	4	1	998	3	36.90
292	4	2	1047	3	41.55
292	4	3	968	2	37.38
292	4	4	51	3	72.27
292	4	5	518	3	46.05
293	1	1	1004	3	72.54
293	2	1	548	12	56.32
293	2	2	731	2	51.36
293	2	3	925	2	33.94
293	2	4	335	3	28.74
293	2	5	310	3	30.78
293	2	6	704	12	255.09
293	3	1	54	12	223.41
293	3	2	761	1	18.88
293	3	3	325	1	13.21
293	3	4	821	3	17.07
293	3	5	632	3	49.44
293	4	1	696	3	19.86
293	4	2	226	12	95.48
293	4	3	471	12	315.04
293	4	4	989	3	36.27
293	5	1	378	12	326.48
294	1	1	284	3	26.67
294	1	2	732	3	81.27
294	1	3	962	2	31.44
294	1	4	236	3	66.03
294	1	5	530	1	14.32
294	1	6	86	2	30.08
295	1	1	780	12	80.41
295	1	2	208	12	112.09
296	1	1	756	2	59.04
296	1	2	822	3	33.09
296	2	1	335	1	9.58
296	2	2	135	1	11.58
296	2	3	319	12	161.70
296	2	4	658	3	74.64
296	3	1	576	1	26.76
297	1	1	76	12	197.34
297	1	2	371	2	30.98
297	2	1	829	2	21.20
297	2	2	370	2	12.40
297	2	3	866	1	14.59
297	2	4	704	3	69.57
297	2	5	308	12	143.66
298	1	1	871	1	10.22
298	1	2	805	12	314.16
298	1	3	605	1	20.78
298	1	4	86	2	30.08
298	1	5	645	2	36.26
298	2	1	658	3	74.64
298	2	2	273	2	11.86
298	2	3	46	1	15.67
298	2	4	617	2	24.18
299	1	1	515	12	275.99
299	1	2	911	3	17.13
300	1	1	349	2	31.18
301	1	1	488	1	5.10
301	1	2	976	2	20.82
301	1	3	525	12	242.99
301	1	4	227	3	50.28
301	1	5	306	3	79.41
301	1	6	597	1	27.67
301	2	1	826	2	41.80
301	2	2	245	1	28.86
301	2	3	180	1	18.57
301	2	4	94	1	6.53
301	2	5	510	1	11.03
301	2	6	173	2	53.04
301	3	1	519	3	35.28
301	3	2	42	1	21.28
301	4	1	564	3	56.67
301	4	2	52	12	294.03
301	4	3	593	1	6.84
301	4	4	1011	3	33.36
301	5	1	124	3	82.26
301	5	2	763	2	15.36
301	5	3	926	1	24.60
301	5	4	6	12	222.75
301	5	5	301	12	325.38
301	5	6	203	12	163.57
302	1	1	741	12	141.79
303	1	1	662	2	46.58
303	1	2	168	2	48.74
303	1	3	951	1	24.53
303	1	4	610	12	65.56
304	1	1	281	2	11.26
304	1	2	603	12	63.58
304	1	3	525	2	44.18
304	2	1	558	2	57.18
304	2	2	406	1	6.72
304	2	3	612	3	88.74
304	2	4	975	3	27.60
304	2	5	345	12	90.53
304	3	1	445	2	42.86
304	3	2	109	12	176.00
304	3	3	379	3	25.17
304	3	4	294	1	7.91
304	3	5	218	12	99.66
304	4	1	966	3	31.35
304	4	2	32	12	141.68
304	5	1	572	2	54.98
304	5	2	485	12	251.46
304	5	3	877	3	57.45
304	5	4	1015	1	28.35
304	5	5	177	1	27.41
304	6	1	672	1	10.92
304	6	2	687	3	50.67
304	6	3	859	3	30.78
305	1	1	151	3	67.11
305	1	2	357	1	23.64
305	1	3	562	12	128.15
305	1	4	86	3	45.12
305	1	5	78	2	31.00
305	1	6	72	2	49.92
305	2	1	598	2	25.86
305	2	2	318	2	28.24
305	2	3	851	2	40.26
305	3	1	977	12	159.61
306	1	1	316	1	28.78
306	1	2	852	2	52.18
307	1	1	563	12	295.02
307	1	2	893	2	48.88
307	1	3	430	3	31.41
307	1	4	723	12	229.13
307	2	1	303	2	24.70
307	2	2	768	3	24.24
307	3	1	242	2	56.64
307	3	2	302	2	45.40
307	3	3	998	12	135.30
307	3	4	260	12	80.74
308	1	1	288	3	44.85
308	1	2	1045	2	37.52
308	1	3	1014	2	27.64
308	2	1	1043	12	184.03
308	2	2	499	2	14.24
309	1	1	735	2	44.22
309	1	2	491	1	7.33
309	1	3	966	12	114.95
309	1	4	329	3	44.94
309	1	5	916	12	228.03
309	1	6	595	2	57.90
309	2	1	1030	12	233.31
310	1	1	590	3	45.99
310	1	2	597	12	304.37
310	1	3	156	3	22.47
310	1	4	855	1	7.23
310	1	5	965	2	51.70
310	1	6	942	3	84.69
310	2	1	609	3	67.53
310	3	1	799	3	41.61
310	4	1	35	3	52.92
310	4	2	110	2	25.48
311	1	1	597	12	304.37
311	1	2	621	3	88.38
311	1	3	402	12	209.88
311	1	4	36	12	324.61
311	1	5	836	1	16.19
311	1	6	740	3	15.24
311	2	1	294	12	87.01
311	2	2	49	3	30.54
311	2	3	484	12	318.12
311	2	4	168	1	24.37
311	2	5	235	3	65.55
311	2	6	515	1	25.09
311	3	1	795	2	42.92
311	3	2	61	2	33.96
311	3	3	980	1	22.32
311	3	4	517	12	81.62
311	3	5	1044	3	57.36
311	3	6	281	12	61.93
312	1	1	38	3	17.25
312	1	2	367	1	19.87
312	1	3	851	12	221.43
312	1	4	954	3	16.53
312	1	5	624	3	66.75
312	1	6	572	2	54.98
313	1	1	51	1	24.09
313	1	2	813	1	9.68
313	1	3	282	12	306.13
313	1	4	857	12	86.24
313	1	5	598	3	38.79
313	1	6	897	3	68.97
313	2	1	610	1	5.96
313	2	2	335	12	105.38
313	2	3	208	12	112.09
313	2	4	787	1	9.59
313	2	5	801	1	5.17
313	3	1	141	12	247.17
313	4	1	96	1	22.62
313	5	1	222	1	24.20
313	5	2	436	2	11.98
313	5	3	956	2	40.22
313	5	4	153	3	69.33
313	6	1	194	3	45.30
313	6	2	495	1	27.04
313	6	3	39	2	14.76
313	6	4	317	1	5.79
313	6	5	657	2	34.86
314	1	1	271	2	13.78
314	1	2	861	3	41.55
314	1	3	621	2	58.92
314	1	4	332	3	75.87
314	1	5	234	1	26.40
314	1	6	690	1	20.07
314	2	1	299	1	13.23
314	2	2	440	3	16.05
314	2	3	52	12	294.03
314	2	4	958	12	119.13
314	2	5	748	12	250.36
314	3	1	814	1	13.52
314	4	1	228	2	43.86
314	4	2	217	1	6.83
314	5	1	147	2	39.16
314	5	2	157	12	264.66
314	5	3	523	12	294.58
314	6	1	971	12	217.91
314	6	2	910	2	56.58
314	6	3	244	12	134.31
314	6	4	555	2	17.58
314	6	5	112	3	23.85
315	1	1	621	12	324.06
315	2	1	68	3	64.98
315	2	2	223	12	143.44
315	3	1	925	3	50.91
315	3	2	104	3	19.26
315	3	3	810	2	55.42
315	4	1	789	12	157.41
315	4	2	268	12	100.65
315	4	3	275	3	65.07
315	4	4	1032	2	39.86
315	4	5	272	1	9.60
315	4	6	125	12	97.02
315	5	1	920	1	20.28
315	5	2	284	3	26.67
315	5	3	433	12	234.41
315	5	4	80	1	28.44
315	5	5	979	3	85.17
316	1	1	95	1	29.19
316	1	2	14	2	10.54
316	1	3	131	12	206.14
316	1	4	717	12	263.12
316	1	5	447	3	40.74
316	2	1	958	2	21.66
316	2	2	819	1	7.77
316	2	3	317	2	11.58
316	2	4	695	1	24.03
316	2	5	749	1	5.57
316	3	1	206	12	270.60
316	4	1	646	1	7.95
316	4	2	158	1	13.39
316	4	3	768	1	8.08
316	4	4	502	3	72.33
317	1	1	440	3	16.05
317	1	2	171	2	54.60
317	2	1	225	12	186.56
318	1	1	2	2	56.30
319	1	1	285	3	25.62
319	1	2	38	3	17.25
319	1	3	103	1	12.37
319	1	4	132	2	36.86
319	1	5	195	3	67.59
319	1	6	580	2	48.98
319	2	1	659	2	48.76
319	2	2	275	12	238.59
320	1	1	392	2	26.34
320	1	2	692	12	196.90
320	1	3	448	12	63.58
320	1	4	104	12	70.62
320	2	1	726	12	313.61
320	2	2	1043	12	184.03
320	2	3	481	2	11.64
320	2	4	226	2	17.36
320	2	5	1002	2	47.94
320	3	1	666	2	31.10
320	3	2	160	1	29.92
320	4	1	874	2	38.80
320	4	2	397	3	34.14
320	4	3	225	2	33.92
320	4	4	503	3	74.37
321	1	1	668	3	44.55
321	1	2	625	12	197.89
321	1	3	272	12	105.60
321	1	4	1015	2	56.70
321	1	5	50	1	15.62
321	1	6	251	2	11.42
321	2	1	170	3	29.25
321	2	2	982	12	315.37
321	2	3	415	3	33.93
321	3	1	19	1	29.22
321	3	2	258	2	35.36
321	3	3	35	1	17.64
321	4	1	268	2	18.30
321	4	2	367	3	59.61
321	4	3	962	3	47.16
321	5	1	74	1	12.56
321	5	2	93	3	65.01
321	5	3	559	1	6.67
321	5	4	800	12	199.76
321	5	5	403	12	274.78
322	1	1	492	2	44.20
322	1	2	598	1	12.93
322	2	1	773	3	76.20
322	2	2	778	12	97.46
322	2	3	678	2	52.90
322	2	4	390	12	92.73
322	2	5	130	1	15.76
322	3	1	9	3	73.41
322	3	2	168	2	48.74
322	3	3	983	3	78.33
322	3	4	523	2	53.56
322	3	5	852	3	78.27
322	3	6	480	1	28.89
322	4	1	243	1	19.35
322	4	2	593	2	13.68
322	4	3	356	3	67.29
322	4	4	288	2	29.90
322	4	5	862	1	6.71
322	5	1	683	1	9.19
322	5	2	818	2	13.14
322	5	3	837	1	8.82
322	5	4	738	2	48.44
322	6	1	719	12	111.76
322	6	2	865	1	29.58
322	6	3	682	1	27.34
323	1	1	841	3	76.14
323	2	1	63	1	23.72
323	2	2	686	3	58.56
323	2	3	278	1	24.56
323	2	4	14	3	15.81
323	2	5	877	12	210.65
323	2	6	587	12	114.18
323	3	1	79	1	11.97
323	3	2	495	12	297.44
323	4	1	546	2	59.76
323	4	2	838	1	19.30
323	4	3	81	1	25.52
323	4	4	342	1	11.22
323	4	5	838	1	19.30
324	1	1	333	1	16.64
324	1	2	966	2	20.90
324	1	3	559	12	73.37
324	1	4	72	12	274.56
324	1	5	558	1	28.59
324	1	6	375	12	257.40
325	1	1	558	3	85.77
325	1	2	566	3	21.12
325	1	3	1027	3	86.19
325	2	1	802	3	36.72
325	3	1	16	12	98.45
325	3	2	658	2	49.76
325	3	3	643	3	58.11
325	3	4	864	2	46.96
325	3	5	60	2	32.42
325	4	1	121	1	29.02
325	5	1	783	1	27.87
325	5	2	313	3	60.66
325	6	1	543	3	84.18
325	6	2	312	12	131.23
326	1	1	165	3	81.51
326	1	2	585	2	53.70
326	2	1	1029	2	30.38
326	2	2	2	2	56.30
326	2	3	885	3	27.66
326	2	4	624	1	22.25
326	2	5	224	1	23.92
326	3	1	933	1	27.47
326	3	2	525	1	22.09
326	4	1	1034	12	164.56
326	4	2	908	12	129.14
327	1	1	196	3	61.95
327	1	2	906	12	113.63
327	1	3	836	12	178.09
327	1	4	332	1	25.29
327	1	5	684	2	45.10
327	2	1	728	12	60.28
327	2	2	338	3	72.84
327	2	3	840	2	29.26
328	1	1	1016	3	59.97
328	2	1	616	1	12.39
328	2	2	1034	3	44.88
328	3	1	438	1	10.67
328	3	2	776	2	47.26
328	3	3	757	2	42.74
328	4	1	877	1	19.15
328	4	2	82	2	44.60
328	4	3	365	3	25.20
328	4	4	798	12	324.28
328	4	5	695	3	72.09
328	5	1	39	1	7.38
328	6	1	542	3	58.56
328	6	2	788	3	32.19
328	6	3	373	3	76.71
328	6	4	920	2	40.56
328	6	5	64	1	26.33
329	1	1	488	3	15.30
329	1	2	60	3	48.63
329	1	3	764	1	11.52
329	2	1	292	3	23.52
329	2	2	14	1	5.27
329	3	1	506	2	48.74
329	3	2	980	1	22.32
329	4	1	485	3	68.58
329	4	2	603	12	63.58
329	4	3	68	1	21.66
329	4	4	764	12	126.72
329	4	5	325	1	13.21
329	4	6	474	2	53.44
329	5	1	940	1	12.18
329	6	1	635	1	21.39
329	6	2	674	1	14.87
329	6	3	784	1	8.87
329	6	4	38	12	63.25
329	6	5	279	12	175.67
330	1	1	772	1	13.29
330	1	2	427	12	220.11
330	1	3	34	2	28.56
330	1	4	158	12	147.29
330	1	5	623	2	44.20
330	2	1	39	1	7.38
330	2	2	466	12	129.91
330	2	3	293	1	21.79
330	3	1	601	3	42.87
330	3	2	496	12	258.17
330	3	3	332	3	75.87
330	4	1	70	2	32.10
330	4	2	544	12	71.83
330	5	1	9	1	24.47
330	5	2	509	12	171.38
330	5	3	797	2	36.50
330	5	4	167	2	42.10
331	1	1	858	2	10.66
331	1	2	638	1	5.73
331	1	3	572	1	27.49
331	1	4	593	2	13.68
331	2	1	559	2	13.34
331	2	2	205	2	25.98
331	2	3	218	2	18.12
331	2	4	539	3	31.44
331	2	5	259	1	10.52
331	3	1	413	2	36.68
331	3	2	514	12	177.98
331	3	3	471	1	28.64
331	4	1	280	1	18.58
331	5	1	180	1	18.57
331	5	2	126	3	72.27
332	1	1	803	1	20.69
332	1	2	767	1	6.53
332	1	3	543	12	308.66
332	1	4	779	1	19.87
332	1	5	639	12	258.83
332	2	1	558	2	57.18
332	2	2	942	12	310.53
332	2	3	457	2	42.00
332	2	4	325	1	13.21
332	2	5	891	3	70.80
332	3	1	581	1	14.98
332	3	2	715	2	37.96
332	3	3	107	2	12.82
332	3	4	114	2	44.64
332	3	5	139	2	31.08
332	4	1	915	1	12.51
332	4	2	306	1	26.47
332	4	3	440	1	5.35
332	4	4	852	12	286.99
332	4	5	859	1	10.26
332	4	6	866	2	29.18
333	1	1	232	1	14.33
333	1	2	737	3	78.57
333	1	3	511	2	29.44
333	1	4	152	2	11.62
333	2	1	638	12	63.03
333	2	2	239	2	48.38
333	2	3	619	2	41.58
333	3	1	816	3	34.71
333	4	1	839	3	58.50
333	4	2	1044	3	57.36
333	4	3	65	1	9.98
333	4	4	983	3	78.33
334	1	1	947	2	39.06
334	2	1	362	2	45.84
334	2	2	117	12	274.23
334	2	3	651	3	89.76
334	2	4	839	2	39.00
334	2	5	780	3	21.93
334	3	1	510	12	121.33
334	3	2	642	3	41.73
334	3	3	134	3	67.35
334	3	4	51	1	24.09
334	4	1	42	1	21.28
334	4	2	814	3	40.56
334	4	3	807	3	29.61
334	4	4	5	3	20.01
334	4	5	132	2	36.86
335	1	1	715	2	37.96
335	1	2	779	1	19.87
335	1	3	396	12	131.45
335	1	4	296	2	19.90
335	2	1	279	12	175.67
335	2	2	754	12	275.44
335	2	3	899	1	7.35
335	2	4	1014	2	27.64
335	2	5	751	3	74.67
335	2	6	815	12	70.84
335	3	1	761	1	18.88
335	3	2	446	1	21.44
335	3	3	612	2	59.16
335	3	4	756	3	88.56
336	1	1	670	2	33.82
336	1	2	301	12	325.38
336	2	1	973	1	16.02
336	2	2	982	3	86.01
336	3	1	385	3	62.43
336	3	2	762	2	56.08
336	3	3	63	1	23.72
336	3	4	83	1	15.54
337	1	1	153	12	254.21
337	1	2	884	2	33.78
337	2	1	508	12	313.28
337	3	1	975	3	27.60
337	3	2	430	2	20.94
337	4	1	276	3	69.66
337	4	2	213	3	75.51
337	4	3	408	1	15.95
338	1	1	295	3	75.15
338	1	2	516	1	10.87
338	2	1	1036	3	35.22
338	2	2	336	1	27.91
338	3	1	1037	12	293.37
338	3	2	273	12	65.23
338	3	3	111	12	306.68
338	3	4	71	2	15.00
338	3	5	234	2	52.80
338	4	1	773	3	76.20
338	4	2	87	3	51.57
338	4	3	343	2	11.40
338	4	4	870	12	251.79
338	4	5	87	1	17.19
338	5	1	777	1	23.70
338	6	1	535	1	19.83
339	1	1	555	12	96.69
339	1	2	751	3	74.67
339	1	3	59	12	316.91
339	1	4	606	1	25.31
339	1	5	805	3	85.68
339	2	1	341	1	17.40
339	2	2	175	2	51.20
339	2	3	783	2	55.74
339	2	4	982	3	86.01
339	2	5	462	1	23.78
339	2	6	28	3	59.97
339	3	1	478	2	35.66
339	3	2	434	1	25.44
339	4	1	38	2	11.50
339	4	2	426	1	8.67
340	1	1	635	1	21.39
340	1	2	587	3	31.14
340	1	3	392	1	13.17
340	1	4	426	3	26.01
340	1	5	591	3	38.10
340	2	1	4	12	232.76
340	2	2	883	2	18.60
340	2	3	180	1	18.57
340	2	4	248	2	52.36
340	2	5	675	12	241.23
340	3	1	1037	3	80.01
340	3	2	440	2	10.70
340	4	1	698	1	15.50
340	4	2	599	3	16.71
340	4	3	998	2	24.60
340	4	4	110	1	12.74
340	4	5	480	2	57.78
340	5	1	78	2	31.00
340	5	2	561	1	8.64
341	1	1	143	1	12.49
341	1	2	389	2	35.04
341	1	3	627	1	6.58
341	1	4	289	1	19.36
341	1	5	153	2	46.22
342	1	1	569	2	26.80
342	2	1	1015	2	56.70
342	2	2	10	3	53.46
342	2	3	661	1	11.08
342	2	4	46	12	172.37
342	2	5	665	1	6.72
342	3	1	717	3	71.76
342	3	2	829	1	10.60
342	3	3	621	2	58.92
343	1	1	430	2	20.94
343	1	2	485	1	22.86
343	1	3	34	2	28.56
343	2	1	882	12	238.59
343	2	2	166	2	16.52
343	2	3	605	3	62.34
343	3	1	42	1	21.28
343	3	2	304	12	247.06
343	3	3	590	3	45.99
343	3	4	365	1	8.40
343	3	5	819	2	15.54
343	3	6	549	12	69.96
344	1	1	752	3	25.77
344	1	2	948	2	16.76
344	2	1	671	1	26.19
344	3	1	329	2	29.96
344	3	2	839	3	58.50
344	3	3	987	12	307.34
344	3	4	270	2	40.92
344	3	5	500	3	48.84
344	3	6	736	1	26.06
345	1	1	334	3	42.75
345	1	2	384	2	42.14
345	1	3	71	12	82.50
345	1	4	242	1	28.32
345	1	5	930	12	280.61
345	2	1	143	3	37.47
345	2	2	281	12	61.93
345	2	3	311	2	48.44
345	2	4	238	12	265.65
345	2	5	52	2	53.46
345	3	1	120	3	15.33
346	1	1	893	1	24.44
346	1	2	872	3	78.87
346	1	3	182	2	14.86
346	1	4	954	12	60.61
347	1	1	731	2	51.36
347	1	2	360	2	23.88
347	1	3	548	2	10.24
347	1	4	87	3	51.57
347	1	5	839	12	214.50
347	2	1	611	2	42.64
347	2	2	966	1	10.45
347	2	3	389	12	192.72
347	2	4	765	3	89.64
347	2	5	127	2	35.72
347	3	1	907	12	242.55
347	3	2	716	12	152.35
347	4	1	389	3	52.56
347	4	2	562	12	128.15
347	4	3	189	2	33.46
347	4	4	1008	3	85.95
347	4	5	383	3	56.67
347	5	1	845	2	32.36
347	5	2	592	3	45.96
347	5	3	402	1	19.08
348	1	1	757	1	21.37
348	1	2	320	3	62.76
348	1	3	372	12	112.31
348	1	4	667	12	253.77
348	1	5	133	2	35.08
348	2	1	750	12	72.27
348	2	2	582	2	41.76
348	2	3	266	12	205.26
348	2	4	5	1	6.67
348	2	5	560	3	68.73
348	2	6	148	12	145.64
348	3	1	1023	12	170.39
348	3	2	469	1	26.88
348	3	3	258	3	53.04
348	3	4	749	3	16.71
348	4	1	760	1	6.12
348	4	2	677	3	42.24
348	4	3	331	1	7.84
348	4	4	301	2	59.16
348	4	5	69	2	33.22
348	4	6	385	3	62.43
349	1	1	125	1	8.82
349	1	2	392	3	39.51
349	1	3	439	12	210.43
349	1	4	804	3	77.70
349	2	1	148	2	26.48
349	2	2	81	3	76.56
349	2	3	462	2	47.56
349	2	4	1011	1	11.12
349	2	5	904	3	55.89
349	3	1	510	1	11.03
350	1	1	558	1	28.59
350	2	1	814	12	148.72
350	2	2	299	3	39.69
350	3	1	369	2	45.84
350	3	2	323	1	13.60
350	4	1	466	12	129.91
350	5	1	988	3	33.51
350	5	2	630	3	84.00
351	1	1	404	12	297.11
351	1	2	777	3	71.10
351	1	3	384	2	42.14
351	1	4	594	2	12.16
352	1	1	689	3	30.09
352	1	2	520	3	30.03
352	2	1	307	2	10.56
352	2	2	383	12	207.79
352	2	3	713	2	50.12
352	2	4	660	2	24.22
352	2	5	686	1	19.52
352	3	1	153	2	46.22
352	3	2	791	12	57.09
352	3	3	946	1	9.51
352	3	4	421	1	14.30
353	1	1	611	3	63.96
353	1	2	888	12	63.58
353	2	1	686	1	19.52
353	2	2	582	1	20.88
353	2	3	270	12	225.06
353	2	4	1031	1	6.87
353	2	5	870	1	22.89
353	3	1	716	3	41.55
353	3	2	85	2	43.08
353	3	3	248	12	287.98
353	3	4	143	2	24.98
353	3	5	132	3	55.29
353	3	6	756	1	29.52
354	1	1	111	3	83.64
354	1	2	803	2	41.38
354	1	3	5	12	73.37
354	1	4	961	1	10.08
354	1	5	238	12	265.65
354	1	6	1006	12	221.76
355	1	1	1018	12	101.42
355	2	1	840	2	29.26
355	2	2	477	2	47.78
355	2	3	118	12	158.18
355	3	1	10	1	17.82
355	3	2	415	1	11.31
355	3	3	978	2	20.10
355	4	1	533	1	18.74
355	4	2	912	12	252.34
355	4	3	385	3	62.43
355	4	4	279	12	175.67
356	1	1	967	1	7.48
356	1	2	377	1	29.73
356	1	3	537	3	78.90
357	1	1	60	12	178.31
358	1	1	355	12	223.63
358	1	2	1025	3	86.34
358	1	3	280	3	55.74
358	1	4	985	1	20.34
359	1	1	447	3	40.74
359	1	2	401	3	50.25
359	1	3	730	2	49.32
359	1	4	494	12	311.85
359	1	5	382	1	7.09
359	1	6	359	12	204.71
359	2	1	923	2	29.28
359	3	1	622	12	191.95
359	4	1	194	12	166.10
359	4	2	210	2	24.14
359	4	3	212	1	29.80
359	4	4	500	12	179.08
359	4	5	769	12	63.14
359	4	6	509	2	31.16
359	5	1	803	1	20.69
359	5	2	435	2	23.70
359	6	1	692	2	35.80
359	6	2	207	12	273.57
359	6	3	656	2	50.86
360	1	1	129	3	17.25
361	1	1	575	1	25.67
361	1	2	63	12	260.92
361	1	3	1025	1	28.78
361	1	4	210	3	36.21
361	1	5	446	2	42.88
362	1	1	864	2	46.96
362	1	2	248	12	287.98
362	2	1	312	3	35.79
362	2	2	375	2	46.80
362	2	3	599	1	5.57
362	2	4	448	3	17.34
362	2	5	439	1	19.13
362	3	1	711	2	56.14
362	3	2	573	3	61.08
362	3	3	681	12	309.32
363	1	1	962	2	31.44
363	1	2	1027	2	57.46
363	1	3	905	1	11.44
363	1	4	833	3	58.89
363	2	1	519	2	23.52
363	3	1	57	3	52.80
363	3	2	655	12	234.52
363	3	3	290	2	17.82
363	3	4	514	1	16.18
363	3	5	732	2	54.18
364	1	1	751	3	74.67
364	1	2	848	12	191.07
364	1	3	134	1	22.45
364	1	4	504	12	304.92
364	1	5	958	3	32.49
364	1	6	477	3	71.67
364	2	1	907	3	66.15
364	2	2	110	2	25.48
364	2	3	219	3	19.35
364	3	1	432	3	76.35
364	4	1	1007	1	12.34
364	4	2	339	2	21.34
364	4	3	212	2	59.60
364	4	4	885	3	27.66
364	4	5	766	3	89.58
364	4	6	318	12	155.32
365	1	1	7	2	24.40
365	1	2	279	3	47.91
365	1	3	99	12	268.40
365	1	4	100	2	46.92
365	1	5	326	3	39.48
365	1	6	298	2	49.28
366	1	1	306	12	291.17
366	1	2	519	2	23.52
366	1	3	305	12	57.97
366	1	4	396	2	23.90
366	2	1	141	2	44.94
366	2	2	964	2	26.66
366	2	3	630	3	84.00
366	2	4	792	12	71.39
366	2	5	144	2	52.42
366	3	1	226	2	17.36
366	3	2	857	3	23.52
366	3	3	464	1	15.07
366	4	1	939	1	14.37
366	4	2	908	12	129.14
366	4	3	575	2	51.34
366	4	4	347	2	21.18
367	1	1	952	3	64.44
367	1	2	80	2	56.88
367	1	3	461	3	31.98
367	1	4	836	3	48.57
367	1	5	475	3	86.52
367	1	6	563	3	80.46
367	2	1	111	2	55.76
367	2	2	693	2	56.16
367	3	1	435	12	130.35
367	3	2	522	2	14.60
367	3	3	307	12	58.08
367	3	4	75	1	22.73
367	3	5	600	3	50.97
367	3	6	671	1	26.19
368	1	1	264	3	38.52
369	1	1	27	12	229.57
369	1	2	738	3	72.66
369	1	3	371	12	170.39
369	1	4	483	2	24.12
369	1	5	89	1	27.85
369	2	1	798	12	324.28
369	2	2	402	12	209.88
369	3	1	606	12	278.41
369	3	2	302	3	68.10
369	3	3	885	12	101.42
369	3	4	133	1	17.54
369	4	1	795	3	64.38
369	4	2	818	2	13.14
369	4	3	838	3	57.90
369	4	4	379	1	8.39
369	4	5	429	2	33.04
369	5	1	583	1	18.49
369	5	2	838	2	38.60
370	1	1	830	12	94.38
371	1	1	690	1	20.07
371	1	2	583	3	55.47
371	1	3	520	1	10.01
371	2	1	471	1	28.64
371	2	2	870	3	68.67
371	3	1	235	2	43.70
371	3	2	1046	2	22.00
371	3	3	552	3	15.90
371	3	4	806	1	8.34
371	4	1	911	12	62.81
371	4	2	121	1	29.02
371	4	3	237	3	76.44
371	4	4	141	1	22.47
371	4	5	77	3	26.52
371	5	1	589	1	21.56
371	5	2	602	1	11.72
371	6	1	174	2	51.36
371	6	2	1020	1	5.51
372	1	1	996	3	57.36
372	1	2	460	2	38.70
372	1	3	675	3	65.79
372	1	4	556	12	63.58
372	1	5	298	3	73.92
372	2	1	838	2	38.60
372	2	2	590	1	15.33
372	2	3	472	3	25.08
373	1	1	869	2	35.86
373	1	2	368	2	56.74
373	1	3	948	1	8.38
373	1	4	825	3	88.89
373	1	5	373	2	51.14
373	1	6	110	3	38.22
373	2	1	833	2	39.26
373	2	2	50	2	31.24
373	2	3	752	3	25.77
373	2	4	65	3	29.94
373	3	1	26	2	52.06
374	1	1	74	3	37.68
375	1	1	771	2	30.70
375	1	2	576	2	53.52
375	1	3	235	2	43.70
375	2	1	57	1	17.60
375	2	2	762	3	84.12
375	2	3	335	12	105.38
375	2	4	745	2	20.52
376	1	1	343	12	62.70
377	1	1	224	1	23.92
377	1	2	897	2	45.98
377	1	3	650	12	256.74
377	1	4	617	2	24.18
377	1	5	449	12	312.18
377	1	6	324	1	20.37
377	2	1	114	2	44.64
377	2	2	797	1	18.25
377	2	3	194	3	45.30
377	3	1	242	2	56.64
377	3	2	167	12	231.55
377	3	3	223	3	39.12
377	3	4	206	3	73.80
377	4	1	550	3	33.66
377	4	2	410	3	52.62
377	4	3	33	1	18.75
377	4	4	691	1	29.00
377	4	5	720	1	28.12
377	4	6	603	1	5.78
377	5	1	902	1	10.77
377	5	2	180	1	18.57
377	5	3	866	12	160.49
378	1	1	167	3	63.15
378	1	2	445	2	42.86
378	1	3	236	3	66.03
378	2	1	257	1	29.82
378	2	2	906	3	30.99
378	2	3	906	1	10.33
378	2	4	881	12	284.13
378	2	5	918	3	47.10
378	2	6	506	2	48.74
378	3	1	411	1	23.99
378	3	2	750	3	19.71
378	3	3	765	3	89.64
378	4	1	987	3	83.82
378	4	2	930	12	280.61
379	1	1	105	2	44.82
379	1	2	124	12	301.62
379	1	3	36	3	88.53
379	1	4	332	1	25.29
379	1	5	251	12	62.81
379	1	6	896	12	303.16
379	2	1	710	2	58.54
379	3	1	435	12	130.35
379	4	1	792	2	12.98
379	4	2	920	3	60.84
379	4	3	231	12	193.16
379	4	4	644	1	28.46
379	5	1	666	12	171.05
379	5	2	987	2	55.88
379	5	3	126	3	72.27
379	5	4	515	3	75.27
379	5	5	549	2	12.72
380	1	1	123	12	84.70
380	1	2	579	12	84.48
381	1	1	928	2	15.12
381	1	2	701	12	267.52
381	1	3	722	12	165.00
381	1	4	537	1	26.30
381	1	5	338	1	24.28
381	2	1	107	1	6.41
381	2	2	655	1	21.32
381	2	3	646	3	23.85
381	2	4	605	1	20.78
381	3	1	676	12	135.85
381	3	2	14	2	10.54
381	4	1	338	2	48.56
381	4	2	288	1	14.95
381	4	3	1000	2	13.82
382	1	1	652	2	20.56
382	1	2	709	12	73.81
382	1	3	730	12	271.26
383	1	1	1006	3	60.48
383	1	2	853	2	58.46
383	1	3	63	1	23.72
383	2	1	774	12	62.48
383	2	2	109	1	16.00
383	3	1	901	3	47.01
383	4	1	444	1	18.16
383	4	2	47	3	72.12
383	4	3	526	1	19.31
383	4	4	1024	12	218.35
383	4	5	679	12	129.91
383	4	6	146	2	42.60
384	1	1	982	3	86.01
384	2	1	287	12	133.65
384	2	2	116	2	23.74
384	2	3	1024	3	59.55
384	2	4	987	1	27.94
384	2	5	627	12	72.38
384	3	1	376	1	21.17
384	3	2	891	12	259.60
384	3	3	60	12	178.31
384	3	4	650	12	256.74
384	4	1	79	12	131.67
384	4	2	368	1	28.37
384	4	3	439	2	38.26
384	4	4	90	1	6.77
384	4	5	196	12	227.15
384	4	6	488	1	5.10
384	5	1	1014	3	41.46
384	5	2	37	3	42.45
385	1	1	2	1	28.15
385	1	2	190	1	20.28
385	1	3	448	2	11.56
385	1	4	146	2	42.60
385	1	5	691	12	319.00
385	2	1	967	2	14.96
385	2	2	512	3	82.47
385	2	3	442	1	13.67
385	2	4	57	3	52.80
385	3	1	840	3	43.89
385	3	2	880	1	26.62
385	3	3	116	12	130.57
385	3	4	704	12	255.09
385	3	5	926	3	73.80
385	4	1	155	1	24.83
385	4	2	496	3	70.41
385	4	3	210	3	36.21
385	4	4	287	1	12.15
385	4	5	112	12	87.45
385	4	6	884	12	185.79
386	1	1	629	12	266.09
386	1	2	344	1	15.06
386	1	3	546	12	328.68
386	2	1	1010	2	38.36
386	3	1	520	12	110.11
387	1	1	966	1	10.45
387	1	2	946	2	19.02
387	1	3	1025	1	28.78
387	1	4	1034	3	44.88
387	1	5	133	2	35.08
387	2	1	785	3	84.33
387	2	2	293	12	239.69
387	2	3	24	1	18.60
387	2	4	725	1	24.18
387	2	5	191	12	190.19
387	3	1	625	3	53.97
387	3	2	175	3	76.80
387	3	3	353	2	49.58
387	3	4	61	12	186.78
387	3	5	497	2	42.92
387	3	6	904	12	204.93
387	4	1	57	2	35.20
387	4	2	205	1	12.99
387	4	3	210	3	36.21
387	4	4	490	12	91.41
387	4	5	636	3	76.53
387	4	6	537	3	78.90
387	5	1	399	3	43.89
387	5	2	371	1	15.49
387	5	3	439	12	210.43
387	5	4	821	1	5.69
387	5	5	770	3	47.43
387	5	6	672	3	32.76
388	1	1	53	12	292.82
388	1	2	474	3	80.16
388	1	3	206	3	73.80
388	1	4	386	12	312.95
388	1	5	112	2	15.90
388	1	6	893	2	48.88
388	2	1	755	1	13.25
388	2	2	52	2	53.46
388	2	3	806	12	91.74
388	3	1	1047	2	27.70
388	3	2	478	1	17.83
388	3	3	46	12	172.37
388	3	4	199	1	20.28
388	3	5	238	1	24.15
388	4	1	922	12	253.00
388	4	2	1038	12	253.22
388	4	3	402	12	209.88
388	4	4	916	3	62.19
388	4	5	439	12	210.43
388	4	6	523	2	53.56
389	1	1	861	3	41.55
389	1	2	874	12	213.40
389	1	3	168	12	268.07
389	1	4	114	3	66.96
389	2	1	710	1	29.27
389	2	2	132	2	36.86
389	3	1	280	1	18.58
389	3	2	76	3	53.82
389	3	3	643	12	213.07
389	4	1	907	1	22.05
389	4	2	263	1	13.12
389	4	3	522	3	21.90
389	4	4	461	1	10.66
389	4	5	831	3	70.83
389	4	6	598	2	25.86
389	5	1	728	12	60.28
389	5	2	472	2	16.72
389	5	3	809	3	27.57
389	5	4	791	3	15.57
389	5	5	982	1	28.67
389	6	1	711	3	84.21
390	1	1	395	12	186.89
390	1	2	4	1	21.16
390	2	1	190	1	20.28
390	2	2	961	3	30.24
390	2	3	1017	12	157.30
390	2	4	173	1	26.52
390	2	5	191	2	34.58
391	1	1	688	1	15.77
391	1	2	303	3	37.05
391	1	3	194	2	30.20
391	1	4	341	1	17.40
391	1	5	116	2	23.74
391	1	6	757	2	42.74
391	2	1	670	3	50.73
391	2	2	114	12	245.52
391	2	3	214	12	307.45
391	2	4	435	2	23.70
391	2	5	35	12	194.04
391	2	6	360	12	131.34
391	3	1	816	12	127.27
391	3	2	742	2	45.58
391	3	3	184	12	298.21
391	4	1	787	12	105.49
391	4	2	429	12	181.72
391	4	3	762	12	308.44
391	4	4	328	12	215.49
391	5	1	934	3	75.27
392	1	1	368	3	85.11
392	1	2	470	3	28.32
392	1	3	368	12	312.07
392	1	4	349	1	15.59
392	2	1	268	2	18.30
392	2	2	849	2	57.92
392	2	3	257	12	328.02
392	2	4	343	12	62.70
392	2	5	401	12	184.25
392	3	1	226	1	8.68
392	3	2	954	3	16.53
393	1	1	150	3	25.98
394	1	1	625	1	17.99
394	1	2	973	1	16.02
394	1	3	321	12	128.59
394	1	4	729	1	6.71
394	2	1	174	12	282.48
394	2	2	959	3	30.21
394	2	3	638	1	5.73
394	3	1	674	2	29.74
394	3	2	607	1	22.19
394	3	3	560	3	68.73
394	3	4	427	12	220.11
394	3	5	432	1	25.45
394	3	6	925	3	50.91
395	1	1	92	2	58.52
395	1	2	1031	1	6.87
395	1	3	212	1	29.80
396	1	1	427	3	60.03
396	2	1	975	3	27.60
396	2	2	443	2	58.64
396	2	3	46	3	47.01
396	2	4	81	2	51.04
396	3	1	143	1	12.49
396	3	2	185	3	47.76
396	3	3	202	2	20.16
396	4	1	345	1	8.23
396	4	2	949	2	21.54
397	1	1	199	3	60.84
397	1	2	438	12	117.37
397	1	3	55	12	235.51
397	1	4	1009	1	22.13
397	2	1	603	2	11.56
397	2	2	127	12	196.46
397	2	3	637	2	29.12
397	3	1	313	12	222.42
397	3	2	631	2	10.00
397	3	3	410	1	17.54
397	3	4	330	2	41.20
397	3	5	847	2	37.58
397	3	6	545	12	188.10
397	4	1	978	3	30.15
397	4	2	611	3	63.96
397	4	3	976	2	20.82
397	4	4	425	12	174.35
397	4	5	429	12	181.72
397	5	1	29	1	20.97
397	5	2	240	12	125.84
397	5	3	342	3	33.66
397	5	4	639	1	23.53
397	5	5	561	3	25.92
398	1	1	932	1	21.07
398	1	2	70	12	176.55
398	1	3	442	1	13.67
398	1	4	337	12	183.92
398	2	1	763	3	23.04
398	3	1	209	2	29.60
398	3	2	25	2	29.82
398	3	3	283	3	86.49
398	3	4	1030	1	21.21
398	3	5	750	1	6.57
398	3	6	967	1	7.48
399	1	1	698	2	31.00
399	1	2	194	2	30.20
399	1	3	776	3	70.89
400	1	1	200	2	23.76
400	1	2	1006	1	20.16
400	2	1	815	3	19.32
400	2	2	593	3	20.52
400	2	3	980	1	22.32
400	2	4	256	2	56.02
400	3	1	1033	3	88.11
400	3	2	428	12	68.64
400	3	3	882	3	65.07
400	3	4	298	12	271.04
400	4	1	730	1	24.66
400	4	2	937	2	11.50
400	5	1	184	2	54.22
400	5	2	813	3	29.04
400	5	3	941	12	182.27
401	1	1	808	1	25.86
401	1	2	703	12	294.47
401	1	3	542	12	214.72
401	1	4	582	3	62.64
401	1	5	886	12	192.83
402	1	1	995	2	28.16
402	1	2	530	12	157.52
402	1	3	150	3	25.98
402	1	4	159	12	162.36
402	1	5	595	1	28.95
402	1	6	993	2	48.74
402	2	1	139	3	46.62
402	2	2	209	12	162.80
402	2	3	913	12	238.48
403	1	1	170	2	19.50
403	1	2	117	12	274.23
403	1	3	614	1	8.93
403	1	4	362	3	68.76
403	1	5	1008	3	85.95
403	1	6	392	3	39.51
404	1	1	353	12	272.69
404	1	2	214	2	55.90
404	2	1	732	1	27.09
404	2	2	464	2	30.14
405	1	1	733	2	19.42
405	2	1	729	1	6.71
405	3	1	173	1	26.52
405	4	1	503	12	272.69
406	1	1	588	3	34.38
406	1	2	775	2	21.32
406	1	3	655	1	21.32
406	1	4	761	3	56.64
406	2	1	347	12	116.49
406	2	2	703	2	53.54
406	2	3	498	3	49.44
406	2	4	914	2	10.46
406	2	5	402	2	38.16
406	2	6	150	2	17.32
406	3	1	649	2	34.18
406	3	2	1019	2	25.86
406	3	3	930	3	76.53
406	3	4	949	12	118.47
406	3	5	443	3	87.96
406	4	1	738	1	24.22
406	4	2	198	1	13.81
406	4	3	703	2	53.54
406	4	4	597	1	27.67
406	4	5	141	3	67.41
406	5	1	485	3	68.58
406	5	2	498	2	32.96
406	5	3	568	12	124.52
406	5	4	1044	12	210.32
406	5	5	237	12	280.28
407	1	1	211	2	35.16
408	1	1	79	2	23.94
408	1	2	1044	3	57.36
408	2	1	621	12	324.06
408	2	2	350	12	153.89
408	3	1	484	2	57.84
408	3	2	142	12	165.00
408	3	3	663	2	14.46
408	3	4	923	2	29.28
408	3	5	658	12	273.68
408	3	6	664	3	78.24
408	4	1	232	1	14.33
408	4	2	160	3	89.76
409	1	1	304	3	67.38
409	1	2	34	1	14.28
409	1	3	364	2	45.58
409	1	4	96	3	67.86
409	1	5	120	1	5.11
409	1	6	292	3	23.52
409	2	1	1039	2	52.92
409	2	2	406	3	20.16
409	3	1	633	2	32.36
409	3	2	947	3	58.59
409	4	1	182	12	81.73
409	4	2	258	12	194.48
409	4	3	842	1	28.53
409	4	4	785	1	28.11
409	4	5	438	3	32.01
409	5	1	222	3	72.60
409	5	2	672	12	120.12
409	5	3	996	3	57.36
409	6	1	929	12	207.90
409	6	2	823	1	23.20
409	6	3	881	3	77.49
409	6	4	330	3	61.80
409	6	5	711	3	84.21
410	1	1	266	3	55.98
410	1	2	875	1	13.36
410	2	1	904	12	204.93
410	2	2	279	3	47.91
410	2	3	721	2	10.14
411	1	1	1026	1	26.36
411	2	1	358	3	51.51
411	2	2	219	12	70.95
411	2	3	890	2	39.56
411	2	4	999	1	17.61
411	2	5	255	12	302.28
411	3	1	718	12	111.10
411	3	2	47	1	24.04
411	3	3	870	1	22.89
411	3	4	278	3	73.68
411	4	1	121	3	87.06
411	4	2	541	12	193.82
411	4	3	47	2	48.08
411	4	4	202	2	20.16
412	1	1	491	2	14.66
412	1	2	236	3	66.03
412	2	1	635	12	235.29
412	2	2	1043	2	33.46
412	2	3	1030	3	63.63
412	2	4	815	12	70.84
413	1	1	422	1	19.36
413	1	2	128	3	63.75
413	1	3	519	3	35.28
413	2	1	452	2	15.56
413	2	2	996	2	38.24
413	2	3	329	1	14.98
413	3	1	694	12	173.36
413	3	2	201	12	191.18
413	3	3	252	3	76.95
413	4	1	841	1	25.38
413	4	2	615	3	70.71
413	4	3	401	1	16.75
413	4	4	942	2	56.46
413	5	1	897	12	252.89
413	5	2	201	3	52.14
413	5	3	599	2	11.14
413	5	4	442	12	150.37
413	5	5	1024	2	39.70
414	1	1	965	2	51.70
414	1	2	583	12	203.39
414	2	1	1012	3	53.55
414	2	2	671	3	78.57
414	2	3	908	1	11.74
414	2	4	318	1	14.12
414	2	5	607	1	22.19
415	1	1	326	3	39.48
415	2	1	835	3	43.68
416	1	1	594	12	66.88
416	1	2	1004	3	72.54
416	1	3	22	12	88.55
417	1	1	632	2	32.96
417	1	2	891	3	70.80
417	1	3	704	12	255.09
417	1	4	691	2	58.00
417	2	1	933	3	82.41
417	2	2	953	3	59.79
417	2	3	634	3	23.61
417	3	1	766	3	89.58
417	4	1	519	1	11.76
417	4	2	9	1	24.47
417	4	3	273	1	5.93
417	5	1	819	12	85.47
417	6	1	100	12	258.06
417	6	2	562	2	23.30
417	6	3	75	1	22.73
417	6	4	278	12	270.16
417	6	5	492	12	243.10
417	6	6	29	2	41.94
418	1	1	351	12	280.17
418	1	2	811	2	11.50
418	1	3	709	1	6.71
418	1	4	782	3	36.18
418	1	5	434	1	25.44
418	1	6	499	3	21.36
418	2	1	515	12	275.99
418	3	1	554	1	6.30
418	4	1	912	1	22.94
418	4	2	412	1	6.56
418	4	3	759	12	231.77
418	4	4	176	3	62.22
418	4	5	879	2	17.86
418	4	6	793	1	16.37
418	5	1	785	1	28.11
418	5	2	1045	2	37.52
418	5	3	312	1	11.93
418	5	4	146	2	42.60
418	5	5	89	2	55.70
418	6	1	376	2	42.34
418	6	2	728	1	5.48
418	6	3	742	2	45.58
418	6	4	249	1	25.97
418	6	5	733	1	9.71
418	6	6	643	12	213.07
419	1	1	82	12	245.30
419	1	2	714	2	51.58
419	1	3	936	3	61.02
419	1	4	496	1	23.47
420	1	1	246	12	204.27
420	1	2	820	12	326.26
420	1	3	242	2	56.64
420	1	4	888	2	11.56
420	2	1	552	1	5.30
420	2	2	588	12	126.06
420	2	3	53	2	53.24
420	2	4	357	1	23.64
420	2	5	530	12	157.52
420	2	6	280	1	18.58
421	1	1	45	3	23.13
421	1	2	66	2	20.58
421	2	1	574	2	37.82
421	2	2	320	2	41.84
421	2	3	529	3	87.30
421	2	4	791	3	15.57
421	2	5	121	3	87.06
421	3	1	851	3	60.39
421	3	2	522	12	80.30
421	4	1	162	3	58.92
421	4	2	500	3	48.84
421	4	3	44	12	244.20
421	4	4	122	3	66.39
421	4	5	411	12	263.89
421	5	1	719	2	20.32
421	5	2	478	2	35.66
421	6	1	677	12	154.88
422	1	1	66	2	20.58
422	1	2	512	2	54.98
422	1	3	6	1	20.25
422	1	4	490	3	24.93
422	2	1	51	1	24.09
422	2	2	981	3	74.70
422	2	3	863	1	13.71
422	2	4	217	3	20.49
422	2	5	945	1	15.89
422	3	1	874	3	58.20
422	3	2	59	12	316.91
422	3	3	727	3	24.75
422	3	4	557	12	149.38
422	3	5	172	12	328.79
422	4	1	911	12	62.81
422	4	2	414	1	22.23
422	4	3	571	2	34.50
422	5	1	209	3	44.40
422	5	2	342	2	22.44
422	5	3	554	1	6.30
422	5	4	816	1	11.57
422	5	5	16	12	98.45
422	6	1	508	2	56.96
422	6	2	625	12	197.89
422	6	3	476	1	5.09
422	6	4	183	3	75.81
422	6	5	914	3	15.69
423	1	1	808	1	25.86
423	1	2	565	12	264.22
423	1	3	206	2	49.20
423	1	4	836	12	178.09
423	1	5	714	3	77.37
424	1	1	52	3	80.19
425	1	1	104	3	19.26
425	1	2	515	12	275.99
425	1	3	903	1	23.54
425	1	4	733	1	9.71
425	1	5	534	2	32.30
425	2	1	996	2	38.24
425	2	2	57	12	193.60
425	2	3	330	3	61.80
425	2	4	555	1	8.79
425	3	1	428	3	18.72
425	3	2	831	2	47.22
425	3	3	749	2	11.14
425	3	4	343	1	5.70
425	3	5	1036	2	23.48
425	3	6	435	1	11.85
425	4	1	752	12	94.49
426	1	1	904	2	37.26
426	1	2	622	3	52.35
426	2	1	528	12	108.24
426	2	2	769	12	63.14
426	2	3	728	1	5.48
426	2	4	917	2	36.86
426	2	5	818	12	72.27
426	3	1	657	12	191.73
426	3	2	48	2	58.12
426	3	3	257	3	89.46
426	3	4	36	2	59.02
426	3	5	525	2	44.18
426	3	6	186	3	25.59
426	4	1	770	12	173.91
426	4	2	849	3	86.88
426	4	3	912	12	252.34
426	5	1	665	2	13.44
426	5	2	353	12	272.69
426	5	3	958	2	21.66
426	5	4	361	2	25.52
426	5	5	697	3	66.27
426	5	6	108	2	32.42
427	1	1	314	2	49.84
427	1	2	192	2	44.74
427	2	1	145	1	26.47
427	2	2	974	1	6.37
427	2	3	54	2	40.62
427	2	4	379	2	16.78
427	3	1	302	1	22.70
427	3	2	18	1	8.45
427	3	3	568	2	22.64
427	3	4	299	3	39.69
428	1	1	57	1	17.60
428	2	1	330	2	41.20
428	2	2	776	3	70.89
428	3	1	629	1	24.19
428	3	2	684	1	22.55
428	3	3	2	1	28.15
428	3	4	413	3	55.02
428	3	5	1027	2	57.46
428	3	6	726	1	28.51
428	4	1	486	12	174.57
428	4	2	1019	1	12.93
428	4	3	469	1	26.88
428	4	4	494	2	56.70
428	4	5	540	12	295.35
429	1	1	972	12	266.31
429	2	1	30	3	24.48
429	2	2	856	3	15.87
430	1	1	337	2	33.44
430	1	2	717	12	263.12
430	1	3	858	1	5.33
431	1	1	238	2	48.30
431	1	2	792	3	19.47
431	1	3	634	12	86.57
431	1	4	253	2	56.48
431	1	5	511	2	29.44
431	2	1	306	3	79.41
431	2	2	348	1	18.36
431	2	3	534	2	32.30
431	2	4	958	12	119.13
431	3	1	430	3	31.41
431	3	2	473	1	7.28
431	3	3	371	3	46.47
431	3	4	455	12	318.89
431	3	5	1015	3	85.05
431	4	1	421	3	42.90
431	4	2	338	12	267.08
431	5	1	261	3	46.11
431	5	2	531	3	74.34
431	5	3	311	12	266.42
431	5	4	705	3	29.91
431	5	5	681	1	28.12
431	6	1	851	3	60.39
431	6	2	775	1	10.66
431	6	3	458	1	8.01
432	1	1	744	3	40.62
432	1	2	1040	2	56.24
432	1	3	868	12	128.70
433	1	1	333	1	16.64
433	1	2	983	1	26.11
433	1	3	904	3	55.89
434	1	1	520	1	10.01
434	1	2	148	1	13.24
434	1	3	729	1	6.71
434	1	4	602	3	35.16
434	2	1	564	2	37.78
434	2	2	223	3	39.12
434	2	3	391	2	58.10
434	2	4	558	1	28.59
434	2	5	716	12	152.35
434	2	6	826	1	20.90
434	3	1	548	2	10.24
434	3	2	683	1	9.19
434	3	3	930	3	76.53
434	3	4	745	12	112.86
435	1	1	696	12	72.82
435	1	2	223	3	39.12
435	1	3	573	1	20.36
435	1	4	525	3	66.27
435	1	5	803	2	41.38
435	2	1	328	2	39.18
435	2	2	131	1	18.74
435	3	1	166	2	16.52
435	3	2	137	12	141.35
435	3	3	161	2	37.26
436	1	1	310	12	112.86
436	1	2	488	12	56.10
436	1	3	169	3	29.25
436	1	4	284	2	17.78
436	2	1	569	2	26.80
436	2	2	905	12	125.84
436	2	3	427	3	60.03
436	2	4	1033	12	323.07
436	2	5	231	3	52.68
436	3	1	643	1	19.37
436	3	2	566	12	77.44
436	3	3	880	12	292.82
436	3	4	202	3	30.24
436	4	1	716	1	13.85
436	4	2	290	1	8.91
436	4	3	794	2	21.04
437	1	1	727	12	90.75
437	1	2	484	12	318.12
437	1	3	405	1	6.46
437	1	4	703	1	26.77
437	1	5	448	2	11.56
437	1	6	338	1	24.28
437	2	1	989	1	12.09
437	3	1	180	2	37.14
437	4	1	383	3	56.67
437	4	2	64	3	78.99
437	4	3	17	1	25.23
437	4	4	567	1	12.37
437	5	1	615	1	23.57
437	5	2	824	2	57.12
437	5	3	211	12	193.38
437	5	4	556	2	11.56
437	5	5	58	2	53.20
437	6	1	377	3	89.19
437	6	2	1046	2	22.00
438	1	1	387	2	30.86
438	1	2	444	2	36.32
438	1	3	235	12	240.35
438	1	4	653	1	28.87
438	1	5	414	12	244.53
438	1	6	70	12	176.55
438	2	1	270	12	225.06
438	2	2	479	3	35.10
438	2	3	430	12	115.17
438	3	1	661	2	22.16
438	4	1	976	3	31.23
438	4	2	426	1	8.67
439	1	1	996	3	57.36
439	1	2	503	1	24.79
439	1	3	172	3	89.67
439	2	1	880	1	26.62
439	2	2	879	3	26.79
439	2	3	878	1	22.43
439	2	4	665	2	13.44
440	1	1	283	3	86.49
440	1	2	134	12	246.95
440	2	1	407	2	37.96
440	3	1	941	3	49.71
441	1	1	457	2	42.00
441	1	2	862	3	20.13
441	2	1	557	3	40.74
441	2	2	212	3	89.40
441	2	3	637	2	29.12
441	2	4	951	2	49.06
441	2	5	454	1	28.31
441	3	1	726	3	85.53
442	1	1	556	2	11.56
442	1	2	788	2	21.46
442	1	3	914	2	10.46
442	1	4	535	2	39.66
442	2	1	464	12	165.77
442	2	2	528	12	108.24
442	2	3	477	3	71.67
442	2	4	596	1	18.47
442	2	5	886	3	52.59
443	1	1	750	1	6.57
443	1	2	176	1	20.74
443	2	1	514	1	16.18
443	2	2	368	2	56.74
443	2	3	695	1	24.03
443	2	4	910	1	28.29
443	2	5	834	12	248.16
443	2	6	22	2	16.10
443	3	1	282	3	83.49
443	3	2	851	2	40.26
443	3	3	614	2	17.86
443	3	4	101	3	26.19
443	3	5	879	1	8.93
443	4	1	341	12	191.40
443	4	2	935	1	28.56
443	4	3	891	12	259.60
443	4	4	641	12	214.17
443	4	5	976	2	20.82
443	5	1	639	2	47.06
443	5	2	829	3	31.80
443	5	3	9	3	73.41
443	6	1	704	12	255.09
443	6	2	525	3	66.27
443	6	3	964	2	26.66
443	6	4	498	3	49.44
444	1	1	249	1	25.97
444	1	2	99	3	73.20
444	1	3	878	12	246.73
445	1	1	780	1	7.31
445	1	2	124	1	27.42
445	2	1	615	3	70.71
445	2	2	4	12	232.76
445	2	3	259	2	21.04
445	2	4	182	2	14.86
445	2	5	610	12	65.56
445	3	1	226	12	95.48
445	3	2	793	12	180.07
445	3	3	50	12	171.82
445	4	1	879	1	8.93
445	4	2	158	12	147.29
445	4	3	2	2	56.30
445	5	1	692	2	35.80
445	5	2	352	12	107.25
446	1	1	617	2	24.18
446	1	2	532	3	83.64
446	1	3	919	1	13.35
446	1	4	599	12	61.27
446	1	5	162	2	39.28
446	2	1	65	12	109.78
446	2	2	877	2	38.30
446	3	1	136	2	50.70
446	3	2	55	3	64.23
446	3	3	871	3	30.66
446	4	1	621	2	58.92
446	4	2	185	3	47.76
446	4	3	534	12	177.65
446	5	1	403	12	274.78
446	5	2	881	2	51.66
446	5	3	236	12	242.11
446	6	1	420	1	28.55
446	6	2	998	1	12.30
447	1	1	490	2	16.62
447	2	1	243	3	58.05
447	3	1	937	1	5.75
447	3	2	102	12	73.81
447	3	3	175	3	76.80
447	3	4	408	3	47.85
447	3	5	484	3	86.76
447	3	6	471	1	28.64
447	4	1	348	1	18.36
447	4	2	95	3	87.57
447	4	3	741	1	12.89
447	4	4	586	3	53.10
447	4	5	4	1	21.16
447	4	6	176	12	228.14
448	1	1	200	12	130.68
448	1	2	405	2	12.92
448	1	3	734	2	36.62
448	1	4	718	2	20.20
448	1	5	769	3	17.22
448	1	6	306	1	26.47
449	1	1	352	1	9.75
449	1	2	557	2	27.16
449	1	3	341	3	52.20
449	1	4	279	3	47.91
449	2	1	168	2	48.74
449	2	2	446	3	64.32
449	2	3	69	12	182.71
449	2	4	835	1	14.56
449	2	5	717	1	23.92
449	3	1	161	12	204.93
449	3	2	690	2	40.14
449	4	1	969	12	153.23
449	4	2	1033	1	29.37
449	5	1	67	3	83.40
449	5	2	155	1	24.83
449	5	3	345	1	8.23
450	1	1	999	1	17.61
450	2	1	446	12	235.84
451	1	1	748	1	22.76
451	1	2	305	12	57.97
451	1	3	254	12	248.05
451	1	4	186	3	25.59
451	1	5	570	1	25.09
451	1	6	476	3	15.27
451	2	1	74	2	25.12
451	2	2	359	12	204.71
451	2	3	834	2	45.12
451	2	4	172	3	89.67
451	2	5	791	3	15.57
451	2	6	475	3	86.52
451	3	1	348	3	55.08
451	3	2	663	12	79.53
451	3	3	97	12	133.21
452	1	1	655	3	63.96
452	1	2	1028	12	152.90
452	2	1	530	2	28.64
453	1	1	56	1	12.83
453	1	2	645	2	36.26
453	1	3	500	1	16.28
453	2	1	382	2	14.18
453	2	2	39	1	7.38
453	2	3	1044	1	19.12
454	1	1	373	2	51.14
454	1	2	288	3	44.85
454	1	3	479	2	23.40
454	1	4	332	12	278.19
454	1	5	902	3	32.31
454	1	6	260	3	22.02
454	2	1	244	2	24.42
454	2	2	503	2	49.58
454	2	3	15	2	24.66
454	3	1	905	12	125.84
454	4	1	541	12	193.82
454	4	2	45	1	7.71
454	4	3	891	1	23.60
454	4	4	465	1	8.00
455	1	1	750	2	13.14
455	1	2	512	1	27.49
455	1	3	13	3	52.80
455	1	4	41	12	186.89
455	1	5	224	2	47.84
455	2	1	905	2	22.88
455	2	2	300	12	290.18
455	2	3	873	3	61.95
455	3	1	325	3	39.63
455	4	1	537	1	26.30
455	4	2	868	12	128.70
455	4	3	836	2	32.38
456	1	1	675	12	241.23
456	2	1	326	1	13.16
456	2	2	268	1	9.15
456	2	3	661	3	33.24
456	3	1	357	2	47.28
456	3	2	841	1	25.38
456	3	3	873	1	20.65
456	3	4	350	1	13.99
456	3	5	308	12	143.66
456	3	6	183	12	277.97
456	4	1	614	12	98.23
456	4	2	958	1	10.83
456	4	3	397	12	125.18
456	4	4	326	2	26.32
456	4	5	917	1	18.43
457	1	1	105	3	67.23
457	1	2	1011	1	11.12
457	1	3	939	12	158.07
457	1	4	925	3	50.91
457	1	5	42	2	42.56
457	2	1	852	12	286.99
457	2	2	164	2	18.80
457	2	3	870	2	45.78
457	2	4	545	3	51.30
457	2	5	1008	1	28.65
457	2	6	113	3	57.75
458	1	1	824	12	314.16
458	1	2	745	1	10.26
458	1	3	992	2	21.02
458	1	4	439	3	57.39
458	1	5	790	3	37.29
458	2	1	360	12	131.34
458	2	2	971	3	59.43
458	2	3	25	3	44.73
458	2	4	198	12	151.91
458	2	5	505	3	82.89
458	2	6	571	2	34.50
458	3	1	257	12	328.02
458	3	2	299	12	145.53
458	3	3	356	3	67.29
458	3	4	593	12	75.24
458	3	5	296	2	19.90
458	4	1	867	2	31.36
458	5	1	906	3	30.99
458	5	2	663	1	7.23
458	5	3	370	12	68.20
458	5	4	1045	3	56.28
458	5	5	918	2	31.40
459	1	1	548	1	5.12
459	1	2	854	3	74.25
459	1	3	913	2	43.36
459	1	4	644	1	28.46
459	2	1	465	12	88.00
459	2	2	157	2	48.12
459	2	3	1000	12	76.01
459	2	4	206	2	49.20
459	2	5	137	3	38.55
459	3	1	703	12	294.47
459	3	2	79	3	35.91
459	3	3	105	3	67.23
460	1	1	53	2	53.24
460	1	2	496	2	46.94
460	2	1	151	3	67.11
460	3	1	1029	3	45.57
460	3	2	72	3	74.88
460	3	3	21	12	210.54
460	4	1	359	12	204.71
460	4	2	154	3	17.91
460	4	3	480	3	86.67
460	4	4	1039	3	79.38
460	4	5	721	1	5.07
460	4	6	352	3	29.25
460	5	1	241	2	33.90
460	5	2	29	1	20.97
461	1	1	9	1	24.47
461	1	2	1046	3	33.00
461	1	3	564	3	56.67
461	1	4	630	1	28.00
461	1	5	378	3	89.04
461	1	6	562	12	128.15
461	2	1	407	2	37.96
461	2	2	356	3	67.29
462	1	1	112	3	23.85
462	1	2	475	3	86.52
462	1	3	120	1	5.11
462	2	1	446	2	42.88
462	2	2	711	3	84.21
462	2	3	382	12	77.99
462	2	4	1018	12	101.42
462	2	5	346	3	21.42
462	3	1	53	1	26.62
462	3	2	855	3	21.69
462	4	1	101	3	26.19
462	4	2	22	3	24.15
463	1	1	714	2	51.58
463	2	1	110	1	12.74
463	2	2	163	1	20.38
463	2	3	142	12	165.00
463	2	4	358	2	34.34
463	3	1	409	1	22.34
463	3	2	428	1	6.24
463	4	1	979	3	85.17
463	4	2	457	2	42.00
463	4	3	381	3	62.58
463	4	4	555	3	26.37
463	5	1	545	3	51.30
463	5	2	323	3	40.80
463	5	3	915	3	37.53
463	5	4	645	2	36.26
464	1	1	392	3	39.51
464	1	2	686	3	58.56
464	2	1	782	12	132.66
464	2	2	72	2	49.92
464	2	3	479	3	35.10
464	2	4	702	12	97.02
464	2	5	600	2	33.98
464	2	6	616	12	136.29
464	3	1	249	2	51.94
464	3	2	721	12	55.77
464	3	3	24	3	55.80
464	3	4	90	1	6.77
464	3	5	326	12	144.76
464	3	6	149	1	14.66
464	4	1	532	3	83.64
464	4	2	31	1	16.74
464	4	3	395	1	16.99
464	4	4	494	12	311.85
464	5	1	648	1	16.34
464	5	2	644	3	85.38
464	5	3	454	12	311.41
464	5	4	618	3	87.21
464	5	5	114	1	22.32
464	5	6	683	2	18.38
465	1	1	220	1	11.74
465	1	2	270	1	20.46
465	1	3	279	3	47.91
465	1	4	977	2	29.02
465	1	5	497	1	21.46
465	2	1	404	2	54.02
465	2	2	1022	3	18.75
465	2	3	978	2	20.10
465	2	4	855	1	7.23
465	2	5	675	3	65.79
465	2	6	851	2	40.26
466	1	1	749	2	11.14
466	1	2	80	12	312.84
466	1	3	749	1	5.57
466	1	4	87	3	51.57
466	2	1	6	12	222.75
466	2	2	805	1	28.56
466	2	3	496	2	46.94
466	2	4	981	12	273.90
466	2	5	53	3	79.86
466	2	6	668	3	44.55
466	3	1	986	12	72.16
466	4	1	929	2	37.80
466	4	2	522	12	80.30
466	4	3	266	2	37.32
466	4	4	16	3	26.85
467	1	1	769	1	5.74
467	1	2	905	12	125.84
467	1	3	193	12	265.98
467	1	4	847	12	206.69
467	1	5	172	12	328.79
467	1	6	362	2	45.84
467	2	1	997	3	27.09
467	2	2	457	3	63.00
467	2	3	412	2	13.12
467	3	1	839	3	58.50
467	3	2	282	12	306.13
467	3	3	558	1	28.59
467	4	1	1009	1	22.13
467	4	2	248	2	52.36
467	4	3	526	1	19.31
467	4	4	39	12	81.18
467	5	1	263	12	144.32
467	5	2	303	1	12.35
467	5	3	559	3	20.01
467	5	4	37	1	14.15
467	5	5	663	12	79.53
467	5	6	798	3	88.44
468	1	1	705	1	9.97
468	2	1	127	1	17.86
468	2	2	478	1	17.83
469	1	1	430	1	10.47
469	1	2	750	1	6.57
469	1	3	271	2	13.78
469	2	1	744	3	40.62
469	2	2	993	3	73.11
469	2	3	1007	1	12.34
469	3	1	580	3	73.47
469	3	2	48	1	29.06
469	3	3	159	2	29.52
470	1	1	882	1	21.69
470	1	2	476	1	5.09
470	1	3	890	1	19.78
470	1	4	130	2	31.52
470	1	5	114	2	44.64
470	2	1	346	3	21.42
470	2	2	528	3	29.52
470	2	3	845	12	177.98
470	3	1	857	1	7.84
470	3	2	682	2	54.68
470	3	3	365	2	16.80
470	3	4	440	2	10.70
471	1	1	660	12	133.21
471	1	2	898	2	44.04
471	1	3	271	2	13.78
471	1	4	95	1	29.19
471	2	1	620	2	51.60
471	2	2	906	12	113.63
471	3	1	616	3	37.17
471	3	2	582	12	229.68
471	3	3	90	1	6.77
471	3	4	672	12	120.12
471	3	5	490	12	91.41
471	3	6	325	3	39.63
471	4	1	480	3	86.67
471	5	1	38	3	17.25
471	5	2	351	3	76.41
471	5	3	207	12	273.57
471	6	1	559	1	6.67
472	1	1	206	12	270.60
472	1	2	101	3	26.19
472	1	3	62	12	185.35
472	1	4	600	2	33.98
472	1	5	856	1	5.29
473	1	1	959	2	20.14
473	2	1	878	2	44.86
473	2	2	786	3	62.52
473	2	3	552	3	15.90
473	3	1	55	12	235.51
473	3	2	779	2	39.74
473	4	1	975	3	27.60
473	4	2	75	1	22.73
473	4	3	37	12	155.65
473	4	4	718	12	111.10
473	4	5	232	2	28.66
473	5	1	900	3	50.82
473	5	2	1029	12	167.09
473	5	3	686	3	58.56
473	5	4	389	2	35.04
473	5	5	435	3	35.55
474	1	1	142	1	15.00
474	1	2	57	1	17.60
474	1	3	492	2	44.20
474	2	1	96	12	248.82
474	2	2	358	1	17.17
474	2	3	572	1	27.49
474	3	1	734	12	201.41
474	3	2	612	1	29.58
474	3	3	192	3	67.11
474	4	1	232	12	157.63
474	4	2	233	2	11.30
474	4	3	272	2	19.20
474	4	4	267	3	25.89
474	4	5	332	12	278.19
474	4	6	697	3	66.27
475	1	1	683	12	101.09
475	1	2	863	2	27.42
475	1	3	75	1	22.73
475	1	4	101	12	96.03
475	2	1	496	2	46.94
475	2	2	653	1	28.87
475	2	3	718	2	20.20
475	3	1	29	3	62.91
475	3	2	366	3	72.36
475	3	3	533	1	18.74
475	3	4	50	3	46.86
475	3	5	243	1	19.35
475	4	1	406	1	6.72
475	4	2	710	1	29.27
476	1	1	504	3	83.16
476	1	2	756	1	29.52
476	1	3	936	2	40.68
476	1	4	765	1	29.88
476	1	5	469	2	53.76
476	2	1	755	2	26.50
476	2	2	751	12	273.79
477	1	1	912	12	252.34
477	1	2	91	2	23.16
477	1	3	509	2	31.16
477	1	4	320	12	230.12
477	2	1	1021	12	239.47
477	3	1	780	3	21.93
477	3	2	698	12	170.50
477	4	1	1032	12	219.23
477	4	2	271	12	75.79
477	4	3	983	3	78.33
477	4	4	210	2	24.14
477	4	5	306	1	26.47
477	4	6	379	2	16.78
477	5	1	121	2	58.04
477	5	2	1019	12	142.23
477	5	3	243	3	58.05
477	6	1	242	2	56.64
478	1	1	602	2	23.44
478	1	2	248	1	26.18
478	1	3	66	1	10.29
478	1	4	763	2	15.36
478	1	5	142	1	15.00
478	2	1	471	2	57.28
478	2	2	341	2	34.80
478	3	1	44	3	66.60
478	3	2	19	3	87.66
478	4	1	429	3	49.56
478	4	2	722	2	30.00
478	5	1	227	3	50.28
478	5	2	844	12	207.02
478	6	1	820	2	59.32
478	6	2	841	1	25.38
478	6	3	84	3	80.52
478	6	4	325	3	39.63
478	6	5	44	3	66.60
478	6	6	115	3	42.06
479	1	1	564	3	56.67
480	1	1	998	12	135.30
480	1	2	170	12	107.25
480	1	3	1014	1	13.82
480	1	4	500	2	32.56
480	1	5	846	3	19.95
480	1	6	1045	12	206.36
480	2	1	898	3	66.06
480	3	1	189	1	16.73
480	4	1	379	1	8.39
480	4	2	199	12	223.08
480	4	3	216	1	23.05
480	5	1	994	2	30.88
480	5	2	364	2	45.58
480	5	3	843	2	16.22
480	5	4	675	3	65.79
480	5	5	518	12	168.85
481	1	1	211	12	193.38
481	1	2	196	1	20.65
481	1	3	43	2	55.64
481	1	4	221	2	24.46
481	1	5	265	1	16.73
481	1	6	398	12	320.54
481	2	1	466	3	35.43
481	2	2	22	2	16.10
481	2	3	414	2	44.46
481	2	4	232	1	14.33
481	3	1	360	3	35.82
481	3	2	611	3	63.96
481	4	1	926	3	73.80
481	4	2	398	12	320.54
481	4	3	106	12	287.10
481	4	4	557	1	13.58
481	5	1	758	12	198.66
481	5	2	665	2	13.44
481	5	3	128	12	233.75
482	1	1	529	3	87.30
482	1	2	139	1	15.54
482	2	1	473	12	80.08
482	2	2	165	2	54.34
482	2	3	671	3	78.57
482	2	4	196	2	41.30
482	2	5	172	2	59.78
482	2	6	620	1	25.80
482	3	1	88	2	29.18
482	4	1	771	12	168.85
482	4	2	101	1	8.73
482	5	1	992	2	21.02
482	5	2	478	1	17.83
482	5	3	626	12	91.08
482	5	4	798	3	88.44
482	5	5	615	3	70.71
482	5	6	665	3	20.16
482	6	1	983	12	287.21
482	6	2	582	1	20.88
482	6	3	19	2	58.44
482	6	4	300	3	79.14
482	6	5	739	1	25.28
483	1	1	906	1	10.33
483	1	2	380	2	54.90
483	1	3	686	1	19.52
483	1	4	234	12	290.40
483	1	5	963	2	50.64
483	2	1	906	12	113.63
483	2	2	830	3	25.74
483	2	3	721	12	55.77
483	2	4	38	2	11.50
483	3	1	256	2	56.02
483	3	2	258	12	194.48
483	3	3	472	2	16.72
483	3	4	783	2	55.74
483	3	5	912	2	45.88
483	3	6	403	12	274.78
483	4	1	657	2	34.86
484	1	1	967	3	22.44
484	1	2	1008	2	57.30
484	1	3	898	12	242.22
484	1	4	828	2	33.02
484	1	5	636	1	25.51
484	2	1	950	12	121.66
484	2	2	508	12	313.28
484	2	3	518	12	168.85
484	2	4	914	1	5.23
484	2	5	421	3	42.90
484	2	6	682	2	54.68
484	3	1	199	2	40.56
484	3	2	231	12	193.16
484	3	3	500	2	32.56
484	3	4	719	1	10.16
484	3	5	624	2	44.50
484	3	6	831	2	47.22
484	4	1	325	1	13.21
484	5	1	689	1	10.03
484	5	2	96	12	248.82
484	6	1	336	3	83.73
484	6	2	762	12	308.44
484	6	3	937	2	11.50
484	6	4	710	1	29.27
484	6	5	565	2	48.04
484	6	6	163	12	224.18
485	1	1	830	3	25.74
485	1	2	342	3	33.66
485	2	1	715	2	37.96
485	2	2	714	2	51.58
485	2	3	167	1	21.05
485	2	4	121	1	29.02
485	2	5	636	3	76.53
485	2	6	422	2	38.72
485	3	1	587	1	10.38
485	3	2	257	12	328.02
485	3	3	867	2	31.36
485	3	4	548	1	5.12
485	4	1	4	1	21.16
486	1	1	76	2	35.88
486	1	2	794	2	21.04
486	1	3	833	2	39.26
486	1	4	1003	12	107.47
486	1	5	609	12	247.61
487	1	1	855	3	21.69
487	1	2	666	12	171.05
487	1	3	784	3	26.61
487	1	4	984	1	6.17
487	2	1	72	2	49.92
487	2	2	752	2	17.18
487	2	3	363	3	44.16
487	2	4	1033	1	29.37
487	2	5	536	12	268.62
487	2	6	580	2	48.98
487	3	1	460	2	38.70
487	3	2	199	2	40.56
487	3	3	575	3	77.01
487	4	1	891	3	70.80
487	4	2	781	1	24.74
487	4	3	463	3	81.42
487	5	1	459	3	35.88
487	5	2	377	1	29.73
487	5	3	199	12	223.08
487	5	4	547	2	49.50
487	5	5	182	1	7.43
488	1	1	890	12	217.58
488	1	2	518	1	15.35
488	1	3	280	1	18.58
488	1	4	449	1	28.38
488	1	5	912	2	45.88
488	2	1	662	2	46.58
488	3	1	28	12	219.89
488	3	2	281	1	5.63
488	4	1	617	2	24.18
488	4	2	328	12	215.49
488	4	3	18	1	8.45
488	5	1	894	1	7.88
488	5	2	228	3	65.79
488	5	3	101	12	96.03
488	5	4	895	1	25.06
488	5	5	802	1	12.24
488	6	1	161	12	204.93
488	6	2	825	2	59.26
488	6	3	986	12	72.16
489	1	1	943	1	16.13
489	1	2	545	2	34.20
489	2	1	590	3	45.99
489	2	2	906	2	20.66
489	2	3	101	1	8.73
489	2	4	448	1	5.78
489	2	5	19	12	321.42
489	3	1	758	3	54.18
489	3	2	885	3	27.66
489	3	3	805	2	57.12
489	3	4	111	1	27.88
489	4	1	627	1	6.58
489	4	2	608	3	88.44
490	1	1	861	12	152.35
490	1	2	739	2	50.56
490	2	1	110	1	12.74
490	2	2	815	3	19.32
490	2	3	1033	3	88.11
490	3	1	919	3	40.05
490	3	2	887	12	166.10
490	3	3	571	1	17.25
491	1	1	810	1	27.71
491	1	2	529	1	29.10
491	1	3	110	3	38.22
491	1	4	926	12	270.60
491	1	5	768	3	24.24
491	1	6	775	1	10.66
492	1	1	573	1	20.36
492	2	1	434	1	25.44
492	2	2	464	2	30.14
492	2	3	654	3	80.40
492	2	4	545	2	34.20
492	2	5	453	2	29.54
492	3	1	665	1	6.72
492	4	1	773	3	76.20
492	4	2	387	12	169.73
492	4	3	300	12	290.18
492	4	4	45	3	23.13
492	4	5	207	3	74.61
492	4	6	290	3	26.73
492	5	1	555	12	96.69
492	5	2	301	1	29.58
492	6	1	398	2	58.28
492	6	2	527	3	82.20
492	6	3	300	12	290.18
492	6	4	556	2	11.56
492	6	5	541	3	52.86
492	6	6	1043	12	184.03
493	1	1	765	2	59.76
493	1	2	758	12	198.66
493	1	3	593	12	75.24
493	2	1	371	2	30.98
493	2	2	540	1	26.85
493	3	1	911	12	62.81
494	1	1	811	1	5.75
494	1	2	1	1	21.18
494	2	1	169	3	29.25
494	2	2	394	1	5.38
494	3	1	456	1	25.66
494	3	2	222	3	72.60
494	4	1	930	2	51.02
494	4	2	856	1	5.29
494	4	3	139	3	46.62
494	4	4	967	1	7.48
494	4	5	976	1	10.41
495	1	1	457	12	231.00
495	1	2	270	3	61.38
495	1	3	19	12	321.42
495	1	4	889	3	41.76
495	1	5	324	3	61.11
495	2	1	978	12	110.55
495	2	2	530	1	14.32
495	2	3	843	3	24.33
495	2	4	38	1	5.75
495	3	1	843	3	24.33
495	3	2	548	12	56.32
495	3	3	389	2	35.04
495	3	4	551	3	71.19
495	3	5	988	1	11.17
495	3	6	238	2	48.30
495	4	1	139	1	15.54
495	4	2	285	1	8.54
495	4	3	277	12	290.07
495	4	4	559	3	20.01
495	4	5	461	3	31.98
495	5	1	242	3	84.96
495	5	2	589	1	21.56
495	5	3	837	12	97.02
495	5	4	496	2	46.94
495	5	5	855	12	79.53
495	5	6	598	12	142.23
495	6	1	528	3	29.52
495	6	2	454	12	311.41
495	6	3	145	2	52.94
495	6	4	294	2	15.82
495	6	5	974	1	6.37
496	1	1	16	2	17.90
496	2	1	758	12	198.66
496	2	2	769	2	11.48
496	2	3	368	2	56.74
496	2	4	918	2	31.40
496	3	1	35	1	17.64
496	3	2	930	2	51.02
496	3	3	653	12	317.57
496	3	4	596	3	55.41
496	4	1	1003	3	29.31
496	4	2	766	3	89.58
496	4	3	38	1	5.75
496	4	4	33	12	206.25
496	4	5	392	3	39.51
496	4	6	214	12	307.45
497	1	1	380	12	301.95
497	1	2	453	12	162.47
497	1	3	430	12	115.17
497	1	4	631	12	55.00
497	1	5	289	2	38.72
497	1	6	896	1	27.56
497	2	1	43	2	55.64
497	3	1	403	12	274.78
498	1	1	193	3	72.54
498	1	2	858	1	5.33
498	1	3	921	12	278.30
498	2	1	229	1	6.99
498	2	2	332	1	25.29
498	3	1	97	12	133.21
498	3	2	313	12	222.42
498	3	3	1046	3	33.00
498	3	4	194	1	15.10
498	3	5	330	1	20.60
498	3	6	231	1	17.56
498	4	1	486	12	174.57
498	4	2	74	12	138.16
498	4	3	113	12	211.75
498	4	4	778	1	8.86
498	4	5	905	1	11.44
498	4	6	200	12	130.68
498	5	1	164	1	9.40
498	5	2	216	2	46.10
498	6	1	861	12	152.35
498	6	2	1001	12	316.36
498	6	3	624	1	22.25
498	6	4	849	2	57.92
498	6	5	282	3	83.49
498	6	6	445	2	42.86
499	1	1	594	2	12.16
499	1	2	30	12	89.76
499	1	3	561	1	8.64
499	1	4	169	12	107.25
499	1	5	83	3	46.62
499	2	1	632	2	32.96
499	2	2	47	2	48.08
499	2	3	716	3	41.55
499	2	4	751	2	49.78
499	2	5	985	1	20.34
499	3	1	900	2	33.88
499	3	2	918	1	15.70
499	3	3	70	12	176.55
499	3	4	556	12	63.58
499	3	5	897	12	252.89
500	1	1	757	12	235.07
500	1	2	254	1	22.55
500	1	3	840	2	29.26
500	1	4	959	1	10.07
500	2	1	289	12	212.96
500	2	2	54	1	20.31
500	2	3	496	12	258.17
500	2	4	91	2	23.16
500	2	5	436	3	17.97
500	2	6	116	3	35.61
500	3	1	922	3	69.00
500	3	2	228	12	241.23
500	3	3	943	1	16.13
500	3	4	458	12	88.11
500	3	5	880	12	292.82
500	4	1	790	2	24.86
500	4	2	667	1	23.07
501	1	1	977	2	29.02
501	1	2	679	3	35.43
501	1	3	656	2	50.86
501	1	4	608	12	324.28
501	1	5	920	3	60.84
502	1	1	600	1	16.99
502	2	1	892	1	7.25
502	2	2	587	3	31.14
502	2	3	54	12	223.41
502	2	4	279	3	47.91
502	2	5	623	3	66.30
502	2	6	327	2	50.40
502	3	1	586	3	53.10
502	4	1	308	1	13.06
502	4	2	566	1	7.04
502	4	3	309	1	19.40
502	4	4	799	2	27.74
503	1	1	256	1	28.01
503	1	2	645	3	54.39
504	1	1	591	1	12.70
504	1	2	572	12	302.39
504	1	3	200	1	11.88
504	1	4	1	2	42.36
504	1	5	193	12	265.98
504	1	6	871	2	20.44
504	2	1	565	12	264.22
504	2	2	479	1	11.70
504	2	3	401	1	16.75
504	2	4	176	3	62.22
504	2	5	231	2	35.12
504	2	6	550	1	11.22
504	3	1	226	12	95.48
504	3	2	602	1	11.72
504	3	3	201	2	34.76
504	3	4	999	1	17.61
504	3	5	930	1	25.51
505	1	1	30	12	89.76
505	1	2	627	2	13.16
505	1	3	472	3	25.08
506	1	1	102	3	20.13
506	1	2	1028	2	27.80
506	1	3	979	12	312.29
506	1	4	409	12	245.74
506	1	5	190	2	40.56
506	1	6	575	12	282.37
506	2	1	743	2	15.96
506	2	2	183	2	50.54
506	2	3	680	1	5.25
506	2	4	873	12	227.15
506	2	5	927	12	166.43
506	3	1	769	12	63.14
506	3	2	162	12	216.04
506	3	3	205	3	38.97
507	1	1	266	12	205.26
507	2	1	535	1	19.83
507	2	2	193	3	72.54
507	2	3	574	12	208.01
507	3	1	772	3	39.87
507	3	2	573	1	20.36
507	3	3	231	12	193.16
507	3	4	750	12	72.27
507	3	5	110	12	140.14
507	3	6	648	2	32.68
507	4	1	594	2	12.16
507	4	2	296	1	9.95
507	4	3	451	2	24.90
507	4	4	1042	1	26.76
507	4	5	1030	1	21.21
507	5	1	440	3	16.05
507	5	2	308	3	39.18
507	5	3	650	12	256.74
507	5	4	989	2	24.18
507	6	1	577	2	42.12
507	6	2	118	12	158.18
507	6	3	707	1	6.71
507	6	4	134	3	67.35
507	6	5	141	12	247.17
507	6	6	594	12	66.88
508	1	1	111	12	306.68
508	1	2	951	2	49.06
508	1	3	973	3	48.06
508	1	4	877	2	38.30
508	1	5	895	12	275.66
508	1	6	377	2	59.46
508	2	1	746	2	22.24
508	2	2	314	3	74.76
508	2	3	644	2	56.92
508	2	4	467	2	50.94
508	2	5	800	12	199.76
509	1	1	268	3	27.45
509	1	2	249	1	25.97
509	1	3	931	3	43.02
509	2	1	678	12	290.95
509	2	2	779	2	39.74
509	2	3	594	1	6.08
510	1	1	198	1	13.81
510	1	2	651	3	89.76
510	1	3	363	1	14.72
510	2	1	969	2	27.86
510	2	2	11	2	32.96
510	2	3	1025	12	316.58
510	3	1	599	3	16.71
510	4	1	712	12	310.31
510	4	2	132	2	36.86
510	5	1	65	12	109.78
510	5	2	624	1	22.25
510	5	3	913	12	238.48
510	5	4	580	1	24.49
511	1	1	362	12	252.12
511	1	2	417	3	41.76
511	1	3	620	2	51.60
511	1	4	825	12	325.93
511	1	5	151	2	44.74
511	2	1	673	2	52.04
511	2	2	365	2	16.80
511	2	3	55	2	42.82
511	3	1	335	2	19.16
511	4	1	118	1	14.38
511	4	2	870	3	68.67
511	4	3	639	3	70.59
511	4	4	617	1	12.09
511	5	1	985	3	61.02
511	5	2	210	3	36.21
511	5	3	302	1	22.70
511	5	4	705	3	29.91
511	5	5	654	3	80.40
512	1	1	776	12	259.93
512	1	2	320	2	41.84
512	1	3	561	2	17.28
512	1	4	320	12	230.12
512	1	5	1019	3	38.79
512	2	1	581	12	164.78
512	2	2	622	1	17.45
512	2	3	523	2	53.56
512	2	4	509	1	15.58
512	2	5	772	12	146.19
512	2	6	701	3	72.96
513	1	1	1023	2	30.98
513	2	1	756	1	29.52
513	2	2	479	3	35.10
513	2	3	854	3	74.25
513	2	4	100	1	23.46
513	2	5	109	2	32.00
513	2	6	927	12	166.43
514	1	1	899	1	7.35
514	1	2	591	2	25.40
514	1	3	58	1	26.60
514	1	4	190	3	60.84
514	2	1	541	3	52.86
514	2	2	746	3	33.36
514	2	3	810	12	304.81
514	2	4	575	3	77.01
514	3	1	614	12	98.23
514	4	1	694	3	47.28
514	4	2	560	3	68.73
514	4	3	441	3	17.61
514	4	4	313	12	222.42
514	4	5	268	1	9.15
514	4	6	629	1	24.19
514	5	1	237	2	50.96
514	6	1	339	2	21.34
514	6	2	661	12	121.88
514	6	3	96	1	22.62
514	6	4	698	3	46.50
514	6	5	786	12	229.24
515	1	1	888	2	11.56
515	1	2	119	12	196.02
515	1	3	682	2	54.68
515	1	4	200	2	23.76
515	1	5	529	3	87.30
515	2	1	111	1	27.88
515	2	2	413	3	55.02
515	2	3	914	2	10.46
515	2	4	67	12	305.80
515	2	5	349	1	15.59
515	3	1	76	2	35.88
515	3	2	167	2	42.10
515	3	3	883	3	27.90
515	3	4	943	2	32.26
515	3	5	319	1	14.70
516	1	1	189	2	33.46
517	1	1	256	2	56.02
517	1	2	335	2	19.16
517	1	3	665	2	13.44
517	1	4	607	2	44.38
518	1	1	503	1	24.79
518	1	2	210	3	36.21
518	1	3	236	2	44.02
518	1	4	96	2	45.24
518	1	5	457	12	231.00
518	1	6	568	2	22.64
519	1	1	819	1	7.77
519	1	2	446	12	235.84
519	1	3	663	1	7.23
519	1	4	385	2	41.62
519	1	5	955	1	18.91
519	1	6	242	2	56.64
520	1	1	1044	2	38.24
520	1	2	678	1	26.45
520	2	1	748	1	22.76
520	2	2	168	3	73.11
520	2	3	663	2	14.46
520	2	4	3	12	109.34
520	2	5	247	1	25.40
521	1	1	899	3	22.05
521	1	2	740	2	10.16
521	1	3	351	3	76.41
521	1	4	776	12	259.93
521	1	5	1036	3	35.22
521	2	1	490	3	24.93
521	2	2	261	2	30.74
522	1	1	746	12	122.32
522	1	2	708	2	18.24
522	1	3	678	12	290.95
522	1	4	770	2	31.62
522	1	5	716	3	41.55
522	2	1	937	2	11.50
522	3	1	243	12	212.85
522	3	2	872	2	52.58
522	3	3	766	3	89.58
522	3	4	421	2	28.60
522	4	1	800	1	18.16
522	4	2	1019	12	142.23
522	4	3	746	1	11.12
522	5	1	911	2	11.42
522	5	2	988	1	11.17
522	5	3	348	12	201.96
522	6	1	856	12	58.19
523	1	1	486	1	15.87
523	1	2	423	1	26.88
523	1	3	583	3	55.47
523	1	4	453	12	162.47
523	1	5	543	12	308.66
523	2	1	148	3	39.72
523	2	2	239	1	24.19
523	2	3	556	1	5.78
523	2	4	600	2	33.98
523	2	5	500	12	179.08
523	2	6	487	12	325.82
523	3	1	331	12	86.24
523	3	2	502	12	265.21
523	3	3	633	2	32.36
523	3	4	473	12	80.08
524	1	1	290	2	17.82
524	1	2	518	3	46.05
524	1	3	883	1	9.30
524	1	4	124	3	82.26
524	1	5	145	2	52.94
524	1	6	867	1	15.68
525	1	1	529	3	87.30
525	1	2	304	3	67.38
525	1	3	291	2	19.90
525	1	4	507	1	18.02
526	1	1	747	1	26.98
526	1	2	517	12	81.62
526	1	3	523	2	53.56
526	1	4	1023	3	46.47
526	1	5	489	3	64.05
526	1	6	954	2	11.02
527	1	1	505	2	55.26
527	2	1	937	12	63.25
527	2	2	233	1	5.65
527	2	3	426	1	8.67
527	2	4	533	3	56.22
527	3	1	75	2	45.46
527	3	2	822	12	121.33
527	3	3	615	3	70.71
527	4	1	537	12	289.30
527	4	2	515	2	50.18
527	4	3	831	3	70.83
527	4	4	436	1	5.99
527	4	5	823	12	255.20
527	4	6	970	12	314.27
527	5	1	108	12	178.31
528	1	1	130	3	47.28
528	1	2	927	1	15.13
528	1	3	110	2	25.48
528	2	1	455	3	86.97
528	2	2	38	12	63.25
528	2	3	577	3	63.18
528	2	4	1014	3	41.46
528	2	5	853	12	321.53
528	2	6	425	3	47.55
528	3	1	363	3	44.16
528	3	2	566	2	14.08
528	3	3	843	2	16.22
528	3	4	281	3	16.89
528	4	1	275	2	43.38
528	4	2	1043	1	16.73
528	5	1	596	3	55.41
528	5	2	102	1	6.71
528	5	3	93	2	43.34
528	6	1	307	1	5.28
529	1	1	865	1	29.58
529	1	2	659	12	268.18
529	1	3	334	12	156.75
529	1	4	998	3	36.90
529	1	5	531	3	74.34
530	1	1	193	2	48.36
530	1	2	1001	2	57.52
530	1	3	319	1	14.70
530	1	4	18	3	25.35
530	1	5	20	12	106.70
530	1	6	246	1	18.57
530	2	1	867	2	31.36
530	2	2	769	2	11.48
530	3	1	822	2	22.06
530	3	2	691	3	87.00
530	3	3	839	12	214.50
530	4	1	335	12	105.38
530	5	1	597	12	304.37
530	5	2	288	1	14.95
531	1	1	400	1	9.76
531	1	2	581	1	14.98
531	1	3	973	1	16.02
531	1	4	114	3	66.96
531	2	1	535	2	39.66
531	2	2	270	3	61.38
531	2	3	963	1	25.32
531	2	4	9	12	269.17
531	3	1	82	12	245.30
531	3	2	465	3	24.00
531	4	1	905	3	34.32
531	4	2	951	2	49.06
531	4	3	346	2	14.28
531	5	1	841	3	76.14
531	5	2	370	12	68.20
531	5	3	210	12	132.77
531	5	4	160	2	59.84
531	6	1	968	2	37.38
531	6	2	137	12	141.35
531	6	3	157	1	24.06
531	6	4	172	2	59.78
531	6	5	477	12	262.79
532	1	1	481	2	11.64
532	1	2	710	1	29.27
532	1	3	640	12	125.95
533	1	1	46	2	31.34
533	2	1	772	2	26.58
533	2	2	496	3	70.41
533	2	3	238	3	72.45
533	2	4	219	3	19.35
533	2	5	39	3	22.14
533	2	6	400	2	19.52
533	3	1	803	12	227.59
533	3	2	784	12	97.57
533	3	3	164	3	28.20
533	3	4	2	3	84.45
533	4	1	1028	1	13.90
533	5	1	138	3	64.53
533	5	2	74	3	37.68
533	5	3	767	3	19.59
533	5	4	279	2	31.94
533	5	5	432	1	25.45
533	5	6	322	3	50.91
534	1	1	449	1	28.38
534	1	2	389	3	52.56
534	1	3	706	3	78.06
534	2	1	433	1	21.31
534	3	1	17	1	25.23
534	3	2	958	1	10.83
534	3	3	415	12	124.41
534	3	4	723	2	41.66
534	4	1	3	2	19.88
534	4	2	659	3	73.14
535	1	1	751	3	74.67
535	1	2	205	2	25.98
535	1	3	664	2	52.16
535	1	4	248	12	287.98
535	1	5	702	1	8.82
535	2	1	351	2	50.94
535	2	2	251	3	17.13
535	3	1	855	3	21.69
535	3	2	190	3	60.84
535	3	3	373	2	51.14
535	4	1	241	2	33.90
535	4	2	668	12	163.35
535	5	1	954	1	5.51
535	5	2	125	12	97.02
535	5	3	494	3	85.05
535	5	4	443	2	58.64
535	5	5	447	3	40.74
536	1	1	992	12	115.61
536	1	2	801	2	10.34
536	1	3	649	3	51.27
536	1	4	938	2	53.70
536	2	1	703	1	26.77
536	2	2	478	2	35.66
536	2	3	685	3	33.66
536	2	4	821	2	11.38
536	2	5	587	3	31.14
536	2	6	245	1	28.86
536	3	1	68	3	64.98
536	3	2	219	12	70.95
536	3	3	964	12	146.63
536	3	4	866	1	14.59
536	4	1	424	2	17.16
536	4	2	976	1	10.41
536	5	1	746	12	122.32
536	5	2	539	3	31.44
536	5	3	614	3	26.79
536	5	4	290	12	98.01
536	5	5	563	1	26.82
537	1	1	646	2	15.90
537	1	2	199	12	223.08
537	1	3	236	2	44.02
537	1	4	615	3	70.71
537	1	5	133	2	35.08
537	2	1	787	1	9.59
537	2	2	114	3	66.96
537	2	3	805	2	57.12
537	2	4	180	1	18.57
537	2	5	778	3	26.58
537	3	1	256	1	28.01
537	3	2	229	12	76.89
537	3	3	927	2	30.26
537	3	4	276	2	46.44
537	3	5	582	2	41.76
537	3	6	488	3	15.30
537	4	1	744	1	13.54
538	1	1	341	3	52.20
538	1	2	650	2	46.68
538	1	3	801	2	10.34
538	2	1	402	1	19.08
539	1	1	730	1	24.66
539	2	1	1046	1	11.00
539	2	2	1030	12	233.31
539	2	3	608	2	58.96
539	3	1	611	3	63.96
539	3	2	806	1	8.34
539	3	3	571	3	51.75
539	3	4	580	1	24.49
539	3	5	723	1	20.83
539	3	6	153	2	46.22
540	1	1	110	1	12.74
540	1	2	519	2	23.52
540	1	3	78	12	170.50
540	1	4	547	3	74.25
540	1	5	208	3	30.57
540	1	6	547	12	272.25
540	2	1	647	3	87.96
540	3	1	886	3	52.59
540	3	2	221	12	134.53
540	4	1	724	3	61.89
540	4	2	961	2	20.16
540	4	3	103	1	12.37
540	5	1	400	12	107.36
540	5	2	950	12	121.66
540	6	1	204	1	24.28
540	6	2	808	12	284.46
540	6	3	624	3	66.75
541	1	1	572	3	82.47
541	2	1	1014	12	152.02
541	2	2	918	1	15.70
541	2	3	202	2	20.16
541	2	4	578	3	21.96
541	2	5	576	3	80.28
541	2	6	336	3	83.73
541	3	1	934	3	75.27
541	3	2	353	2	49.58
542	1	1	185	3	47.76
542	1	2	865	2	59.16
542	1	3	958	1	10.83
542	1	4	923	2	29.28
542	1	5	764	3	34.56
542	1	6	821	2	11.38
542	2	1	116	1	11.87
542	2	2	986	3	19.68
542	2	3	413	12	201.74
542	2	4	712	1	28.21
543	1	1	711	12	308.77
543	1	2	288	1	14.95
543	1	3	751	12	273.79
544	1	1	532	1	27.88
544	1	2	599	2	11.14
544	1	3	639	12	258.83
544	1	4	456	2	51.32
544	2	1	269	3	46.08
544	2	2	586	1	17.70
544	2	3	953	3	59.79
544	2	4	1006	3	60.48
544	2	5	653	12	317.57
544	2	6	319	2	29.40
544	3	1	169	12	107.25
544	4	1	536	3	73.26
544	4	2	197	12	129.03
544	5	1	72	12	274.56
544	5	2	265	2	33.46
544	5	3	1032	2	39.86
544	5	4	768	3	24.24
544	5	5	182	1	7.43
544	5	6	761	2	37.76
544	6	1	362	3	68.76
544	6	2	508	3	85.44
544	6	3	285	12	93.94
544	6	4	291	2	19.90
544	6	5	41	3	50.97
545	1	1	992	3	31.53
545	1	2	915	1	12.51
545	1	3	461	1	10.66
545	1	4	138	2	43.02
545	1	5	144	2	52.42
545	2	1	948	3	25.14
545	2	2	501	1	22.39
546	1	1	778	3	26.58
546	1	2	157	2	48.12
546	2	1	851	3	60.39
546	2	2	921	1	25.30
546	2	3	180	3	55.71
546	2	4	898	2	44.04
546	2	5	435	2	23.70
546	3	1	668	2	29.70
546	4	1	764	1	11.52
546	4	2	962	12	172.92
546	4	3	804	3	77.70
546	4	4	431	3	53.58
547	1	1	178	1	11.21
547	1	2	891	1	23.60
547	1	3	809	12	101.09
547	1	4	887	1	15.10
547	1	5	136	1	25.35
547	2	1	28	3	59.97
547	2	2	449	12	312.18
547	2	3	111	1	27.88
547	2	4	605	3	62.34
547	3	1	523	2	53.56
547	3	2	186	2	17.06
547	3	3	189	1	16.73
547	3	4	533	2	37.48
547	3	5	261	3	46.11
547	3	6	870	2	45.78
547	4	1	756	12	324.72
547	4	2	581	2	29.96
547	4	3	612	12	325.38
547	5	1	1002	3	71.91
547	5	2	711	1	28.07
547	5	3	785	1	28.11
548	1	1	395	1	16.99
548	1	2	201	12	191.18
548	1	3	942	1	28.23
548	2	1	690	3	60.21
548	2	2	338	1	24.28
548	2	3	538	1	14.50
548	2	4	781	3	74.22
548	2	5	835	12	160.16
548	3	1	524	3	45.78
548	3	2	795	3	64.38
548	3	3	444	3	54.48
548	3	4	816	12	127.27
549	1	1	129	2	11.50
549	1	2	435	1	11.85
549	1	3	72	1	24.96
549	1	4	1014	12	152.02
549	1	5	164	12	103.40
549	1	6	906	3	30.99
549	2	1	45	3	23.13
549	3	1	739	1	25.28
549	3	2	548	12	56.32
549	3	3	577	12	231.66
549	3	4	1046	3	33.00
549	4	1	614	3	26.79
549	4	2	818	12	72.27
549	4	3	163	3	61.14
549	4	4	1043	1	16.73
549	4	5	166	12	90.86
549	5	1	757	1	21.37
549	5	2	537	2	52.60
549	5	3	191	3	51.87
549	5	4	933	1	27.47
549	5	5	498	3	49.44
549	6	1	766	3	89.58
549	6	2	455	3	86.97
549	6	3	920	3	60.84
549	6	4	576	12	294.36
549	6	5	125	3	26.46
550	1	1	839	3	58.50
550	1	2	521	12	180.51
550	1	3	117	1	24.93
550	1	4	331	2	15.68
550	2	1	581	3	44.94
550	2	2	79	1	11.97
550	3	1	603	1	5.78
550	4	1	472	2	16.72
550	4	2	881	12	284.13
550	4	3	327	1	25.20
550	4	4	900	12	186.34
550	5	1	354	1	6.19
550	5	2	263	1	13.12
550	5	3	92	1	29.26
550	5	4	224	3	71.76
550	5	5	356	12	246.73
550	5	6	381	1	20.86
550	6	1	957	1	5.70
550	6	2	38	12	63.25
550	6	3	977	2	29.02
550	6	4	478	12	196.13
550	6	5	173	3	79.56
550	6	6	62	1	16.85
551	1	1	911	2	11.42
551	1	2	313	1	20.22
551	1	3	827	1	20.44
551	1	4	524	12	167.86
551	1	5	160	2	59.84
551	1	6	716	1	13.85
552	1	1	1032	2	39.86
552	1	2	375	1	23.40
552	2	1	165	3	81.51
552	2	2	39	1	7.38
552	2	3	563	1	26.82
552	2	4	716	2	27.70
552	2	5	198	12	151.91
552	3	1	401	12	184.25
552	3	2	1046	2	22.00
552	3	3	337	2	33.44
552	3	4	399	12	160.93
552	3	5	120	3	15.33
553	1	1	269	2	30.72
553	1	2	446	3	64.32
553	1	3	686	2	39.04
553	1	4	672	12	120.12
553	1	5	805	2	57.12
553	2	1	798	3	88.44
553	2	2	132	3	55.29
553	2	3	684	3	67.65
553	2	4	279	1	15.97
553	2	5	638	3	17.19
553	3	1	201	1	17.38
553	3	2	191	12	190.19
553	3	3	119	12	196.02
553	3	4	235	1	21.85
553	3	5	431	2	35.72
553	4	1	235	12	240.35
553	4	2	1046	2	22.00
553	4	3	875	3	40.08
554	1	1	742	12	250.69
554	1	2	807	3	29.61
554	1	3	107	3	19.23
554	1	4	800	1	18.16
555	1	1	315	12	285.23
555	1	2	702	2	17.64
555	1	3	321	3	35.07
555	1	4	946	2	19.02
555	1	5	999	12	193.71
555	1	6	719	12	111.76
556	1	1	876	2	25.46
556	1	2	811	12	63.25
556	1	3	559	2	13.34
556	1	4	991	2	53.12
556	2	1	692	2	35.80
556	3	1	282	2	55.66
557	1	1	351	3	76.41
558	1	1	1035	3	62.67
558	1	2	871	2	20.44
558	2	1	12	1	27.42
558	2	2	626	3	24.84
558	2	3	710	2	58.54
558	3	1	691	1	29.00
558	3	2	602	2	23.44
558	3	3	224	1	23.92
558	3	4	679	1	11.81
558	4	1	172	12	328.79
558	4	2	285	1	8.54
558	4	3	77	12	97.24
558	4	4	957	3	17.10
558	4	5	234	12	290.40
558	4	6	834	3	67.68
559	1	1	128	2	42.50
559	1	2	1004	2	48.36
560	1	1	2	12	309.65
560	2	1	650	12	256.74
560	3	1	986	3	19.68
560	3	2	423	12	295.68
560	3	3	995	12	154.88
560	3	4	350	12	153.89
560	3	5	678	1	26.45
560	3	6	932	2	42.14
561	1	1	253	3	84.72
561	1	2	428	1	6.24
561	1	3	660	1	12.11
561	1	4	633	2	32.36
561	1	5	873	1	20.65
561	2	1	5	3	20.01
561	2	2	123	1	7.70
561	2	3	410	2	35.08
562	1	1	223	12	143.44
562	2	1	978	1	10.05
562	2	2	264	3	38.52
562	2	3	892	12	79.75
562	3	1	952	3	64.44
562	3	2	552	3	15.90
563	1	1	117	1	24.93
563	1	2	937	12	63.25
563	2	1	572	12	302.39
563	2	2	265	2	33.46
563	2	3	777	3	71.10
563	2	4	623	12	243.10
563	2	5	629	1	24.19
563	2	6	538	2	29.00
563	3	1	355	12	223.63
563	3	2	777	1	23.70
563	3	3	965	3	77.55
563	3	4	633	12	177.98
563	3	5	781	1	24.74
564	1	1	880	12	292.82
564	1	2	229	12	76.89
564	1	3	472	3	25.08
564	1	4	445	3	64.29
564	2	1	1019	2	25.86
564	2	2	302	3	68.10
564	2	3	1047	1	13.85
565	1	1	585	3	80.55
565	1	2	87	2	34.38
565	2	1	24	3	55.80
565	2	2	245	2	57.72
565	2	3	700	3	58.05
565	2	4	238	1	24.15
565	2	5	931	12	157.74
565	2	6	64	12	289.63
565	3	1	555	3	26.37
565	3	2	932	3	63.21
566	1	1	154	1	5.97
566	1	2	144	2	52.42
566	1	3	855	3	21.69
566	1	4	657	12	191.73
566	1	5	656	2	50.86
566	1	6	983	3	78.33
567	1	1	463	1	27.14
567	2	1	72	12	274.56
567	2	2	605	1	20.78
567	2	3	17	12	277.53
567	3	1	1013	12	110.44
567	3	2	73	12	117.70
567	3	3	908	3	35.22
567	3	4	873	1	20.65
567	3	5	648	2	32.68
567	3	6	424	2	17.16
567	4	1	190	3	60.84
567	4	2	463	1	27.14
567	5	1	235	2	43.70
567	5	2	86	2	30.08
568	1	1	796	1	24.09
568	1	2	912	2	45.88
568	1	3	215	1	5.27
568	1	4	355	2	40.66
568	2	1	667	3	69.21
568	2	2	863	12	150.81
568	2	3	991	1	26.56
569	1	1	712	3	84.63
569	1	2	379	3	25.17
569	1	3	496	3	70.41
569	1	4	976	3	31.23
569	2	1	604	1	12.83
569	2	2	66	2	20.58
569	2	3	907	12	242.55
569	3	1	704	12	255.09
569	3	2	202	2	20.16
569	3	3	530	3	42.96
569	3	4	80	12	312.84
569	4	1	631	1	5.00
569	4	2	690	1	20.07
569	5	1	626	1	8.28
569	6	1	889	3	41.76
569	6	2	478	12	196.13
569	6	3	816	2	23.14
570	1	1	639	2	47.06
570	1	2	702	1	8.82
570	1	3	531	2	49.56
570	1	4	836	3	48.57
570	1	5	985	12	223.74
570	1	6	962	2	31.44
570	2	1	819	2	15.54
570	2	2	67	1	27.80
570	3	1	378	1	29.68
571	1	1	1034	3	44.88
571	1	2	725	12	265.98
571	2	1	404	12	297.11
571	2	2	892	1	7.25
571	2	3	1035	12	229.79
571	2	4	146	2	42.60
571	3	1	975	1	9.20
571	3	2	773	3	76.20
571	3	3	494	3	85.05
571	3	4	129	1	5.75
571	3	5	510	3	33.09
571	4	1	704	12	255.09
571	4	2	189	3	50.19
571	4	3	874	2	38.80
571	4	4	62	2	33.70
571	4	5	626	12	91.08
571	4	6	849	2	57.92
572	1	1	72	2	49.92
572	2	1	211	2	35.16
572	2	2	1028	1	13.90
572	3	1	542	1	19.52
572	3	2	42	1	21.28
572	3	3	597	2	55.34
572	3	4	936	12	223.74
572	4	1	539	12	115.28
573	1	1	843	3	24.33
573	1	2	239	3	72.57
573	2	1	291	1	9.95
573	2	2	183	3	75.81
574	1	1	482	2	49.90
574	1	2	498	12	181.28
574	2	1	991	12	292.16
574	2	2	116	12	130.57
575	1	1	329	1	14.98
575	1	2	1005	1	18.57
575	1	3	471	1	28.64
575	1	4	243	3	58.05
575	1	5	641	3	58.41
575	1	6	227	3	50.28
575	2	1	256	12	308.11
575	2	2	842	1	28.53
575	2	3	330	12	226.60
575	2	4	333	1	16.64
575	2	5	23	2	53.12
575	3	1	20	1	9.70
575	3	2	351	3	76.41
575	3	3	557	3	40.74
575	3	4	177	12	301.51
576	1	1	804	1	25.90
576	2	1	493	3	67.17
576	2	2	831	2	47.22
576	2	3	531	2	49.56
576	2	4	373	3	76.71
576	3	1	381	1	20.86
576	3	2	400	3	29.28
576	3	3	173	2	53.04
576	3	4	768	12	88.88
576	3	5	510	1	11.03
577	1	1	122	12	243.43
577	1	2	126	3	72.27
577	1	3	206	3	73.80
577	1	4	234	1	26.40
577	2	1	580	2	48.98
577	2	2	7	12	134.20
577	2	3	73	12	117.70
577	2	4	387	1	15.43
577	2	5	431	2	35.72
577	2	6	928	2	15.12
577	3	1	867	12	172.48
577	3	2	641	1	19.47
577	3	3	175	2	51.20
577	3	4	571	3	51.75
577	3	5	1008	3	85.95
577	3	6	209	12	162.80
578	1	1	167	12	231.55
578	1	2	735	3	66.33
578	1	3	82	12	245.30
578	1	4	579	12	84.48
578	1	5	245	2	57.72
578	2	1	686	12	214.72
578	2	2	814	2	27.04
578	2	3	202	3	30.24
578	2	4	669	3	40.86
578	2	5	409	12	245.74
579	1	1	245	12	317.46
579	1	2	475	1	28.84
579	1	3	1042	12	294.36
579	1	4	37	1	14.15
579	1	5	127	3	53.58
579	1	6	171	3	81.90
579	2	1	864	3	70.44
579	2	2	92	3	87.78
579	2	3	905	1	11.44
579	2	4	963	1	25.32
579	2	5	547	1	24.75
579	3	1	701	2	48.64
579	3	2	703	12	294.47
579	3	3	24	12	204.60
579	3	4	424	2	17.16
580	1	1	282	2	55.66
580	1	2	214	2	55.90
580	1	3	200	1	11.88
580	2	1	34	2	28.56
580	2	2	1029	3	45.57
580	3	1	197	2	23.46
580	3	2	323	1	13.60
580	3	3	605	12	228.58
580	4	1	373	3	76.71
580	4	2	882	12	238.59
580	4	3	279	3	47.91
580	4	4	111	2	55.76
580	4	5	904	1	18.63
581	1	1	234	3	79.20
581	2	1	115	2	28.04
581	2	2	91	12	127.38
581	2	3	834	12	248.16
581	2	4	747	3	80.94
581	3	1	1017	3	42.90
581	4	1	251	1	5.71
581	4	2	796	3	72.27
581	4	3	718	12	111.10
581	4	4	331	12	86.24
581	5	1	239	3	72.57
581	5	2	397	3	34.14
581	5	3	790	1	12.43
581	5	4	772	3	39.87
581	5	5	839	2	39.00
582	1	1	375	1	23.40
582	1	2	186	1	8.53
582	1	3	383	12	207.79
582	2	1	185	3	47.76
582	2	2	708	3	27.36
582	3	1	116	12	130.57
582	3	2	678	2	52.90
582	3	3	474	1	26.72
583	1	1	233	12	62.15
583	1	2	164	1	9.40
583	1	3	815	2	12.88
583	1	4	364	2	45.58
583	2	1	349	2	31.18
583	2	2	744	1	13.54
583	2	3	106	1	26.10
583	2	4	668	1	14.85
583	3	1	16	12	98.45
583	3	2	217	1	6.83
583	3	3	80	12	312.84
583	4	1	97	1	12.11
583	4	2	334	2	28.50
583	4	3	690	1	20.07
583	4	4	640	12	125.95
583	4	5	232	1	14.33
583	5	1	162	1	19.64
583	5	2	749	2	11.14
583	5	3	367	12	218.57
583	5	4	361	12	140.36
583	5	5	223	12	143.44
583	6	1	257	12	328.02
583	6	2	831	12	259.71
583	6	3	650	1	23.34
583	6	4	107	3	19.23
583	6	5	421	12	157.30
584	1	1	508	12	313.28
584	1	2	1015	2	56.70
584	1	3	176	3	62.22
584	1	4	196	12	227.15
584	2	1	200	12	130.68
584	2	2	427	1	20.01
584	2	3	962	12	172.92
584	2	4	635	3	64.17
584	2	5	595	3	86.85
584	3	1	150	3	25.98
584	3	2	783	2	55.74
584	4	1	1010	12	210.98
584	4	2	770	1	15.81
584	4	3	733	1	9.71
584	4	4	375	3	70.20
584	4	5	70	12	176.55
584	4	6	343	3	17.10
585	1	1	1030	12	233.31
585	2	1	443	1	29.32
585	2	2	310	2	20.52
585	3	1	806	12	91.74
585	4	1	389	12	192.72
585	4	2	163	1	20.38
585	5	1	388	3	73.17
585	5	2	53	3	79.86
585	5	3	458	2	16.02
585	5	4	712	2	56.42
585	5	5	321	2	23.38
586	1	1	974	3	19.11
586	1	2	1047	12	152.35
586	1	3	837	1	8.82
586	1	4	434	1	25.44
586	2	1	673	1	26.02
586	2	2	89	1	27.85
586	2	3	962	2	31.44
586	2	4	468	1	28.60
586	3	1	703	3	80.31
586	3	2	222	3	72.60
586	3	3	355	1	20.33
586	3	4	918	12	172.70
587	1	1	862	2	13.42
587	2	1	974	12	70.07
587	2	2	801	12	56.87
587	2	3	529	2	58.20
587	2	4	672	1	10.92
588	1	1	801	2	10.34
588	1	2	89	12	306.35
588	1	3	283	1	28.83
588	1	4	803	3	62.07
588	2	1	69	12	182.71
588	2	2	575	1	25.67
588	2	3	452	12	85.58
588	2	4	710	3	87.81
588	2	5	445	12	235.73
588	3	1	43	12	306.02
588	3	2	866	2	29.18
588	3	3	75	3	68.19
588	3	4	871	2	20.44
588	3	5	336	3	83.73
588	3	6	630	2	56.00
588	4	1	853	1	29.23
588	4	2	497	1	21.46
588	4	3	232	2	28.66
588	4	4	224	2	47.84
588	4	5	560	12	252.01
588	4	6	183	2	50.54
589	1	1	162	3	58.92
589	1	2	205	3	38.97
589	1	3	310	3	30.78
589	1	4	140	12	65.45
590	1	1	698	12	170.50
590	2	1	899	1	7.35
590	2	2	906	3	30.99
590	2	3	352	2	19.50
590	2	4	887	1	15.10
590	2	5	685	2	22.44
590	3	1	77	1	8.84
590	3	2	184	3	81.33
590	3	3	374	12	102.08
590	3	4	434	12	279.84
590	3	5	42	1	21.28
590	3	6	639	3	70.59
590	4	1	718	1	10.10
590	4	2	553	2	18.46
590	4	3	532	12	306.68
591	1	1	113	1	19.25
591	1	2	990	12	140.58
591	1	3	462	12	261.58
591	1	4	1036	3	35.22
591	2	1	378	2	59.36
591	3	1	15	3	36.99
591	3	2	994	2	30.88
591	3	3	632	2	32.96
591	3	4	1029	3	45.57
591	4	1	226	2	17.36
591	4	2	62	1	16.85
591	4	3	466	12	129.91
592	1	1	620	2	51.60
593	1	1	140	2	11.90
593	1	2	242	12	311.52
593	1	3	220	12	129.14
593	1	4	387	2	30.86
594	1	1	326	3	39.48
594	1	2	337	1	16.72
594	1	3	792	12	71.39
595	1	1	638	12	63.03
595	1	2	419	12	121.44
595	1	3	966	3	31.35
595	1	4	749	2	11.14
595	1	5	36	12	324.61
595	1	6	752	2	17.18
595	2	1	737	2	52.38
595	2	2	290	3	26.73
595	2	3	135	1	11.58
595	2	4	825	1	29.63
595	3	1	282	12	306.13
595	3	2	606	12	278.41
595	3	3	269	3	46.08
595	3	4	902	12	118.47
595	3	5	102	2	13.42
595	3	6	1008	1	28.65
595	4	1	691	1	29.00
596	1	1	360	2	23.88
596	1	2	1013	2	20.08
596	1	3	988	12	122.87
596	1	4	594	3	18.24
596	1	5	848	3	52.11
596	2	1	168	12	268.07
596	2	2	74	1	12.56
597	1	1	671	12	288.09
597	1	2	1024	1	19.85
597	1	3	968	2	37.38
597	1	4	994	12	169.84
597	2	1	948	1	8.38
597	2	2	769	1	5.74
597	2	3	408	3	47.85
597	2	4	153	3	69.33
597	2	5	175	1	25.60
597	2	6	4	1	21.16
597	3	1	935	2	57.12
597	3	2	498	12	181.28
597	4	1	638	2	11.46
597	4	2	957	2	11.40
597	4	3	494	2	56.70
597	4	4	1009	3	66.39
597	4	5	933	1	27.47
598	1	1	83	12	170.94
598	2	1	414	2	44.46
598	3	1	307	2	10.56
598	3	2	76	1	17.94
598	3	3	416	12	279.29
598	4	1	489	12	234.85
598	4	2	168	12	268.07
598	5	1	634	12	86.57
598	5	2	139	3	46.62
598	6	1	109	1	16.00
598	6	2	421	12	157.30
599	1	1	507	2	36.04
599	2	1	541	3	52.86
599	2	2	151	2	44.74
599	2	3	21	1	19.14
599	2	4	537	3	78.90
599	3	1	898	12	242.22
599	3	2	226	3	26.04
599	3	3	452	3	23.34
599	3	4	768	12	88.88
599	3	5	1033	3	88.11
600	1	1	330	2	41.20
600	1	2	331	3	23.52
600	2	1	510	12	121.33
600	2	2	807	2	19.74
600	2	3	164	2	18.80
600	2	4	741	3	38.67
600	2	5	228	3	65.79
600	2	6	688	12	173.47
600	3	1	979	12	312.29
600	3	2	132	1	18.43
600	3	3	515	12	275.99
600	3	4	305	2	10.54
600	3	5	666	1	15.55
600	4	1	842	3	85.59
600	4	2	246	1	18.57
600	4	3	265	12	184.03
600	4	4	948	3	25.14
600	5	1	69	2	33.22
600	5	2	664	3	78.24
600	5	3	410	12	192.94
601	1	1	690	2	40.14
601	1	2	728	2	10.96
601	1	3	552	3	15.90
601	1	4	568	2	22.64
601	1	5	649	1	17.09
601	1	6	240	1	11.44
601	2	1	730	3	73.98
601	2	2	867	3	47.04
601	2	3	608	12	324.28
601	3	1	968	1	18.69
601	4	1	98	2	48.98
601	5	1	556	2	11.56
601	5	2	106	1	26.10
601	6	1	728	2	10.96
601	6	2	419	3	33.12
601	6	3	818	12	72.27
602	1	1	606	3	75.93
602	1	2	531	2	49.56
602	1	3	269	12	168.96
602	1	4	987	3	83.82
602	1	5	924	1	20.67
602	2	1	951	3	73.59
602	2	2	297	3	75.81
602	2	3	437	1	8.61
602	2	4	46	1	15.67
602	2	5	885	12	101.42
602	2	6	1010	3	57.54
602	3	1	964	3	39.99
602	3	2	478	12	196.13
602	3	3	705	1	9.97
602	3	4	666	3	46.65
602	3	5	447	12	149.38
602	3	6	905	2	22.88
603	1	1	596	1	18.47
603	2	1	90	3	20.31
603	2	2	728	3	16.44
603	2	3	66	2	20.58
603	2	4	356	1	22.43
603	2	5	137	12	141.35
603	3	1	101	1	8.73
603	3	2	142	3	45.00
603	3	3	288	3	44.85
603	4	1	723	2	41.66
603	4	2	319	2	29.40
603	4	3	127	2	35.72
603	4	4	921	2	50.60
603	4	5	470	1	9.44
603	5	1	192	12	246.07
603	5	2	834	2	45.12
603	5	3	332	1	25.29
603	5	4	23	1	26.56
604	1	1	1021	2	43.54
604	1	2	931	2	28.68
604	1	3	224	2	47.84
604	1	4	827	1	20.44
604	1	5	804	3	77.70
604	2	1	383	1	18.89
604	2	2	53	2	53.24
604	2	3	38	1	5.75
604	2	4	821	12	62.59
604	2	5	77	1	8.84
604	3	1	61	3	50.94
604	3	2	309	12	213.40
604	3	3	804	1	25.90
604	3	4	619	12	228.69
604	3	5	73	1	10.70
604	3	6	940	3	36.54
605	1	1	232	12	157.63
605	1	2	893	2	48.88
605	1	3	132	3	55.29
605	1	4	277	1	26.37
605	1	5	326	3	39.48
605	2	1	844	12	207.02
605	2	2	582	12	229.68
605	2	3	817	1	18.75
605	3	1	909	12	240.24
605	3	2	229	3	20.97
605	3	3	126	2	48.18
605	4	1	127	1	17.86
605	4	2	217	1	6.83
605	4	3	598	1	12.93
605	4	4	956	1	20.11
605	4	5	966	3	31.35
605	4	6	333	3	49.92
605	5	1	1047	12	152.35
606	1	1	313	3	60.66
606	1	2	630	2	56.00
606	1	3	950	3	33.18
606	2	1	498	2	32.96
606	2	2	952	3	64.44
606	2	3	545	1	17.10
606	3	1	452	3	23.34
606	4	1	780	2	14.62
606	4	2	352	3	29.25
606	4	3	967	2	14.96
606	5	1	6	12	222.75
606	5	2	843	3	24.33
606	5	3	859	12	112.86
606	5	4	969	12	153.23
607	1	1	589	12	237.16
607	1	2	79	12	131.67
607	2	1	228	3	65.79
607	2	2	191	12	190.19
607	2	3	534	12	177.65
607	3	1	385	2	41.62
607	3	2	781	12	272.14
607	4	1	456	1	25.66
607	4	2	625	1	17.99
607	4	3	256	3	84.03
607	4	4	590	2	30.66
607	4	5	123	3	23.10
607	5	1	760	1	6.12
607	5	2	568	1	11.32
608	1	1	428	2	12.48
608	1	2	190	2	40.56
608	1	3	1035	2	41.78
608	1	4	688	3	47.31
608	1	5	880	1	26.62
608	2	1	779	3	59.61
608	3	1	498	1	16.48
608	3	2	1012	1	17.85
608	3	3	838	2	38.60
608	3	4	552	12	58.30
608	3	5	823	3	69.60
608	4	1	1014	1	13.82
608	4	2	1006	2	40.32
608	4	3	243	3	58.05
608	4	4	1018	1	9.22
608	4	5	699	1	19.71
609	1	1	545	1	17.10
609	1	2	426	2	17.34
609	1	3	871	12	112.42
609	1	4	793	2	32.74
609	1	5	36	12	324.61
609	1	6	869	12	197.23
609	2	1	77	2	17.68
609	2	2	902	1	10.77
609	3	1	794	1	10.52
609	3	2	624	1	22.25
609	3	3	689	2	20.06
609	3	4	86	2	30.08
609	4	1	24	2	37.20
609	4	2	446	2	42.88
609	5	1	699	3	59.13
609	5	2	368	2	56.74
609	5	3	185	1	15.92
609	6	1	939	2	28.74
609	6	2	837	2	17.64
609	6	3	470	3	28.32
609	6	4	780	2	14.62
610	1	1	99	1	24.40
610	1	2	85	3	64.62
610	1	3	558	2	57.18
610	2	1	274	3	18.66
610	2	2	799	3	41.61
610	2	3	316	1	28.78
610	2	4	185	12	175.12
610	2	5	972	3	72.63
610	2	6	818	1	6.57
610	3	1	38	2	11.50
610	3	2	598	3	38.79
610	3	3	675	2	43.86
610	3	4	217	2	13.66
610	3	5	143	1	12.49
610	3	6	485	1	22.86
610	4	1	170	12	107.25
610	4	2	970	12	314.27
610	5	1	987	12	307.34
610	5	2	176	3	62.22
610	5	3	573	12	223.96
610	5	4	1027	12	316.03
610	5	5	319	1	14.70
610	5	6	911	12	62.81
610	6	1	659	1	24.38
610	6	2	738	3	72.66
610	6	3	104	3	19.26
610	6	4	934	1	25.09
610	6	5	425	12	174.35
611	1	1	434	2	50.88
611	1	2	625	3	53.97
612	1	1	375	3	70.20
612	2	1	147	12	215.38
612	2	2	693	2	56.16
612	2	3	891	12	259.60
612	2	4	981	2	49.80
613	1	1	915	3	37.53
613	1	2	292	2	15.68
613	2	1	1002	3	71.91
613	3	1	962	1	15.72
613	3	2	70	2	32.10
613	3	3	325	12	145.31
613	3	4	184	1	27.11
613	3	5	1037	2	53.34
614	1	1	452	3	23.34
614	2	1	452	1	7.78
615	1	1	632	12	181.28
615	2	1	246	3	55.71
616	1	1	809	2	18.38
616	1	2	91	12	127.38
616	1	3	747	2	53.96
616	1	4	26	1	26.03
616	2	1	990	2	25.56
616	2	2	572	12	302.39
616	2	3	154	3	17.91
616	2	4	1035	12	229.79
617	1	1	556	12	63.58
617	1	2	580	2	48.98
617	1	3	511	12	161.92
617	2	1	574	3	56.73
617	3	1	999	12	193.71
617	3	2	355	12	223.63
617	3	3	743	1	7.98
617	3	4	753	12	238.70
617	3	5	645	1	18.13
617	4	1	472	1	8.36
617	4	2	966	3	31.35
617	4	3	169	2	19.50
617	4	4	215	1	5.27
617	4	5	511	2	29.44
617	5	1	593	12	75.24
617	5	2	1023	2	30.98
617	5	3	974	3	19.11
617	5	4	558	3	85.77
617	5	5	795	2	42.92
617	6	1	765	12	328.68
617	6	2	130	12	173.36
617	6	3	935	3	85.68
617	6	4	404	3	81.03
618	1	1	1037	3	80.01
618	1	2	909	3	65.52
618	1	3	263	3	39.36
618	1	4	355	3	60.99
618	1	5	559	1	6.67
618	1	6	1013	2	20.08
618	2	1	282	3	83.49
619	1	1	236	1	22.01
619	2	1	729	1	6.71
619	3	1	659	2	48.76
619	3	2	641	12	214.17
619	3	3	1029	1	15.19
619	3	4	903	12	258.94
620	1	1	903	2	47.08
620	1	2	65	1	9.98
620	2	1	894	3	23.64
620	2	2	802	2	24.48
620	2	3	306	1	26.47
621	1	1	699	3	59.13
622	1	1	648	12	179.74
622	1	2	263	3	39.36
622	1	3	87	3	51.57
622	1	4	242	12	311.52
622	2	1	163	3	61.14
622	2	2	895	1	25.06
622	2	3	827	12	224.84
622	2	4	833	2	39.26
622	2	5	160	2	59.84
623	1	1	603	2	11.56
623	2	1	188	12	84.81
623	2	2	1027	12	316.03
623	2	3	966	3	31.35
623	2	4	878	1	22.43
623	3	1	652	1	10.28
623	3	2	332	12	278.19
623	4	1	184	2	54.22
623	4	2	780	12	80.41
624	1	1	623	3	66.30
624	1	2	520	2	20.02
624	1	3	434	2	50.88
624	1	4	691	3	87.00
624	1	5	23	3	79.68
624	2	1	345	1	8.23
624	2	2	674	3	44.61
624	2	3	989	2	24.18
624	3	1	541	2	35.24
624	3	2	645	1	18.13
624	3	3	735	1	22.11
624	3	4	431	2	35.72
624	3	5	696	2	13.24
624	3	6	67	3	83.40
624	4	1	371	3	46.47
624	4	2	559	2	13.34
624	4	3	218	1	9.06
624	4	4	170	2	19.50
624	4	5	700	12	212.85
624	5	1	233	12	62.15
624	5	2	341	12	191.40
624	5	3	321	2	23.38
624	5	4	526	3	57.93
624	5	5	551	1	23.73
624	5	6	226	3	26.04
624	6	1	756	2	59.04
625	1	1	354	3	18.57
625	2	1	257	12	328.02
625	2	2	909	2	43.68
625	2	3	583	1	18.49
625	2	4	514	3	48.54
625	2	5	946	1	9.51
625	3	1	839	1	19.50
625	3	2	741	2	25.78
625	3	3	300	1	26.38
625	3	4	119	3	53.46
625	3	5	268	1	9.15
626	1	1	902	3	32.31
626	1	2	1013	2	20.08
626	1	3	525	1	22.09
626	2	1	704	1	23.19
626	2	2	322	1	16.97
626	2	3	386	2	56.90
626	2	4	271	1	6.89
626	3	1	72	1	24.96
626	3	2	913	12	238.48
626	3	3	877	12	210.65
626	4	1	723	3	62.49
626	4	2	115	2	28.04
626	4	3	719	2	20.32
626	4	4	531	1	24.78
626	5	1	904	3	55.89
626	5	2	152	12	63.91
626	5	3	1008	1	28.65
626	5	4	901	12	172.37
626	5	5	893	1	24.44
626	6	1	1007	3	37.02
626	6	2	4	3	63.48
626	6	3	11	3	49.44
626	6	4	733	12	106.81
627	1	1	46	1	15.67
627	1	2	554	1	6.30
627	1	3	26	2	52.06
627	1	4	954	2	11.02
627	2	1	980	2	44.64
628	1	1	260	1	7.34
629	1	1	669	2	27.24
629	1	2	167	12	231.55
629	2	1	718	1	10.10
629	2	2	334	2	28.50
629	3	1	989	3	36.27
629	3	2	1028	2	27.80
629	3	3	1040	1	28.12
629	3	4	7	12	134.20
629	3	5	440	3	16.05
629	3	6	339	3	32.01
629	4	1	603	12	63.58
629	4	2	722	12	165.00
629	4	3	56	1	12.83
629	4	4	818	3	19.71
629	5	1	886	3	52.59
629	5	2	783	12	306.57
629	5	3	743	1	7.98
629	5	4	519	1	11.76
629	6	1	246	1	18.57
629	6	2	1039	12	291.06
630	1	1	479	3	35.10
630	1	2	596	1	18.47
630	1	3	541	2	35.24
630	1	4	818	2	13.14
630	1	5	1039	12	291.06
630	1	6	903	1	23.54
630	2	1	882	3	65.07
630	2	2	1028	12	152.90
630	2	3	621	1	29.46
630	2	4	905	2	22.88
630	2	5	697	3	66.27
630	2	6	545	3	51.30
630	3	1	76	2	35.88
630	3	2	984	12	67.87
630	3	3	394	3	16.14
630	3	4	872	3	78.87
630	3	5	437	2	17.22
630	3	6	180	2	37.14
630	4	1	765	1	29.88
630	4	2	662	1	23.29
630	4	3	614	12	98.23
630	4	4	461	3	31.98
630	4	5	26	1	26.03
630	4	6	26	1	26.03
630	5	1	310	1	10.26
630	5	2	442	2	27.34
630	5	3	427	3	60.03
630	5	4	317	12	63.69
630	5	5	783	3	83.61
630	5	6	215	3	15.81
630	6	1	556	1	5.78
631	1	1	711	3	84.21
631	1	2	1025	3	86.34
631	1	3	69	2	33.22
631	1	4	91	12	127.38
631	1	5	782	12	132.66
631	2	1	672	1	10.92
631	2	2	575	12	282.37
631	2	3	798	3	88.44
632	1	1	607	12	244.09
632	1	2	952	2	42.96
632	1	3	1005	2	37.14
632	1	4	361	3	38.28
632	1	5	103	1	12.37
632	1	6	251	3	17.13
633	1	1	980	3	66.96
633	1	2	80	1	28.44
633	1	3	1028	12	152.90
633	2	1	68	3	64.98
633	2	2	736	2	52.12
633	2	3	1011	2	22.24
633	2	4	540	1	26.85
633	2	5	699	1	19.71
634	1	1	712	3	84.63
634	1	2	435	3	35.55
634	1	3	753	2	43.40
634	2	1	175	1	25.60
634	2	2	617	3	36.27
634	2	3	29	1	20.97
634	2	4	104	1	6.42
634	2	5	715	1	18.98
634	2	6	745	12	112.86
634	3	1	724	2	41.26
634	3	2	941	2	33.14
635	1	1	652	2	20.56
635	1	2	850	1	29.32
635	2	1	275	2	43.38
635	2	2	162	12	216.04
635	3	1	215	1	5.27
635	3	2	100	3	70.38
635	3	3	1006	2	40.32
635	3	4	15	1	12.33
635	3	5	684	3	67.65
635	3	6	568	2	22.64
635	4	1	767	1	6.53
635	4	2	955	12	208.01
635	4	3	6	3	60.75
635	4	4	1041	12	188.98
635	5	1	580	2	48.98
635	5	2	156	3	22.47
635	5	3	739	12	278.08
635	5	4	213	1	25.17
635	5	5	276	1	23.22
636	1	1	773	1	25.40
636	1	2	432	2	50.90
636	1	3	214	2	55.90
637	1	1	819	3	23.31
637	1	2	113	2	38.50
637	1	3	479	2	23.40
637	1	4	767	3	19.59
637	2	1	827	1	20.44
637	2	2	111	1	27.88
637	2	3	459	3	35.88
638	1	1	23	3	79.68
638	1	2	703	3	80.31
638	1	3	875	1	13.36
638	1	4	142	1	15.00
638	1	5	930	12	280.61
638	1	6	522	12	80.30
638	2	1	539	1	10.48
638	2	2	222	1	24.20
638	2	3	225	1	16.96
638	2	4	829	2	21.20
638	2	5	660	2	24.22
639	1	1	147	2	39.16
639	1	2	243	12	212.85
639	1	3	841	12	279.18
639	2	1	801	1	5.17
639	2	2	895	12	275.66
639	2	3	461	12	117.26
639	2	4	652	12	113.08
639	3	1	268	12	100.65
639	3	2	597	2	55.34
639	4	1	1042	2	53.52
639	4	2	140	1	5.95
639	4	3	22	12	88.55
639	4	4	457	12	231.00
639	4	5	764	12	126.72
639	4	6	800	2	36.32
640	1	1	820	12	326.26
640	1	2	118	1	14.38
640	1	3	136	12	278.85
640	1	4	594	1	6.08
640	2	1	500	1	16.28
640	2	2	356	2	44.86
640	2	3	852	1	26.09
640	2	4	120	1	5.11
640	2	5	986	2	13.12
640	2	6	461	1	10.66
640	3	1	971	1	19.81
640	3	2	294	2	15.82
640	3	3	273	12	65.23
640	4	1	614	12	98.23
640	4	2	893	2	48.88
640	4	3	134	12	246.95
640	4	4	590	1	15.33
640	5	1	753	1	21.70
640	5	2	734	3	54.93
641	1	1	766	2	59.72
641	1	2	217	1	6.83
641	1	3	1015	1	28.35
641	1	4	87	2	34.38
641	2	1	507	12	198.22
641	2	2	797	12	200.75
641	2	3	647	12	322.52
641	2	4	540	2	53.70
642	1	1	874	1	19.40
642	2	1	205	2	25.98
642	2	2	1027	3	86.19
642	2	3	326	2	26.32
642	2	4	92	1	29.26
643	1	1	510	2	22.06
643	1	2	384	3	63.21
643	1	3	92	2	58.52
643	1	4	443	1	29.32
643	1	5	780	2	14.62
643	1	6	299	1	13.23
643	2	1	446	2	42.88
643	2	2	543	3	84.18
643	2	3	548	2	10.24
644	1	1	121	3	87.06
644	1	2	723	3	62.49
644	1	3	50	12	171.82
644	1	4	325	3	39.63
644	2	1	87	2	34.38
644	3	1	658	1	24.88
644	3	2	106	12	287.10
644	3	3	994	1	15.44
644	3	4	499	1	7.12
645	1	1	425	3	47.55
645	1	2	151	3	67.11
645	2	1	744	1	13.54
645	2	2	245	2	57.72
645	2	3	680	2	10.50
645	2	4	377	3	89.19
645	2	5	400	2	19.52
645	2	6	1047	2	27.70
645	3	1	475	2	57.68
645	3	2	966	3	31.35
645	3	3	112	2	15.90
645	4	1	39	2	14.76
645	5	1	218	3	27.18
645	5	2	497	2	42.92
646	1	1	672	2	21.84
646	1	2	314	2	49.84
646	1	3	211	3	52.74
646	1	4	836	1	16.19
646	1	5	915	12	137.61
646	1	6	265	2	33.46
646	2	1	371	1	15.49
646	2	2	693	3	84.24
646	3	1	35	1	17.64
646	3	2	335	2	19.16
646	3	3	429	3	49.56
646	3	4	1030	1	21.21
646	3	5	773	12	279.40
646	3	6	108	12	178.31
646	4	1	543	2	56.12
646	4	2	85	1	21.54
646	4	3	159	1	14.76
646	4	4	401	2	33.50
646	4	5	890	3	59.34
646	4	6	868	12	128.70
646	5	1	464	2	30.14
646	5	2	777	3	71.10
646	5	3	433	12	234.41
646	5	4	946	2	19.02
646	5	5	338	1	24.28
646	5	6	433	1	21.31
647	1	1	210	12	132.77
647	1	2	299	1	13.23
647	1	3	441	1	5.87
647	2	1	997	2	18.06
647	2	2	772	12	146.19
647	3	1	344	3	45.18
647	3	2	156	2	14.98
647	4	1	764	1	11.52
647	4	2	522	1	7.30
647	5	1	479	1	11.70
647	5	2	934	3	75.27
647	5	3	300	2	52.76
647	5	4	24	3	55.80
648	1	1	683	2	18.38
648	1	2	526	12	212.41
648	1	3	238	3	72.45
648	2	1	792	1	6.49
648	2	2	10	12	196.02
648	2	3	56	3	38.49
648	2	4	136	2	50.70
648	3	1	635	12	235.29
648	3	2	25	2	29.82
648	3	3	440	3	16.05
648	3	4	202	3	30.24
648	4	1	10	1	17.82
648	4	2	71	1	7.50
648	4	3	67	1	27.80
648	4	4	587	1	10.38
648	4	5	563	1	26.82
648	4	6	10	3	53.46
649	1	1	636	1	25.51
650	1	1	699	2	39.42
650	1	2	902	3	32.31
650	1	3	382	12	77.99
650	1	4	897	12	252.89
650	2	1	203	3	44.61
650	2	2	140	2	11.90
\.


--
-- Data for Name: orders; Type: TABLE DATA; Schema: public; Owner: -
--

COPY orders (cust_id, order_id, date, instructions, creditcard, expirydate) FROM stdin;
1	1	000113133121		8000000000001001	01/05
1	2	000205101701		8000000000001001	01/05
1	3	000326214418		8000000000001001	01/05
1	4	000420184456		8000000000001001	01/05
2	1	000113200418		8000000000001001	01/05
2	2	000225105446		8000000000001001	01/05
2	3	000301154527		8000000000001001	01/05
2	4	000404082605		8000000000001001	01/05
2	5	000520225329		8000000000001001	01/05
3	1	000122133053		8000000000001001	01/05
3	2	000229194813		8000000000001001	01/05
3	3	000311091605		8000000000001001	01/05
3	4	000411183516		8000000000001001	01/05
3	5	000502113648		8000000000001001	01/05
4	1	000118080218		8000000000001001	01/05
4	2	000218140724		8000000000001001	01/05
5	1	000127214213		8000000000001001	01/05
5	2	000229231026		8000000000001001	01/05
5	3	000312201822		8000000000001001	01/05
5	4	000422103246		8000000000001001	01/05
5	5	000516140506		8000000000001001	01/05
6	1	000103110930		8000000000001001	01/05
6	2	000212062626		8000000000001001	01/05
7	1	000101210131		8000000000001001	01/05
7	2	000222152411		8000000000001001	01/05
7	3	000329065926		8000000000001001	01/05
7	4	000425105420		8000000000001001	01/05
8	1	000110084046		8000000000001001	01/05
9	1	000107134203		8000000000001001	01/05
9	2	000218174017		8000000000001001	01/05
9	3	000326193232		8000000000001001	01/05
9	4	000425183742		8000000000001001	01/05
10	1	000123130042		8000000000001001	01/05
10	2	000224141230		8000000000001001	01/05
10	3	000306112105		8000000000001001	01/05
10	4	000406074238		8000000000001001	01/05
10	5	000508220456		8000000000001001	01/05
10	6	000609103638		8000000000001001	01/05
11	1	000113143733		8000000000001001	01/05
12	1	000108153518		8000000000001001	01/05
12	2	000208170555		8000000000001001	01/05
12	3	000301221936		8000000000001001	01/05
13	1	000102085443		8000000000001001	01/05
13	2	000221180834		8000000000001001	01/05
13	3	000324194018		8000000000001001	01/05
13	4	000408145711		8000000000001001	01/05
15	1	000108231551		8000000000001001	01/05
15	2	000209123945		8000000000001001	01/05
15	3	000309084511		8000000000001001	01/05
15	4	000411115024		8000000000001001	01/05
15	5	000515221440		8000000000001001	01/05
16	1	000124060507		8000000000001001	01/05
17	1	000116225936		8000000000001001	01/05
17	2	000224162656		8000000000001001	01/05
17	3	000310090346		8000000000001001	01/05
17	4	000412170113		8000000000001001	01/05
18	1	000128234041		8000000000001001	01/05
19	1	000126114517		8000000000001001	01/05
19	2	000211184904		8000000000001001	01/05
20	1	000107081729		8000000000001001	01/05
21	1	000103090728		8000000000001001	01/05
21	2	000206130953		8000000000001001	01/05
21	3	000305174920		8000000000001001	01/05
22	1	000107223244		8000000000001001	01/05
22	2	000219084415		8000000000001001	01/05
22	3	000304121845		8000000000001001	01/05
22	4	000404195615		8000000000001001	01/05
23	1	000114060142		8000000000001001	01/05
23	2	000220174040		8000000000001001	01/05
23	3	000321062710		8000000000001001	01/05
23	4	000408161310		8000000000001001	01/05
23	5	000526173357		8000000000001001	01/05
24	1	000119135332		8000000000001001	01/05
24	2	000205170905		8000000000001001	01/05
25	1	000120125523		8000000000001001	01/05
25	2	000203130911		8000000000001001	01/05
25	3	000325225619		8000000000001001	01/05
25	4	000427210859		8000000000001001	01/05
25	5	000516143610		8000000000001001	01/05
26	1	000108070411		8000000000001001	01/05
26	2	000220104742		8000000000001001	01/05
26	3	000312064226		8000000000001001	01/05
26	4	000416075633		8000000000001001	01/05
26	5	000520215357		8000000000001001	01/05
27	1	000121181548		8000000000001001	01/05
27	2	000205193923		8000000000001001	01/05
27	3	000303231747		8000000000001001	01/05
27	4	000416184049		8000000000001001	01/05
27	5	000514233008		8000000000001001	01/05
28	1	000126111315		8000000000001001	01/05
28	2	000223111243		8000000000001001	01/05
28	3	000307121300		8000000000001001	01/05
28	4	000425125351		8000000000001001	01/05
28	5	000522100528		8000000000001001	01/05
28	6	000605185809		8000000000001001	01/05
29	1	000104232928		8000000000001001	01/05
30	1	000112204509		8000000000001001	01/05
30	2	000226075002		8000000000001001	01/05
30	3	000306182216		8000000000001001	01/05
30	4	000422185040		8000000000001001	01/05
30	5	000504163517		8000000000001001	01/05
31	1	000110144256		8000000000001001	01/05
32	1	000120154849		8000000000001001	01/05
32	2	000224123644		8000000000001001	01/05
32	3	000302194533		8000000000001001	01/05
32	4	000417214923		8000000000001001	01/05
32	5	000510211017		8000000000001001	01/05
32	6	000621184645		8000000000001001	01/05
33	1	000113135903		8000000000001001	01/05
33	2	000222060205		8000000000001001	01/05
33	3	000318170044		8000000000001001	01/05
33	4	000427132238		8000000000001001	01/05
33	5	000502202152		8000000000001001	01/05
34	1	000108143640		8000000000001001	01/05
35	1	000128104300		8000000000001001	01/05
35	2	000210063220		8000000000001001	01/05
35	3	000322203953		8000000000001001	01/05
35	4	000429083218		8000000000001001	01/05
36	1	000118073640		8000000000001001	01/05
36	2	000209221927		8000000000001001	01/05
36	3	000322195854		8000000000001001	01/05
36	4	000412144542		8000000000001001	01/05
37	1	000118202213		8000000000001001	01/05
37	2	000226163934		8000000000001001	01/05
37	3	000325061015		8000000000001001	01/05
37	4	000420071632		8000000000001001	01/05
38	1	000107092544		8000000000001001	01/05
39	1	000129214650		8000000000001001	01/05
39	2	000229082710		8000000000001001	01/05
39	3	000320222709		8000000000001001	01/05
39	4	000423085249		8000000000001001	01/05
39	5	000514170052		8000000000001001	01/05
40	1	000117125524		8000000000001001	01/05
40	2	000208194607		8000000000001001	01/05
42	1	000106185457		8000000000001001	01/05
42	2	000205154504		8000000000001001	01/05
42	3	000306231736		8000000000001001	01/05
43	1	000118205745		8000000000001001	01/05
43	2	000202115356		8000000000001001	01/05
43	3	000303062202		8000000000001001	01/05
43	4	000412133648		8000000000001001	01/05
43	5	000520181528		8000000000001001	01/05
44	1	000103061802		8000000000001001	01/05
44	2	000210120505		8000000000001001	01/05
45	1	000125081557		8000000000001001	01/05
46	1	000124122350		8000000000001001	01/05
46	2	000203222333		8000000000001001	01/05
47	1	000128063724		8000000000001001	01/05
48	1	000106124028		8000000000001001	01/05
48	2	000201073939		8000000000001001	01/05
48	3	000303124204		8000000000001001	01/05
48	4	000426124354		8000000000001001	01/05
48	5	000511151048		8000000000001001	01/05
48	6	000605230602		8000000000001001	01/05
49	1	000113143311		8000000000001001	01/05
49	2	000203100554		8000000000001001	01/05
49	3	000306215136		8000000000001001	01/05
49	4	000424084402		8000000000001001	01/05
49	5	000524182704		8000000000001001	01/05
50	1	000124153551		8000000000001001	01/05
50	2	000219101836		8000000000001001	01/05
50	3	000316235156		8000000000001001	01/05
51	1	000102150818		8000000000001001	01/05
51	2	000218100519		8000000000001001	01/05
51	3	000314103637		8000000000001001	01/05
51	4	000412155753		8000000000001001	01/05
51	5	000528200225		8000000000001001	01/05
51	6	000610100718		8000000000001001	01/05
52	1	000128123357		8000000000001001	01/05
52	2	000225095152		8000000000001001	01/05
52	3	000322160641		8000000000001001	01/05
52	4	000409071808		8000000000001001	01/05
52	5	000507171243		8000000000001001	01/05
52	6	000626203527		8000000000001001	01/05
53	1	000103191517		8000000000001001	01/05
53	2	000225153857		8000000000001001	01/05
53	3	000305231025		8000000000001001	01/05
53	4	000421132533		8000000000001001	01/05
53	5	000523091554		8000000000001001	01/05
53	6	000616132220		8000000000001001	01/05
54	1	000108081848		8000000000001001	01/05
54	2	000209143001		8000000000001001	01/05
54	3	000306225259		8000000000001001	01/05
54	4	000403072535		8000000000001001	01/05
55	1	000124063819		8000000000001001	01/05
56	1	000113130330		8000000000001001	01/05
56	2	000214135919		8000000000001001	01/05
57	1	000122062114		8000000000001001	01/05
57	2	000212075947		8000000000001001	01/05
57	3	000304164820		8000000000001001	01/05
57	4	000401122500		8000000000001001	01/05
58	1	000124235101		8000000000001001	01/05
59	1	000101220718		8000000000001001	01/05
59	2	000219140729		8000000000001001	01/05
59	3	000329065252		8000000000001001	01/05
59	4	000412125105		8000000000001001	01/05
59	5	000521091716		8000000000001001	01/05
60	1	000127093528		8000000000001001	01/05
61	1	000121130035		8000000000001001	01/05
61	2	000214153956		8000000000001001	01/05
61	3	000324125821		8000000000001001	01/05
61	4	000408232334		8000000000001001	01/05
62	1	000117075045		8000000000001001	01/05
62	2	000227075232		8000000000001001	01/05
62	3	000326231643		8000000000001001	01/05
62	4	000405140054		8000000000001001	01/05
62	5	000529162056		8000000000001001	01/05
63	1	000106120755		8000000000001001	01/05
63	2	000202172700		8000000000001001	01/05
63	3	000329132426		8000000000001001	01/05
64	1	000117230512		8000000000001001	01/05
64	2	000220160404		8000000000001001	01/05
64	3	000308075113		8000000000001001	01/05
64	4	000409172717		8000000000001001	01/05
64	5	000511134027		8000000000001001	01/05
64	6	000606205528		8000000000001001	01/05
65	1	000122200239		8000000000001001	01/05
65	2	000203213328		8000000000001001	01/05
66	1	000126224440		8000000000001001	01/05
66	2	000229150109		8000000000001001	01/05
66	3	000323233844		8000000000001001	01/05
66	4	000405090939		8000000000001001	01/05
66	5	000516142617		8000000000001001	01/05
67	1	000115221838		8000000000001001	01/05
67	2	000202141714		8000000000001001	01/05
67	3	000313131343		8000000000001001	01/05
67	4	000418100052		8000000000001001	01/05
68	1	000119133327		8000000000001001	01/05
68	2	000224155203		8000000000001001	01/05
68	3	000324130421		8000000000001001	01/05
68	4	000426181138		8000000000001001	01/05
68	5	000516233351		8000000000001001	01/05
68	6	000628101826		8000000000001001	01/05
69	1	000123104738		8000000000001001	01/05
69	2	000217225247		8000000000001001	01/05
70	1	000121091120		8000000000001001	01/05
70	2	000201070057		8000000000001001	01/05
71	1	000113130525		8000000000001001	01/05
71	2	000210194616		8000000000001001	01/05
71	3	000309173143		8000000000001001	01/05
71	4	000421064606		8000000000001001	01/05
71	5	000509184927		8000000000001001	01/05
71	6	000603160116		8000000000001001	01/05
72	1	000118221003		8000000000001001	01/05
72	2	000208114340		8000000000001001	01/05
74	1	000103070849		8000000000001001	01/05
74	2	000218191816		8000000000001001	01/05
74	3	000315182736		8000000000001001	01/05
74	4	000423134322		8000000000001001	01/05
74	5	000503141748		8000000000001001	01/05
75	1	000116091244		8000000000001001	01/05
75	2	000219141722		8000000000001001	01/05
75	3	000313101853		8000000000001001	01/05
75	4	000403071115		8000000000001001	01/05
75	5	000526194610		8000000000001001	01/05
76	1	000111215737		8000000000001001	01/05
76	2	000218111746		8000000000001001	01/05
76	3	000302162623		8000000000001001	01/05
76	4	000412211620		8000000000001001	01/05
76	5	000518072138		8000000000001001	01/05
77	1	000109205728		8000000000001001	01/05
78	1	000110080456		8000000000001001	01/05
78	2	000215172214		8000000000001001	01/05
78	3	000302144955		8000000000001001	01/05
78	4	000426083818		8000000000001001	01/05
78	5	000508075119		8000000000001001	01/05
79	1	000119161703		8000000000001001	01/05
79	2	000204121835		8000000000001001	01/05
79	3	000304125854		8000000000001001	01/05
79	4	000428195805		8000000000001001	01/05
79	5	000511221126		8000000000001001	01/05
80	1	000129203608		8000000000001001	01/05
81	1	000106225851		8000000000001001	01/05
81	2	000219123257		8000000000001001	01/05
81	3	000326135629		8000000000001001	01/05
81	4	000428071915		8000000000001001	01/05
82	1	000111100753		8000000000001001	01/05
82	2	000203064637		8000000000001001	01/05
82	3	000303081639		8000000000001001	01/05
82	4	000422073630		8000000000001001	01/05
83	1	000113160903		8000000000001001	01/05
83	2	000214164633		8000000000001001	01/05
83	3	000323141358		8000000000001001	01/05
83	4	000422172055		8000000000001001	01/05
83	5	000512064337		8000000000001001	01/05
83	6	000616181017		8000000000001001	01/05
84	1	000115063423		8000000000001001	01/05
85	1	000108163727		8000000000001001	01/05
85	2	000216112526		8000000000001001	01/05
86	1	000113065056		8000000000001001	01/05
86	2	000203223132		8000000000001001	01/05
86	3	000328114659		8000000000001001	01/05
87	1	000124170046		8000000000001001	01/05
88	1	000109230545		8000000000001001	01/05
88	2	000210163405		8000000000001001	01/05
88	3	000326200045		8000000000001001	01/05
88	4	000413094311		8000000000001001	01/05
88	5	000516091032		8000000000001001	01/05
88	6	000624103745		8000000000001001	01/05
89	1	000109111448		8000000000001001	01/05
90	1	000102121736		8000000000001001	01/05
90	2	000218212754		8000000000001001	01/05
90	3	000312114628		8000000000001001	01/05
90	4	000419132925		8000000000001001	01/05
90	5	000510120955		8000000000001001	01/05
91	1	000111120148		8000000000001001	01/05
91	2	000201235100		8000000000001001	01/05
92	1	000110093641		8000000000001001	01/05
92	2	000214093528		8000000000001001	01/05
93	1	000127191547		8000000000001001	01/05
93	2	000203070906		8000000000001001	01/05
93	3	000304195057		8000000000001001	01/05
93	4	000416104041		8000000000001001	01/05
93	5	000517164611		8000000000001001	01/05
93	6	000629185038		8000000000001001	01/05
94	1	000102174239		8000000000001001	01/05
94	2	000210075700		8000000000001001	01/05
95	1	000114190745		8000000000001001	01/05
95	2	000206121112		8000000000001001	01/05
95	3	000316130007		8000000000001001	01/05
95	4	000429185246		8000000000001001	01/05
95	5	000506173418		8000000000001001	01/05
95	6	000629221657		8000000000001001	01/05
96	1	000105095821		8000000000001001	01/05
96	2	000218083142		8000000000001001	01/05
96	3	000308231952		8000000000001001	01/05
96	4	000408082836		8000000000001001	01/05
96	5	000514183151		8000000000001001	01/05
97	1	000118111507		8000000000001001	01/05
97	2	000228190552		8000000000001001	01/05
98	1	000118151344		8000000000001001	01/05
98	2	000212195203		8000000000001001	01/05
99	1	000111072906		8000000000001001	01/05
99	2	000227111912		8000000000001001	01/05
99	3	000319222355		8000000000001001	01/05
99	4	000424081654		8000000000001001	01/05
100	1	000128223133		8000000000001001	01/05
100	2	000219131445		8000000000001001	01/05
100	3	000305223707		8000000000001001	01/05
100	4	000412084833		8000000000001001	01/05
100	5	000508172255		8000000000001001	01/05
101	1	000101073255		8000000000001001	01/05
102	1	000128104019		8000000000001001	01/05
103	1	000111222822		8000000000001001	01/05
103	2	000227101537		8000000000001001	01/05
103	3	000319062733		8000000000001001	01/05
103	4	000405152225		8000000000001001	01/05
103	5	000528161749		8000000000001001	01/05
103	6	000618140220		8000000000001001	01/05
104	1	000112074113		8000000000001001	01/05
104	2	000228083621		8000000000001001	01/05
104	3	000318090741		8000000000001001	01/05
104	4	000423071918		8000000000001001	01/05
105	1	000109191935		8000000000001001	01/05
105	2	000215112950		8000000000001001	01/05
105	3	000304164426		8000000000001001	01/05
106	1	000107121716		8000000000001001	01/05
106	2	000202082701		8000000000001001	01/05
107	1	000119070344		8000000000001001	01/05
107	2	000227074541		8000000000001001	01/05
108	1	000110210000		8000000000001001	01/05
108	2	000205135251		8000000000001001	01/05
108	3	000325213010		8000000000001001	01/05
108	4	000419094149		8000000000001001	01/05
108	5	000524140452		8000000000001001	01/05
108	6	000620114111		8000000000001001	01/05
109	1	000126110219		8000000000001001	01/05
109	2	000219184106		8000000000001001	01/05
109	3	000305204801		8000000000001001	01/05
109	4	000429183136		8000000000001001	01/05
110	1	000111193723		8000000000001001	01/05
110	2	000219111742		8000000000001001	01/05
110	3	000307112333		8000000000001001	01/05
110	4	000421114945		8000000000001001	01/05
110	5	000514231151		8000000000001001	01/05
110	6	000614163217		8000000000001001	01/05
111	1	000129175508		8000000000001001	01/05
111	2	000205073432		8000000000001001	01/05
111	3	000318212557		8000000000001001	01/05
111	4	000420191304		8000000000001001	01/05
111	5	000519175918		8000000000001001	01/05
112	1	000123225630		8000000000001001	01/05
112	2	000225093528		8000000000001001	01/05
112	3	000311094510		8000000000001001	01/05
112	4	000427112818		8000000000001001	01/05
112	5	000521234805		8000000000001001	01/05
113	1	000126232519		8000000000001001	01/05
114	1	000116173502		8000000000001001	01/05
114	2	000220221708		8000000000001001	01/05
114	3	000321231108		8000000000001001	01/05
114	4	000412183003		8000000000001001	01/05
115	1	000104061048		8000000000001001	01/05
115	2	000204115355		8000000000001001	01/05
115	3	000310073228		8000000000001001	01/05
116	1	000126134629		8000000000001001	01/05
116	2	000222085329		8000000000001001	01/05
116	3	000308082332		8000000000001001	01/05
117	1	000101073109		8000000000001001	01/05
117	2	000214142349		8000000000001001	01/05
117	3	000302193027		8000000000001001	01/05
118	1	000117153404		8000000000001001	01/05
118	2	000227154954		8000000000001001	01/05
118	3	000308095149		8000000000001001	01/05
118	4	000404081718		8000000000001001	01/05
118	5	000503125256		8000000000001001	01/05
118	6	000618185251		8000000000001001	01/05
119	1	000119214423		8000000000001001	01/05
119	2	000202074345		8000000000001001	01/05
119	3	000309075756		8000000000001001	01/05
119	4	000427074651		8000000000001001	01/05
119	5	000513114624		8000000000001001	01/05
119	6	000622150743		8000000000001001	01/05
120	1	000116183937		8000000000001001	01/05
121	1	000126211959		8000000000001001	01/05
121	2	000213160458		8000000000001001	01/05
121	3	000314075526		8000000000001001	01/05
122	1	000112145605		8000000000001001	01/05
122	2	000213172921		8000000000001001	01/05
122	3	000308213540		8000000000001001	01/05
122	4	000415100058		8000000000001001	01/05
123	1	000117235325		8000000000001001	01/05
123	2	000213111015		8000000000001001	01/05
123	3	000314141720		8000000000001001	01/05
123	4	000414140938		8000000000001001	01/05
123	5	000518122829		8000000000001001	01/05
124	1	000117203212		8000000000001001	01/05
124	2	000214095915		8000000000001001	01/05
125	1	000107231003		8000000000001001	01/05
125	2	000225114623		8000000000001001	01/05
125	3	000316093807		8000000000001001	01/05
125	4	000408060044		8000000000001001	01/05
126	1	000105100043		8000000000001001	01/05
126	2	000206161913		8000000000001001	01/05
126	3	000321142022		8000000000001001	01/05
126	4	000408183713		8000000000001001	01/05
127	1	000104085703		8000000000001001	01/05
127	2	000205210136		8000000000001001	01/05
127	3	000310161158		8000000000001001	01/05
127	4	000407062458		8000000000001001	01/05
128	1	000108094630		8000000000001001	01/05
128	2	000212064428		8000000000001001	01/05
128	3	000321065756		8000000000001001	01/05
128	4	000405135201		8000000000001001	01/05
128	5	000513123937		8000000000001001	01/05
129	1	000108153955		8000000000001001	01/05
129	2	000222074627		8000000000001001	01/05
129	3	000308211239		8000000000001001	01/05
130	1	000118193110		8000000000001001	01/05
130	2	000214133634		8000000000001001	01/05
130	3	000313150701		8000000000001001	01/05
130	4	000411135553		8000000000001001	01/05
130	5	000527090911		8000000000001001	01/05
130	6	000601171957		8000000000001001	01/05
131	1	000126130705		8000000000001001	01/05
131	2	000208133612		8000000000001001	01/05
132	1	000126061627		8000000000001001	01/05
133	1	000106074529		8000000000001001	01/05
133	2	000219180616		8000000000001001	01/05
133	3	000319200138		8000000000001001	01/05
133	4	000410162312		8000000000001001	01/05
133	5	000527123037		8000000000001001	01/05
133	6	000611183109		8000000000001001	01/05
134	1	000104125922		8000000000001001	01/05
134	2	000223121507		8000000000001001	01/05
134	3	000324140812		8000000000001001	01/05
134	4	000401235414		8000000000001001	01/05
134	5	000502132849		8000000000001001	01/05
135	1	000116160819		8000000000001001	01/05
135	2	000217155310		8000000000001001	01/05
135	3	000310073240		8000000000001001	01/05
135	4	000414163802		8000000000001001	01/05
135	5	000516142517		8000000000001001	01/05
135	6	000602155548		8000000000001001	01/05
136	1	000103135450		8000000000001001	01/05
136	2	000223200635		8000000000001001	01/05
136	3	000303100158		8000000000001001	01/05
136	4	000426153653		8000000000001001	01/05
136	5	000502183214		8000000000001001	01/05
136	6	000612074816		8000000000001001	01/05
137	1	000122062450		8000000000001001	01/05
137	2	000204211011		8000000000001001	01/05
137	3	000328182440		8000000000001001	01/05
137	4	000423233956		8000000000001001	01/05
137	5	000520064037		8000000000001001	01/05
138	1	000120111555		8000000000001001	01/05
138	2	000226085627		8000000000001001	01/05
138	3	000319200620		8000000000001001	01/05
138	4	000403130549		8000000000001001	01/05
138	5	000506081714		8000000000001001	01/05
138	6	000617114611		8000000000001001	01/05
139	1	000113231958		8000000000001001	01/05
139	2	000217105029		8000000000001001	01/05
139	3	000303234553		8000000000001001	01/05
139	4	000409191315		8000000000001001	01/05
139	5	000523060455		8000000000001001	01/05
140	1	000108092443		8000000000001001	01/05
141	1	000125201449		8000000000001001	01/05
141	2	000220155723		8000000000001001	01/05
141	3	000315131745		8000000000001001	01/05
142	1	000112231938		8000000000001001	01/05
142	2	000217123903		8000000000001001	01/05
142	3	000311083454		8000000000001001	01/05
142	4	000407092046		8000000000001001	01/05
143	1	000104123317		8000000000001001	01/05
144	1	000113211108		8000000000001001	01/05
144	2	000201192129		8000000000001001	01/05
144	3	000305164109		8000000000001001	01/05
144	4	000417083820		8000000000001001	01/05
144	5	000521085336		8000000000001001	01/05
144	6	000620083734		8000000000001001	01/05
145	1	000126153957		8000000000001001	01/05
145	2	000211085820		8000000000001001	01/05
145	3	000318072128		8000000000001001	01/05
146	1	000125202050		8000000000001001	01/05
146	2	000214230433		8000000000001001	01/05
146	3	000303101417		8000000000001001	01/05
147	1	000127083541		8000000000001001	01/05
147	2	000221134155		8000000000001001	01/05
147	3	000326122344		8000000000001001	01/05
147	4	000417142151		8000000000001001	01/05
149	1	000107134356		8000000000001001	01/05
149	2	000215203829		8000000000001001	01/05
149	3	000329233347		8000000000001001	01/05
150	1	000108112623		8000000000001001	01/05
150	2	000204113053		8000000000001001	01/05
150	3	000327162717		8000000000001001	01/05
150	4	000418142852		8000000000001001	01/05
151	1	000120142824		8000000000001001	01/05
152	1	000127234233		8000000000001001	01/05
152	2	000212154247		8000000000001001	01/05
152	3	000323143317		8000000000001001	01/05
153	1	000115141512		8000000000001001	01/05
153	2	000205121717		8000000000001001	01/05
153	3	000305213324		8000000000001001	01/05
153	4	000403065432		8000000000001001	01/05
154	1	000104093308		8000000000001001	01/05
154	2	000226230826		8000000000001001	01/05
154	3	000327205402		8000000000001001	01/05
155	1	000121082739		8000000000001001	01/05
155	2	000222121918		8000000000001001	01/05
155	3	000315112901		8000000000001001	01/05
155	4	000411155156		8000000000001001	01/05
155	5	000517201308		8000000000001001	01/05
155	6	000615132021		8000000000001001	01/05
156	1	000124100734		8000000000001001	01/05
156	2	000225101812		8000000000001001	01/05
156	3	000307095037		8000000000001001	01/05
156	4	000402175955		8000000000001001	01/05
157	1	000117131909		8000000000001001	01/05
158	1	000102074242		8000000000001001	01/05
158	2	000205075732		8000000000001001	01/05
158	3	000328134839		8000000000001001	01/05
158	4	000409120101		8000000000001001	01/05
158	5	000529203631		8000000000001001	01/05
158	6	000615204148		8000000000001001	01/05
159	1	000126131356		8000000000001001	01/05
159	2	000214162715		8000000000001001	01/05
159	3	000317134340		8000000000001001	01/05
159	4	000418115355		8000000000001001	01/05
160	1	000128212237		8000000000001001	01/05
160	2	000202124322		8000000000001001	01/05
160	3	000329152019		8000000000001001	01/05
161	1	000108140806		8000000000001001	01/05
161	2	000205115428		8000000000001001	01/05
161	3	000318125643		8000000000001001	01/05
161	4	000423184540		8000000000001001	01/05
161	5	000521172909		8000000000001001	01/05
161	6	000620143047		8000000000001001	01/05
162	1	000124180928		8000000000001001	01/05
162	2	000201092530		8000000000001001	01/05
163	1	000101110331		8000000000001001	01/05
163	2	000203120644		8000000000001001	01/05
163	3	000309173300		8000000000001001	01/05
163	4	000417191405		8000000000001001	01/05
163	5	000524184050		8000000000001001	01/05
164	1	000110231145		8000000000001001	01/05
164	2	000223092036		8000000000001001	01/05
164	3	000301184755		8000000000001001	01/05
164	4	000416073052		8000000000001001	01/05
164	5	000526124351		8000000000001001	01/05
164	6	000608131345		8000000000001001	01/05
165	1	000122122918		8000000000001001	01/05
166	1	000106234319		8000000000001001	01/05
166	2	000226135016		8000000000001001	01/05
166	3	000316105513		8000000000001001	01/05
166	4	000419200940		8000000000001001	01/05
166	5	000514152259		8000000000001001	01/05
167	1	000109203612		8000000000001001	01/05
167	2	000224180317		8000000000001001	01/05
167	3	000308173751		8000000000001001	01/05
167	4	000423084115		8000000000001001	01/05
167	5	000528091453		8000000000001001	01/05
167	6	000616141814		8000000000001001	01/05
168	1	000101121811		8000000000001001	01/05
169	1	000114113710		8000000000001001	01/05
169	2	000205083542		8000000000001001	01/05
170	1	000109160235		8000000000001001	01/05
170	2	000209065825		8000000000001001	01/05
170	3	000313232343		8000000000001001	01/05
171	1	000123120012		8000000000001001	01/05
171	2	000223221629		8000000000001001	01/05
172	1	000128110226		8000000000001001	01/05
172	2	000212082517		8000000000001001	01/05
172	3	000324131351		8000000000001001	01/05
172	4	000403230050		8000000000001001	01/05
173	1	000116101324		8000000000001001	01/05
173	2	000226200320		8000000000001001	01/05
173	3	000312071153		8000000000001001	01/05
173	4	000422185735		8000000000001001	01/05
173	5	000503094756		8000000000001001	01/05
173	6	000624173530		8000000000001001	01/05
174	1	000128155510		8000000000001001	01/05
174	2	000217085905		8000000000001001	01/05
174	3	000302063659		8000000000001001	01/05
174	4	000425230049		8000000000001001	01/05
175	1	000108075219		8000000000001001	01/05
175	2	000226205416		8000000000001001	01/05
175	3	000302201618		8000000000001001	01/05
175	4	000412151732		8000000000001001	01/05
176	1	000112191918		8000000000001001	01/05
176	2	000223133340		8000000000001001	01/05
176	3	000304195239		8000000000001001	01/05
176	4	000409210504		8000000000001001	01/05
176	5	000504061130		8000000000001001	01/05
176	6	000609134834		8000000000001001	01/05
177	1	000115184950		8000000000001001	01/05
177	2	000212200857		8000000000001001	01/05
178	1	000101224150		8000000000001001	01/05
178	2	000201222419		8000000000001001	01/05
178	3	000325075452		8000000000001001	01/05
178	4	000401133950		8000000000001001	01/05
179	1	000105061001		8000000000001001	01/05
179	2	000204151745		8000000000001001	01/05
179	3	000307193109		8000000000001001	01/05
179	4	000411160100		8000000000001001	01/05
179	5	000523213909		8000000000001001	01/05
179	6	000605114829		8000000000001001	01/05
180	1	000126152448		8000000000001001	01/05
180	2	000203201654		8000000000001001	01/05
180	3	000319235102		8000000000001001	01/05
180	4	000404084523		8000000000001001	01/05
180	5	000515173353		8000000000001001	01/05
180	6	000605215045		8000000000001001	01/05
181	1	000115093603		8000000000001001	01/05
181	2	000203225731		8000000000001001	01/05
181	3	000303194333		8000000000001001	01/05
181	4	000415211954		8000000000001001	01/05
182	1	000128150055		8000000000001001	01/05
182	2	000203221431		8000000000001001	01/05
182	3	000305113604		8000000000001001	01/05
182	4	000426065052		8000000000001001	01/05
182	5	000525195625		8000000000001001	01/05
182	6	000629190834		8000000000001001	01/05
183	1	000114103754		8000000000001001	01/05
183	2	000204131541		8000000000001001	01/05
183	3	000323221554		8000000000001001	01/05
183	4	000401082008		8000000000001001	01/05
183	5	000518235131		8000000000001001	01/05
184	1	000117133347		8000000000001001	01/05
184	2	000213173921		8000000000001001	01/05
184	3	000317140309		8000000000001001	01/05
184	4	000409171157		8000000000001001	01/05
185	1	000116155540		8000000000001001	01/05
186	1	000116084413		8000000000001001	01/05
186	2	000208065107		8000000000001001	01/05
186	3	000317075703		8000000000001001	01/05
186	4	000413074358		8000000000001001	01/05
186	5	000521151241		8000000000001001	01/05
186	6	000614095152		8000000000001001	01/05
187	1	000105092613		8000000000001001	01/05
187	2	000215100308		8000000000001001	01/05
188	1	000125065353		8000000000001001	01/05
188	2	000229195106		8000000000001001	01/05
188	3	000320155931		8000000000001001	01/05
189	1	000120093525		8000000000001001	01/05
189	2	000213222659		8000000000001001	01/05
189	3	000329130434		8000000000001001	01/05
190	1	000124063422		8000000000001001	01/05
190	2	000221202505		8000000000001001	01/05
191	1	000128174303		8000000000001001	01/05
191	2	000227094841		8000000000001001	01/05
191	3	000307115644		8000000000001001	01/05
191	4	000425101338		8000000000001001	01/05
191	5	000509151931		8000000000001001	01/05
191	6	000606100901		8000000000001001	01/05
192	1	000128233243		8000000000001001	01/05
192	2	000227234157		8000000000001001	01/05
192	3	000325140935		8000000000001001	01/05
192	4	000411205139		8000000000001001	01/05
192	5	000525222547		8000000000001001	01/05
193	1	000101131050		8000000000001001	01/05
193	2	000216143301		8000000000001001	01/05
193	3	000319093026		8000000000001001	01/05
193	4	000422103330		8000000000001001	01/05
193	5	000527173157		8000000000001001	01/05
194	1	000124071406		8000000000001001	01/05
195	1	000114224040		8000000000001001	01/05
195	2	000204090533		8000000000001001	01/05
195	3	000315103752		8000000000001001	01/05
196	1	000127064727		8000000000001001	01/05
196	2	000226091018		8000000000001001	01/05
196	3	000320164927		8000000000001001	01/05
196	4	000417071257		8000000000001001	01/05
196	5	000525235659		8000000000001001	01/05
197	1	000102143307		8000000000001001	01/05
197	2	000208204741		8000000000001001	01/05
197	3	000317180437		8000000000001001	01/05
197	4	000421142758		8000000000001001	01/05
199	1	000117223150		8000000000001001	01/05
199	2	000205165956		8000000000001001	01/05
199	3	000317072005		8000000000001001	01/05
200	1	000129202953		8000000000001001	01/05
200	2	000222094514		8000000000001001	01/05
200	3	000314070326		8000000000001001	01/05
200	4	000407135858		8000000000001001	01/05
200	5	000518204236		8000000000001001	01/05
200	6	000621124656		8000000000001001	01/05
201	1	000110233945		8000000000001001	01/05
201	2	000208060401		8000000000001001	01/05
202	1	000111220407		8000000000001001	01/05
202	2	000223095247		8000000000001001	01/05
202	3	000322125647		8000000000001001	01/05
202	4	000411124501		8000000000001001	01/05
202	5	000528101339		8000000000001001	01/05
202	6	000626144953		8000000000001001	01/05
203	1	000117105110		8000000000001001	01/05
204	1	000111131330		8000000000001001	01/05
204	2	000222232521		8000000000001001	01/05
205	1	000119134848		8000000000001001	01/05
206	1	000113133954		8000000000001001	01/05
207	1	000119164135		8000000000001001	01/05
207	2	000214171345		8000000000001001	01/05
207	3	000322222425		8000000000001001	01/05
207	4	000409221422		8000000000001001	01/05
207	5	000517091404		8000000000001001	01/05
208	1	000129134127		8000000000001001	01/05
208	2	000220154230		8000000000001001	01/05
208	3	000312105106		8000000000001001	01/05
208	4	000402175443		8000000000001001	01/05
208	5	000516135306		8000000000001001	01/05
209	1	000125102128		8000000000001001	01/05
209	2	000228111825		8000000000001001	01/05
210	1	000121212027		8000000000001001	01/05
210	2	000203160424		8000000000001001	01/05
210	3	000306074952		8000000000001001	01/05
211	1	000108154615		8000000000001001	01/05
212	1	000117152831		8000000000001001	01/05
213	1	000124092554		8000000000001001	01/05
213	2	000215065701		8000000000001001	01/05
213	3	000329061147		8000000000001001	01/05
214	1	000105163029		8000000000001001	01/05
214	2	000211092908		8000000000001001	01/05
214	3	000301083805		8000000000001001	01/05
214	4	000418084807		8000000000001001	01/05
215	1	000118100342		8000000000001001	01/05
215	2	000225195057		8000000000001001	01/05
215	3	000314214103		8000000000001001	01/05
215	4	000428115057		8000000000001001	01/05
215	5	000505081156		8000000000001001	01/05
215	6	000621210926		8000000000001001	01/05
216	1	000124062509		8000000000001001	01/05
217	1	000122235250		8000000000001001	01/05
217	2	000223145954		8000000000001001	01/05
217	3	000318062628		8000000000001001	01/05
217	4	000410092119		8000000000001001	01/05
217	5	000519125310		8000000000001001	01/05
217	6	000605180544		8000000000001001	01/05
218	1	000127105017		8000000000001001	01/05
218	2	000228200539		8000000000001001	01/05
218	3	000315230814		8000000000001001	01/05
218	4	000410094115		8000000000001001	01/05
218	5	000528144127		8000000000001001	01/05
218	6	000607233749		8000000000001001	01/05
219	1	000128103847		8000000000001001	01/05
219	2	000207070913		8000000000001001	01/05
219	3	000315123119		8000000000001001	01/05
219	4	000417062544		8000000000001001	01/05
219	5	000506235332		8000000000001001	01/05
220	1	000125214929		8000000000001001	01/05
220	2	000224224331		8000000000001001	01/05
221	1	000118175850		8000000000001001	01/05
221	2	000218072425		8000000000001001	01/05
221	3	000329201342		8000000000001001	01/05
221	4	000428142322		8000000000001001	01/05
221	5	000507063426		8000000000001001	01/05
221	6	000615230552		8000000000001001	01/05
222	1	000101213019		8000000000001001	01/05
223	1	000127094615		8000000000001001	01/05
224	1	000104181349		8000000000001001	01/05
224	2	000211105934		8000000000001001	01/05
225	1	000110140007		8000000000001001	01/05
226	1	000113120725		8000000000001001	01/05
226	2	000202234056		8000000000001001	01/05
226	3	000324171712		8000000000001001	01/05
227	1	000101080720		8000000000001001	01/05
227	2	000203161542		8000000000001001	01/05
227	3	000317075631		8000000000001001	01/05
227	4	000415224939		8000000000001001	01/05
228	1	000121170203		8000000000001001	01/05
229	1	000102162741		8000000000001001	01/05
229	2	000212215028		8000000000001001	01/05
229	3	000328161100		8000000000001001	01/05
230	1	000125133141		8000000000001001	01/05
230	2	000202090550		8000000000001001	01/05
230	3	000305135738		8000000000001001	01/05
230	4	000417062412		8000000000001001	01/05
231	1	000126103922		8000000000001001	01/05
231	2	000222101613		8000000000001001	01/05
231	3	000327124342		8000000000001001	01/05
231	4	000416140055		8000000000001001	01/05
231	5	000503193201		8000000000001001	01/05
231	6	000626173409		8000000000001001	01/05
232	1	000117094116		8000000000001001	01/05
232	2	000227225432		8000000000001001	01/05
232	3	000320152951		8000000000001001	01/05
232	4	000403144824		8000000000001001	01/05
233	1	000126145815		8000000000001001	01/05
233	2	000207061717		8000000000001001	01/05
233	3	000310223017		8000000000001001	01/05
233	4	000405154234		8000000000001001	01/05
234	1	000111140926		8000000000001001	01/05
234	2	000202220327		8000000000001001	01/05
235	1	000103070258		8000000000001001	01/05
235	2	000212194438		8000000000001001	01/05
235	3	000328091431		8000000000001001	01/05
235	4	000420191826		8000000000001001	01/05
236	1	000102145517		8000000000001001	01/05
236	2	000201200928		8000000000001001	01/05
236	3	000307155456		8000000000001001	01/05
236	4	000413221135		8000000000001001	01/05
236	5	000501131614		8000000000001001	01/05
236	6	000629160237		8000000000001001	01/05
237	1	000112143538		8000000000001001	01/05
238	1	000113090647		8000000000001001	01/05
238	2	000202072622		8000000000001001	01/05
239	1	000107083525		8000000000001001	01/05
240	1	000106140409		8000000000001001	01/05
240	2	000206172021		8000000000001001	01/05
240	3	000308205412		8000000000001001	01/05
240	4	000408131050		8000000000001001	01/05
240	5	000514091231		8000000000001001	01/05
240	6	000617174644		8000000000001001	01/05
241	1	000114155223		8000000000001001	01/05
241	2	000210113241		8000000000001001	01/05
241	3	000315092828		8000000000001001	01/05
241	4	000403213224		8000000000001001	01/05
241	5	000517185203		8000000000001001	01/05
241	6	000612092647		8000000000001001	01/05
242	1	000112071729		8000000000001001	01/05
242	2	000206124046		8000000000001001	01/05
242	3	000311111424		8000000000001001	01/05
242	4	000404124313		8000000000001001	01/05
242	5	000515180851		8000000000001001	01/05
242	6	000623071709		8000000000001001	01/05
243	1	000121213508		8000000000001001	01/05
243	2	000208191604		8000000000001001	01/05
244	1	000110094021		8000000000001001	01/05
244	2	000205204426		8000000000001001	01/05
244	3	000313152706		8000000000001001	01/05
244	4	000419164223		8000000000001001	01/05
244	5	000516110157		8000000000001001	01/05
245	1	000117154758		8000000000001001	01/05
245	2	000202172323		8000000000001001	01/05
245	3	000323201521		8000000000001001	01/05
245	4	000420152239		8000000000001001	01/05
245	5	000513062546		8000000000001001	01/05
245	6	000607205405		8000000000001001	01/05
246	1	000112151509		8000000000001001	01/05
246	2	000226172927		8000000000001001	01/05
246	3	000304060530		8000000000001001	01/05
247	1	000123193044		8000000000001001	01/05
247	2	000211173724		8000000000001001	01/05
247	3	000314104803		8000000000001001	01/05
248	1	000111181227		8000000000001001	01/05
249	1	000108072119		8000000000001001	01/05
249	2	000207112040		8000000000001001	01/05
249	3	000320181014		8000000000001001	01/05
249	4	000426080127		8000000000001001	01/05
249	5	000512165810		8000000000001001	01/05
249	6	000620160521		8000000000001001	01/05
250	1	000102230119		8000000000001001	01/05
250	2	000219191713		8000000000001001	01/05
250	3	000321144155		8000000000001001	01/05
250	4	000426091210		8000000000001001	01/05
250	5	000505090127		8000000000001001	01/05
250	6	000619070114		8000000000001001	01/05
251	1	000118135425		8000000000001001	01/05
251	2	000202063452		8000000000001001	01/05
251	3	000305214239		8000000000001001	01/05
251	4	000426104057		8000000000001001	01/05
251	5	000502223745		8000000000001001	01/05
251	6	000628142450		8000000000001001	01/05
252	1	000101132125		8000000000001001	01/05
252	2	000226214327		8000000000001001	01/05
252	3	000309140939		8000000000001001	01/05
252	4	000407150220		8000000000001001	01/05
252	5	000508155043		8000000000001001	01/05
253	1	000129182328		8000000000001001	01/05
253	2	000207083344		8000000000001001	01/05
253	3	000315123953		8000000000001001	01/05
253	4	000402070656		8000000000001001	01/05
253	5	000516180455		8000000000001001	01/05
254	1	000104165404		8000000000001001	01/05
254	2	000214093208		8000000000001001	01/05
254	3	000319184407		8000000000001001	01/05
254	4	000424164004		8000000000001001	01/05
255	1	000102234656		8000000000001001	01/05
255	2	000203153842		8000000000001001	01/05
255	3	000310155857		8000000000001001	01/05
255	4	000405174937		8000000000001001	01/05
255	5	000505224024		8000000000001001	01/05
255	6	000622063855		8000000000001001	01/05
256	1	000123211753		8000000000001001	01/05
256	2	000220090652		8000000000001001	01/05
256	3	000326154534		8000000000001001	01/05
256	4	000428222951		8000000000001001	01/05
256	5	000520184921		8000000000001001	01/05
257	1	000106154334		8000000000001001	01/05
257	2	000224164131		8000000000001001	01/05
257	3	000323131154		8000000000001001	01/05
257	4	000418163838		8000000000001001	01/05
257	5	000519152123		8000000000001001	01/05
257	6	000608071750		8000000000001001	01/05
258	1	000102214752		8000000000001001	01/05
258	2	000206214802		8000000000001001	01/05
259	1	000116131124		8000000000001001	01/05
259	2	000203144934		8000000000001001	01/05
259	3	000309142412		8000000000001001	01/05
259	4	000414181242		8000000000001001	01/05
259	5	000501064237		8000000000001001	01/05
259	6	000622110104		8000000000001001	01/05
260	1	000105093040		8000000000001001	01/05
260	2	000211203140		8000000000001001	01/05
260	3	000324175119		8000000000001001	01/05
260	4	000412130611		8000000000001001	01/05
261	1	000115104411		8000000000001001	01/05
261	2	000219210449		8000000000001001	01/05
262	1	000113151936		8000000000001001	01/05
262	2	000229152848		8000000000001001	01/05
263	1	000111144818		8000000000001001	01/05
263	2	000215132817		8000000000001001	01/05
263	3	000304113737		8000000000001001	01/05
263	4	000421205021		8000000000001001	01/05
263	5	000517211901		8000000000001001	01/05
264	1	000103124943		8000000000001001	01/05
264	2	000226215213		8000000000001001	01/05
264	3	000301114530		8000000000001001	01/05
265	1	000122103904		8000000000001001	01/05
265	2	000225112532		8000000000001001	01/05
265	3	000327153252		8000000000001001	01/05
265	4	000424063310		8000000000001001	01/05
265	5	000519155108		8000000000001001	01/05
266	1	000101061356		8000000000001001	01/05
267	1	000123062043		8000000000001001	01/05
267	2	000226072801		8000000000001001	01/05
268	1	000114142310		8000000000001001	01/05
268	2	000215203347		8000000000001001	01/05
268	3	000327154020		8000000000001001	01/05
269	1	000108062620		8000000000001001	01/05
269	2	000212095655		8000000000001001	01/05
270	1	000121112902		8000000000001001	01/05
270	2	000213195108		8000000000001001	01/05
271	1	000123080613		8000000000001001	01/05
272	1	000129182256		8000000000001001	01/05
272	2	000225170759		8000000000001001	01/05
272	3	000316173855		8000000000001001	01/05
272	4	000410231726		8000000000001001	01/05
272	5	000525200920		8000000000001001	01/05
272	6	000622090740		8000000000001001	01/05
273	1	000125112911		8000000000001001	01/05
273	2	000224082824		8000000000001001	01/05
273	3	000324074328		8000000000001001	01/05
274	1	000114142811		8000000000001001	01/05
274	2	000212202001		8000000000001001	01/05
274	3	000311193521		8000000000001001	01/05
274	4	000409202556		8000000000001001	01/05
275	1	000106112431		8000000000001001	01/05
275	2	000225155847		8000000000001001	01/05
276	1	000124235121		8000000000001001	01/05
276	2	000209213247		8000000000001001	01/05
276	3	000324181331		8000000000001001	01/05
276	4	000405090718		8000000000001001	01/05
276	5	000508200048		8000000000001001	01/05
277	1	000111175140		8000000000001001	01/05
277	2	000220073527		8000000000001001	01/05
277	3	000318213339		8000000000001001	01/05
278	1	000116100556		8000000000001001	01/05
278	2	000206081017		8000000000001001	01/05
278	3	000311105321		8000000000001001	01/05
278	4	000427122246		8000000000001001	01/05
278	5	000505094907		8000000000001001	01/05
279	1	000101225131		8000000000001001	01/05
279	2	000208191825		8000000000001001	01/05
279	3	000322150636		8000000000001001	01/05
279	4	000429181914		8000000000001001	01/05
279	5	000529190527		8000000000001001	01/05
280	1	000105210716		8000000000001001	01/05
280	2	000203191355		8000000000001001	01/05
281	1	000128195938		8000000000001001	01/05
281	2	000209105918		8000000000001001	01/05
281	3	000323184118		8000000000001001	01/05
281	4	000404231505		8000000000001001	01/05
281	5	000504061549		8000000000001001	01/05
281	6	000607200433		8000000000001001	01/05
282	1	000106171341		8000000000001001	01/05
282	2	000217113607		8000000000001001	01/05
282	3	000310222014		8000000000001001	01/05
282	4	000406071147		8000000000001001	01/05
282	5	000527121635		8000000000001001	01/05
282	6	000615092141		8000000000001001	01/05
283	1	000126192654		8000000000001001	01/05
283	2	000223135419		8000000000001001	01/05
283	3	000329191724		8000000000001001	01/05
283	4	000417222526		8000000000001001	01/05
283	5	000509201932		8000000000001001	01/05
284	1	000122080410		8000000000001001	01/05
284	2	000201151339		8000000000001001	01/05
285	1	000112212531		8000000000001001	01/05
285	2	000212221803		8000000000001001	01/05
286	1	000116182721		8000000000001001	01/05
287	1	000106144812		8000000000001001	01/05
288	1	000117062234		8000000000001001	01/05
288	2	000227134201		8000000000001001	01/05
288	3	000312082909		8000000000001001	01/05
288	4	000412162758		8000000000001001	01/05
288	5	000501221754		8000000000001001	01/05
289	1	000120174439		8000000000001001	01/05
290	1	000129105316		8000000000001001	01/05
290	2	000211084900		8000000000001001	01/05
291	1	000106181226		8000000000001001	01/05
291	2	000208133944		8000000000001001	01/05
291	3	000316111230		8000000000001001	01/05
292	1	000115130313		8000000000001001	01/05
292	2	000202105812		8000000000001001	01/05
292	3	000304065410		8000000000001001	01/05
292	4	000401164541		8000000000001001	01/05
293	1	000117235611		8000000000001001	01/05
293	2	000223102346		8000000000001001	01/05
293	3	000309204551		8000000000001001	01/05
293	4	000418074217		8000000000001001	01/05
293	5	000522130333		8000000000001001	01/05
294	1	000103221240		8000000000001001	01/05
295	1	000118211138		8000000000001001	01/05
296	1	000122185711		8000000000001001	01/05
296	2	000201100811		8000000000001001	01/05
296	3	000313212252		8000000000001001	01/05
297	1	000109063303		8000000000001001	01/05
297	2	000226162401		8000000000001001	01/05
298	1	000104143709		8000000000001001	01/05
298	2	000212091748		8000000000001001	01/05
299	1	000101110443		8000000000001001	01/05
300	1	000104155031		8000000000001001	01/05
301	1	000101231704		8000000000001001	01/05
301	2	000217152204		8000000000001001	01/05
301	3	000311110651		8000000000001001	01/05
301	4	000408171538		8000000000001001	01/05
301	5	000514203149		8000000000001001	01/05
302	1	000119162208		8000000000001001	01/05
303	1	000107151045		8000000000001001	01/05
304	1	000120084740		8000000000001001	01/05
304	2	000218170009		8000000000001001	01/05
304	3	000306154559		8000000000001001	01/05
304	4	000411195353		8000000000001001	01/05
304	5	000511185733		8000000000001001	01/05
304	6	000611092844		8000000000001001	01/05
305	1	000102070935		8000000000001001	01/05
305	2	000216092454		8000000000001001	01/05
305	3	000327175313		8000000000001001	01/05
306	1	000123101251		8000000000001001	01/05
307	1	000110231743		8000000000001001	01/05
307	2	000219205605		8000000000001001	01/05
307	3	000324121416		8000000000001001	01/05
308	1	000116233520		8000000000001001	01/05
308	2	000216095853		8000000000001001	01/05
309	1	000110071208		8000000000001001	01/05
309	2	000203145459		8000000000001001	01/05
310	1	000104164312		8000000000001001	01/05
310	2	000207170441		8000000000001001	01/05
310	3	000313112533		8000000000001001	01/05
310	4	000410133413		8000000000001001	01/05
311	1	000113182330		8000000000001001	01/05
311	2	000201224514		8000000000001001	01/05
311	3	000313195148		8000000000001001	01/05
312	1	000129194348		8000000000001001	01/05
313	1	000124225018		8000000000001001	01/05
313	2	000206200659		8000000000001001	01/05
313	3	000318075022		8000000000001001	01/05
313	4	000421185718		8000000000001001	01/05
313	5	000523224300		8000000000001001	01/05
313	6	000629214505		8000000000001001	01/05
314	1	000106224550		8000000000001001	01/05
314	2	000225231858		8000000000001001	01/05
314	3	000319063326		8000000000001001	01/05
314	4	000429082214		8000000000001001	01/05
314	5	000515122813		8000000000001001	01/05
314	6	000602093912		8000000000001001	01/05
315	1	000107063927		8000000000001001	01/05
315	2	000215120024		8000000000001001	01/05
315	3	000306175025		8000000000001001	01/05
315	4	000413190330		8000000000001001	01/05
315	5	000523174916		8000000000001001	01/05
316	1	000110130506		8000000000001001	01/05
316	2	000213100330		8000000000001001	01/05
316	3	000324195954		8000000000001001	01/05
316	4	000426201423		8000000000001001	01/05
317	1	000126205535		8000000000001001	01/05
317	2	000205151958		8000000000001001	01/05
318	1	000112080031		8000000000001001	01/05
319	1	000108074251		8000000000001001	01/05
319	2	000219104446		8000000000001001	01/05
320	1	000108184406		8000000000001001	01/05
320	2	000219132757		8000000000001001	01/05
320	3	000319064818		8000000000001001	01/05
320	4	000408221252		8000000000001001	01/05
321	1	000104185749		8000000000001001	01/05
321	2	000217225556		8000000000001001	01/05
321	3	000303164723		8000000000001001	01/05
321	4	000403135020		8000000000001001	01/05
321	5	000525123729		8000000000001001	01/05
322	1	000126134912		8000000000001001	01/05
322	2	000203101125		8000000000001001	01/05
322	3	000327080747		8000000000001001	01/05
322	4	000404230952		8000000000001001	01/05
322	5	000523073455		8000000000001001	01/05
322	6	000620161429		8000000000001001	01/05
323	1	000105161414		8000000000001001	01/05
323	2	000225131639		8000000000001001	01/05
323	3	000320120549		8000000000001001	01/05
323	4	000409190632		8000000000001001	01/05
324	1	000101170123		8000000000001001	01/05
325	1	000111190326		8000000000001001	01/05
325	2	000229235950		8000000000001001	01/05
325	3	000317182305		8000000000001001	01/05
325	4	000418220209		8000000000001001	01/05
325	5	000512093801		8000000000001001	01/05
325	6	000610153341		8000000000001001	01/05
326	1	000124170359		8000000000001001	01/05
326	2	000218082320		8000000000001001	01/05
326	3	000303191653		8000000000001001	01/05
326	4	000402134738		8000000000001001	01/05
327	1	000114073302		8000000000001001	01/05
327	2	000221234106		8000000000001001	01/05
328	1	000122221346		8000000000001001	01/05
328	2	000227123813		8000000000001001	01/05
328	3	000303110747		8000000000001001	01/05
328	4	000428101057		8000000000001001	01/05
328	5	000514214835		8000000000001001	01/05
328	6	000623115015		8000000000001001	01/05
329	1	000105173439		8000000000001001	01/05
329	2	000215132845		8000000000001001	01/05
329	3	000318221739		8000000000001001	01/05
329	4	000402142444		8000000000001001	01/05
329	5	000510225636		8000000000001001	01/05
329	6	000625081752		8000000000001001	01/05
330	1	000123095348		8000000000001001	01/05
330	2	000206082017		8000000000001001	01/05
330	3	000325234845		8000000000001001	01/05
330	4	000414120254		8000000000001001	01/05
330	5	000517211724		8000000000001001	01/05
331	1	000110070846		8000000000001001	01/05
331	2	000204111231		8000000000001001	01/05
331	3	000311113754		8000000000001001	01/05
331	4	000411093733		8000000000001001	01/05
331	5	000514164355		8000000000001001	01/05
332	1	000113111149		8000000000001001	01/05
332	2	000225114825		8000000000001001	01/05
332	3	000320120226		8000000000001001	01/05
332	4	000417145805		8000000000001001	01/05
333	1	000124181606		8000000000001001	01/05
333	2	000228121311		8000000000001001	01/05
333	3	000325064939		8000000000001001	01/05
333	4	000429192239		8000000000001001	01/05
334	1	000114215603		8000000000001001	01/05
334	2	000217060420		8000000000001001	01/05
334	3	000329140536		8000000000001001	01/05
334	4	000403092540		8000000000001001	01/05
335	1	000115073804		8000000000001001	01/05
335	2	000221085529		8000000000001001	01/05
335	3	000325135031		8000000000001001	01/05
336	1	000127112846		8000000000001001	01/05
336	2	000226184805		8000000000001001	01/05
336	3	000302224737		8000000000001001	01/05
337	1	000111203720		8000000000001001	01/05
337	2	000227111241		8000000000001001	01/05
337	3	000329111543		8000000000001001	01/05
337	4	000423100207		8000000000001001	01/05
338	1	000124104316		8000000000001001	01/05
338	2	000224225012		8000000000001001	01/05
338	3	000320121652		8000000000001001	01/05
338	4	000421125731		8000000000001001	01/05
338	5	000529095157		8000000000001001	01/05
338	6	000624141909		8000000000001001	01/05
339	1	000102112807		8000000000001001	01/05
339	2	000226102659		8000000000001001	01/05
339	3	000325070732		8000000000001001	01/05
339	4	000421080508		8000000000001001	01/05
340	1	000118194202		8000000000001001	01/05
340	2	000223154053		8000000000001001	01/05
340	3	000320234517		8000000000001001	01/05
340	4	000429070546		8000000000001001	01/05
340	5	000503112847		8000000000001001	01/05
341	1	000114095632		8000000000001001	01/05
342	1	000125164819		8000000000001001	01/05
342	2	000203143401		8000000000001001	01/05
342	3	000319103706		8000000000001001	01/05
343	1	000112234226		8000000000001001	01/05
343	2	000229081212		8000000000001001	01/05
343	3	000318102604		8000000000001001	01/05
344	1	000104162633		8000000000001001	01/05
344	2	000216171636		8000000000001001	01/05
344	3	000318190656		8000000000001001	01/05
345	1	000118100715		8000000000001001	01/05
345	2	000206105252		8000000000001001	01/05
345	3	000309064537		8000000000001001	01/05
346	1	000127232058		8000000000001001	01/05
347	1	000107172541		8000000000001001	01/05
347	2	000204234245		8000000000001001	01/05
347	3	000324065330		8000000000001001	01/05
347	4	000415220437		8000000000001001	01/05
347	5	000515062717		8000000000001001	01/05
348	1	000122155821		8000000000001001	01/05
348	2	000218123954		8000000000001001	01/05
348	3	000327161112		8000000000001001	01/05
348	4	000419061720		8000000000001001	01/05
349	1	000128063352		8000000000001001	01/05
349	2	000220150349		8000000000001001	01/05
349	3	000326075621		8000000000001001	01/05
350	1	000104163628		8000000000001001	01/05
350	2	000210094323		8000000000001001	01/05
350	3	000311152417		8000000000001001	01/05
350	4	000413185354		8000000000001001	01/05
350	5	000523090809		8000000000001001	01/05
351	1	000104141400		8000000000001001	01/05
352	1	000121120229		8000000000001001	01/05
352	2	000203091802		8000000000001001	01/05
352	3	000328143239		8000000000001001	01/05
353	1	000114135213		8000000000001001	01/05
353	2	000206151606		8000000000001001	01/05
353	3	000329160227		8000000000001001	01/05
354	1	000126100007		8000000000001001	01/05
355	1	000106205818		8000000000001001	01/05
355	2	000203131318		8000000000001001	01/05
355	3	000326182305		8000000000001001	01/05
355	4	000405142706		8000000000001001	01/05
356	1	000118121519		8000000000001001	01/05
357	1	000102081930		8000000000001001	01/05
358	1	000121153557		8000000000001001	01/05
359	1	000108125015		8000000000001001	01/05
359	2	000220162736		8000000000001001	01/05
359	3	000303081719		8000000000001001	01/05
359	4	000401112342		8000000000001001	01/05
359	5	000511165926		8000000000001001	01/05
359	6	000613121849		8000000000001001	01/05
362	1	000124150904		8000000000001001	01/05
362	2	000208151434		8000000000001001	01/05
362	3	000324161136		8000000000001001	01/05
363	1	000102234837		8000000000001001	01/05
363	2	000225192525		8000000000001001	01/05
363	3	000325201329		8000000000001001	01/05
364	1	000120120817		8000000000001001	01/05
364	2	000215090022		8000000000001001	01/05
364	3	000317201746		8000000000001001	01/05
364	4	000416161430		8000000000001001	01/05
365	1	000115060418		8000000000001001	01/05
366	1	000122205534		8000000000001001	01/05
366	2	000221091902		8000000000001001	01/05
366	3	000328111126		8000000000001001	01/05
366	4	000422220504		8000000000001001	01/05
367	1	000120232557		8000000000001001	01/05
367	2	000214083033		8000000000001001	01/05
367	3	000304074433		8000000000001001	01/05
368	1	000118094608		8000000000001001	01/05
369	1	000113224409		8000000000001001	01/05
369	2	000222080159		8000000000001001	01/05
369	3	000301174725		8000000000001001	01/05
369	4	000417170437		8000000000001001	01/05
369	5	000502103734		8000000000001001	01/05
370	1	000118073013		8000000000001001	01/05
371	1	000121121014		8000000000001001	01/05
371	2	000209124922		8000000000001001	01/05
371	3	000302174450		8000000000001001	01/05
371	4	000410212629		8000000000001001	01/05
371	5	000521145617		8000000000001001	01/05
371	6	000610104740		8000000000001001	01/05
372	1	000103153912		8000000000001001	01/05
372	2	000211144338		8000000000001001	01/05
373	1	000120231152		8000000000001001	01/05
373	2	000225120713		8000000000001001	01/05
373	3	000328104146		8000000000001001	01/05
374	1	000104141314		8000000000001001	01/05
375	1	000122200501		8000000000001001	01/05
375	2	000227101056		8000000000001001	01/05
376	1	000119120027		8000000000001001	01/05
377	1	000114090057		8000000000001001	01/05
377	2	000210135840		8000000000001001	01/05
377	3	000311152612		8000000000001001	01/05
377	4	000415205950		8000000000001001	01/05
377	5	000523181500		8000000000001001	01/05
378	1	000124134746		8000000000001001	01/05
378	2	000211091358		8000000000001001	01/05
378	3	000326101449		8000000000001001	01/05
378	4	000428225856		8000000000001001	01/05
379	1	000122172312		8000000000001001	01/05
379	2	000224215344		8000000000001001	01/05
379	3	000314084312		8000000000001001	01/05
379	4	000415090626		8000000000001001	01/05
379	5	000515192537		8000000000001001	01/05
380	1	000118145814		8000000000001001	01/05
381	1	000120071904		8000000000001001	01/05
381	2	000211191651		8000000000001001	01/05
381	3	000319151819		8000000000001001	01/05
381	4	000416150233		8000000000001001	01/05
382	1	000112190859		8000000000001001	01/05
383	1	000126222123		8000000000001001	01/05
383	2	000201142735		8000000000001001	01/05
383	3	000316202513		8000000000001001	01/05
383	4	000407222236		8000000000001001	01/05
384	1	000115094617		8000000000001001	01/05
384	2	000214160423		8000000000001001	01/05
384	3	000314060409		8000000000001001	01/05
384	4	000406233808		8000000000001001	01/05
384	5	000519142359		8000000000001001	01/05
385	1	000123155555		8000000000001001	01/05
385	2	000226193222		8000000000001001	01/05
385	3	000317174747		8000000000001001	01/05
385	4	000419080145		8000000000001001	01/05
386	1	000121232855		8000000000001001	01/05
386	2	000216185529		8000000000001001	01/05
386	3	000315181835		8000000000001001	01/05
387	1	000128100702		8000000000001001	01/05
387	2	000206135826		8000000000001001	01/05
387	3	000307170409		8000000000001001	01/05
387	4	000416175749		8000000000001001	01/05
387	5	000505194729		8000000000001001	01/05
388	1	000127220526		8000000000001001	01/05
388	2	000206191959		8000000000001001	01/05
388	3	000320064601		8000000000001001	01/05
388	4	000405102128		8000000000001001	01/05
389	1	000113114737		8000000000001001	01/05
389	2	000210131917		8000000000001001	01/05
389	3	000302220056		8000000000001001	01/05
389	4	000413191925		8000000000001001	01/05
389	5	000514123241		8000000000001001	01/05
389	6	000623063502		8000000000001001	01/05
390	1	000107214950		8000000000001001	01/05
390	2	000226175939		8000000000001001	01/05
391	1	000116080421		8000000000001001	01/05
391	2	000227155828		8000000000001001	01/05
391	3	000316100442		8000000000001001	01/05
391	4	000421195213		8000000000001001	01/05
391	5	000520180110		8000000000001001	01/05
392	1	000114091037		8000000000001001	01/05
392	2	000209211135		8000000000001001	01/05
392	3	000317085831		8000000000001001	01/05
393	1	000120153132		8000000000001001	01/05
394	1	000110204112		8000000000001001	01/05
394	2	000227100828		8000000000001001	01/05
394	3	000304131725		8000000000001001	01/05
395	1	000123142810		8000000000001001	01/05
396	1	000116202910		8000000000001001	01/05
396	2	000224165954		8000000000001001	01/05
396	3	000313134838		8000000000001001	01/05
396	4	000418071624		8000000000001001	01/05
397	1	000120195545		8000000000001001	01/05
397	2	000218102017		8000000000001001	01/05
397	3	000326230432		8000000000001001	01/05
397	4	000423080939		8000000000001001	01/05
397	5	000512102300		8000000000001001	01/05
398	1	000109070757		8000000000001001	01/05
398	2	000221215929		8000000000001001	01/05
398	3	000314113400		8000000000001001	01/05
399	1	000128115748		8000000000001001	01/05
400	1	000120181452		8000000000001001	01/05
400	2	000220092148		8000000000001001	01/05
400	3	000318065216		8000000000001001	01/05
400	4	000422145905		8000000000001001	01/05
400	5	000525115307		8000000000001001	01/05
401	1	000123235402		8000000000001001	01/05
402	1	000103074835		8000000000001001	01/05
402	2	000221152434		8000000000001001	01/05
403	1	000120173539		8000000000001001	01/05
404	1	000108110723		8000000000001001	01/05
404	2	000226062132		8000000000001001	01/05
405	1	000124102529		8000000000001001	01/05
405	2	000229154454		8000000000001001	01/05
405	3	000314232849		8000000000001001	01/05
405	4	000418184503		8000000000001001	01/05
406	1	000120075539		8000000000001001	01/05
406	2	000217074643		8000000000001001	01/05
406	3	000324080221		8000000000001001	01/05
406	4	000410231559		8000000000001001	01/05
406	5	000515125107		8000000000001001	01/05
407	1	000109205531		8000000000001001	01/05
408	1	000114111957		8000000000001001	01/05
408	2	000214222622		8000000000001001	01/05
408	3	000305194108		8000000000001001	01/05
408	4	000416072357		8000000000001001	01/05
409	1	000121170049		8000000000001001	01/05
409	2	000224205646		8000000000001001	01/05
409	3	000321104942		8000000000001001	01/05
409	4	000408083953		8000000000001001	01/05
409	5	000524113830		8000000000001001	01/05
409	6	000616203410		8000000000001001	01/05
412	1	000108213447		8000000000001001	01/05
412	2	000208225417		8000000000001001	01/05
413	1	000105233449		8000000000001001	01/05
413	2	000204210301		8000000000001001	01/05
413	3	000329231340		8000000000001001	01/05
413	4	000411092556		8000000000001001	01/05
413	5	000507153510		8000000000001001	01/05
414	1	000123080820		8000000000001001	01/05
414	2	000226103625		8000000000001001	01/05
415	1	000109124223		8000000000001001	01/05
415	2	000224105411		8000000000001001	01/05
416	1	000116181315		8000000000001001	01/05
417	1	000118125438		8000000000001001	01/05
417	2	000220060420		8000000000001001	01/05
417	3	000315160605		8000000000001001	01/05
417	4	000410072850		8000000000001001	01/05
417	5	000516132310		8000000000001001	01/05
417	6	000608145321		8000000000001001	01/05
418	1	000117134859		8000000000001001	01/05
418	2	000201221301		8000000000001001	01/05
418	3	000329212031		8000000000001001	01/05
418	4	000426065507		8000000000001001	01/05
418	5	000508062912		8000000000001001	01/05
418	6	000602081427		8000000000001001	01/05
419	1	000120112656		8000000000001001	01/05
420	1	000103165646		8000000000001001	01/05
420	2	000215171626		8000000000001001	01/05
421	1	000124161748		8000000000001001	01/05
421	2	000217065101		8000000000001001	01/05
421	3	000306175959		8000000000001001	01/05
421	4	000417105207		8000000000001001	01/05
421	5	000515161041		8000000000001001	01/05
421	6	000628121430		8000000000001001	01/05
422	1	000114113031		8000000000001001	01/05
422	2	000219124305		8000000000001001	01/05
422	3	000307154220		8000000000001001	01/05
422	4	000408202418		8000000000001001	01/05
422	5	000522153526		8000000000001001	01/05
422	6	000622094508		8000000000001001	01/05
423	1	000117213014		8000000000001001	01/05
424	1	000110163454		8000000000001001	01/05
425	1	000109181351		8000000000001001	01/05
425	2	000206060322		8000000000001001	01/05
425	3	000307144943		8000000000001001	01/05
425	4	000424135944		8000000000001001	01/05
426	1	000106182740		8000000000001001	01/05
426	2	000225104211		8000000000001001	01/05
426	3	000322172457		8000000000001001	01/05
426	4	000426094036		8000000000001001	01/05
426	5	000515121349		8000000000001001	01/05
427	1	000104062129		8000000000001001	01/05
427	2	000220174430		8000000000001001	01/05
427	3	000326162416		8000000000001001	01/05
428	1	000112093254		8000000000001001	01/05
428	2	000203102225		8000000000001001	01/05
428	3	000308212317		8000000000001001	01/05
428	4	000423075607		8000000000001001	01/05
429	1	000111130532		8000000000001001	01/05
429	2	000212181004		8000000000001001	01/05
430	1	000120100648		8000000000001001	01/05
431	1	000117061632		8000000000001001	01/05
431	2	000205151258		8000000000001001	01/05
431	3	000327083305		8000000000001001	01/05
431	4	000415195842		8000000000001001	01/05
431	5	000507191009		8000000000001001	01/05
431	6	000618172906		8000000000001001	01/05
432	1	000109224527		8000000000001001	01/05
433	1	000118085812		8000000000001001	01/05
434	1	000122145302		8000000000001001	01/05
434	2	000218234045		8000000000001001	01/05
434	3	000324152219		8000000000001001	01/05
435	1	000125110017		8000000000001001	01/05
435	2	000205175239		8000000000001001	01/05
435	3	000301142652		8000000000001001	01/05
436	1	000112062309		8000000000001001	01/05
436	2	000210125046		8000000000001001	01/05
436	3	000308071736		8000000000001001	01/05
436	4	000417155424		8000000000001001	01/05
437	1	000128231401		8000000000001001	01/05
437	2	000207130240		8000000000001001	01/05
437	3	000306065108		8000000000001001	01/05
437	4	000426082341		8000000000001001	01/05
437	5	000518223442		8000000000001001	01/05
437	6	000605215539		8000000000001001	01/05
438	1	000118070731		8000000000001001	01/05
438	2	000207190953		8000000000001001	01/05
438	3	000315084611		8000000000001001	01/05
438	4	000425101051		8000000000001001	01/05
439	1	000102140119		8000000000001001	01/05
439	2	000211160559		8000000000001001	01/05
440	1	000117235101		8000000000001001	01/05
440	2	000210073417		8000000000001001	01/05
440	3	000302101753		8000000000001001	01/05
441	1	000112061815		8000000000001001	01/05
441	2	000224212023		8000000000001001	01/05
441	3	000301144226		8000000000001001	01/05
442	1	000128084111		8000000000001001	01/05
442	2	000229132424		8000000000001001	01/05
443	1	000112095757		8000000000001001	01/05
443	2	000220233003		8000000000001001	01/05
443	3	000301210558		8000000000001001	01/05
443	4	000422170305		8000000000001001	01/05
443	5	000527113810		8000000000001001	01/05
443	6	000627123057		8000000000001001	01/05
445	1	000120175010		8000000000001001	01/05
445	2	000202161734		8000000000001001	01/05
445	3	000312163502		8000000000001001	01/05
445	4	000425224708		8000000000001001	01/05
445	5	000506185403		8000000000001001	01/05
446	1	000120173043		8000000000001001	01/05
446	2	000216190714		8000000000001001	01/05
446	3	000321124923		8000000000001001	01/05
446	4	000421160138		8000000000001001	01/05
446	5	000517194302		8000000000001001	01/05
446	6	000609183550		8000000000001001	01/05
447	1	000112191553		8000000000001001	01/05
447	2	000215131854		8000000000001001	01/05
447	3	000329204735		8000000000001001	01/05
447	4	000427133825		8000000000001001	01/05
448	1	000102212343		8000000000001001	01/05
449	1	000119133511		8000000000001001	01/05
449	2	000216172927		8000000000001001	01/05
449	3	000304195602		8000000000001001	01/05
449	4	000427094527		8000000000001001	01/05
449	5	000512161522		8000000000001001	01/05
451	1	000106202333		8000000000001001	01/05
451	2	000215123420		8000000000001001	01/05
451	3	000314061443		8000000000001001	01/05
452	1	000111174816		8000000000001001	01/05
452	2	000217193401		8000000000001001	01/05
453	1	000113183357		8000000000001001	01/05
453	2	000223222923		8000000000001001	01/05
454	1	000117061647		8000000000001001	01/05
454	2	000222060311		8000000000001001	01/05
454	3	000301193927		8000000000001001	01/05
454	4	000410120116		8000000000001001	01/05
455	1	000120075841		8000000000001001	01/05
455	2	000208173549		8000000000001001	01/05
455	3	000315093628		8000000000001001	01/05
455	4	000424192817		8000000000001001	01/05
456	1	000125131908		8000000000001001	01/05
456	2	000225150842		8000000000001001	01/05
456	3	000303070355		8000000000001001	01/05
456	4	000421083125		8000000000001001	01/05
457	1	000117173245		8000000000001001	01/05
457	2	000218103726		8000000000001001	01/05
458	1	000124124454		8000000000001001	01/05
458	2	000217153449		8000000000001001	01/05
458	3	000307094555		8000000000001001	01/05
458	4	000408190710		8000000000001001	01/05
458	5	000509062941		8000000000001001	01/05
459	1	000106174924		8000000000001001	01/05
459	2	000204210948		8000000000001001	01/05
459	3	000322145337		8000000000001001	01/05
460	1	000124163136		8000000000001001	01/05
460	2	000208163123		8000000000001001	01/05
460	3	000323181826		8000000000001001	01/05
460	4	000416143752		8000000000001001	01/05
460	5	000502212035		8000000000001001	01/05
461	1	000124185639		8000000000001001	01/05
461	2	000217205909		8000000000001001	01/05
462	1	000113113605		8000000000001001	01/05
462	2	000210140351		8000000000001001	01/05
462	3	000329064435		8000000000001001	01/05
462	4	000427171614		8000000000001001	01/05
463	1	000103094104		8000000000001001	01/05
463	2	000221083651		8000000000001001	01/05
463	3	000320142641		8000000000001001	01/05
463	4	000411075146		8000000000001001	01/05
463	5	000520115602		8000000000001001	01/05
464	1	000120213826		8000000000001001	01/05
464	2	000206103447		8000000000001001	01/05
464	3	000311150101		8000000000001001	01/05
464	4	000403105932		8000000000001001	01/05
464	5	000515211143		8000000000001001	01/05
465	1	000109214150		8000000000001001	01/05
465	2	000203105647		8000000000001001	01/05
466	1	000120185146		8000000000001001	01/05
466	2	000223152004		8000000000001001	01/05
466	3	000313175908		8000000000001001	01/05
466	4	000401131148		8000000000001001	01/05
467	1	000120192818		8000000000001001	01/05
467	2	000208223141		8000000000001001	01/05
467	3	000305214039		8000000000001001	01/05
467	4	000421221503		8000000000001001	01/05
467	5	000513111335		8000000000001001	01/05
469	1	000117233238		8000000000001001	01/05
469	2	000213140906		8000000000001001	01/05
469	3	000304164833		8000000000001001	01/05
470	1	000120162041		8000000000001001	01/05
470	2	000207210346		8000000000001001	01/05
470	3	000320172800		8000000000001001	01/05
471	1	000122104437		8000000000001001	01/05
471	2	000223214246		8000000000001001	01/05
471	3	000311111021		8000000000001001	01/05
471	4	000416115814		8000000000001001	01/05
471	5	000510151341		8000000000001001	01/05
471	6	000609070813		8000000000001001	01/05
472	1	000113081830		8000000000001001	01/05
473	1	000112185641		8000000000001001	01/05
473	2	000213131831		8000000000001001	01/05
473	3	000308172950		8000000000001001	01/05
473	4	000421082147		8000000000001001	01/05
473	5	000522061101		8000000000001001	01/05
474	1	000118110706		8000000000001001	01/05
474	2	000207144013		8000000000001001	01/05
474	3	000324145324		8000000000001001	01/05
474	4	000407194643		8000000000001001	01/05
475	1	000117064904		8000000000001001	01/05
475	2	000210200012		8000000000001001	01/05
475	3	000311065257		8000000000001001	01/05
475	4	000401141501		8000000000001001	01/05
476	1	000113234426		8000000000001001	01/05
476	2	000218191802		8000000000001001	01/05
477	1	000103120902		8000000000001001	01/05
477	2	000211112039		8000000000001001	01/05
477	3	000309182101		8000000000001001	01/05
477	4	000422153229		8000000000001001	01/05
477	5	000513164647		8000000000001001	01/05
477	6	000622090019		8000000000001001	01/05
478	1	000101072718		8000000000001001	01/05
478	2	000227110025		8000000000001001	01/05
478	3	000321114543		8000000000001001	01/05
478	4	000408195516		8000000000001001	01/05
478	5	000521195153		8000000000001001	01/05
478	6	000617172145		8000000000001001	01/05
479	1	000113204819		8000000000001001	01/05
480	1	000119120700		8000000000001001	01/05
480	2	000202214050		8000000000001001	01/05
480	3	000316150010		8000000000001001	01/05
480	4	000404230853		8000000000001001	01/05
480	5	000508234805		8000000000001001	01/05
481	1	000128183650		8000000000001001	01/05
481	2	000210091048		8000000000001001	01/05
481	3	000302180615		8000000000001001	01/05
481	4	000425161542		8000000000001001	01/05
481	5	000527171800		8000000000001001	01/05
482	1	000116193206		8000000000001001	01/05
482	2	000229094531		8000000000001001	01/05
482	3	000309180150		8000000000001001	01/05
482	4	000407122912		8000000000001001	01/05
482	5	000508172346		8000000000001001	01/05
482	6	000606211041		8000000000001001	01/05
483	1	000101200106		8000000000001001	01/05
483	2	000201082850		8000000000001001	01/05
483	3	000325115813		8000000000001001	01/05
483	4	000407171505		8000000000001001	01/05
484	1	000117202951		8000000000001001	01/05
484	2	000226151038		8000000000001001	01/05
484	3	000311090219		8000000000001001	01/05
484	4	000420221109		8000000000001001	01/05
484	5	000528130919		8000000000001001	01/05
484	6	000610175443		8000000000001001	01/05
485	1	000124230208		8000000000001001	01/05
485	2	000219110121		8000000000001001	01/05
485	3	000309100119		8000000000001001	01/05
485	4	000426181933		8000000000001001	01/05
486	1	000112183847		8000000000001001	01/05
487	1	000111221814		8000000000001001	01/05
487	2	000217223118		8000000000001001	01/05
487	3	000324075037		8000000000001001	01/05
487	4	000414071627		8000000000001001	01/05
487	5	000528175622		8000000000001001	01/05
488	1	000116072459		8000000000001001	01/05
488	2	000222070226		8000000000001001	01/05
488	3	000301134358		8000000000001001	01/05
488	4	000404221309		8000000000001001	01/05
488	5	000502143111		8000000000001001	01/05
488	6	000615100942		8000000000001001	01/05
489	1	000125233102		8000000000001001	01/05
489	2	000209122704		8000000000001001	01/05
489	3	000324130140		8000000000001001	01/05
489	4	000413164454		8000000000001001	01/05
490	1	000123125347		8000000000001001	01/05
490	2	000207091359		8000000000001001	01/05
490	3	000309100552		8000000000001001	01/05
491	1	000128204332		8000000000001001	01/05
492	1	000126201746		8000000000001001	01/05
492	2	000219140724		8000000000001001	01/05
492	3	000329060749		8000000000001001	01/05
492	4	000406112016		8000000000001001	01/05
492	5	000521060842		8000000000001001	01/05
492	6	000603122325		8000000000001001	01/05
493	1	000115141012		8000000000001001	01/05
493	2	000228095151		8000000000001001	01/05
493	3	000325173650		8000000000001001	01/05
494	1	000105062852		8000000000001001	01/05
494	2	000207215248		8000000000001001	01/05
494	3	000301171829		8000000000001001	01/05
494	4	000420144353		8000000000001001	01/05
495	1	000104112045		8000000000001001	01/05
495	2	000216122848		8000000000001001	01/05
495	3	000311142759		8000000000001001	01/05
495	4	000401141057		8000000000001001	01/05
495	5	000510123051		8000000000001001	01/05
495	6	000608222731		8000000000001001	01/05
496	1	000111112232		8000000000001001	01/05
496	2	000228074516		8000000000001001	01/05
496	3	000309165713		8000000000001001	01/05
496	4	000416163317		8000000000001001	01/05
497	1	000120062744		8000000000001001	01/05
497	2	000217220539		8000000000001001	01/05
497	3	000329223838		8000000000001001	01/05
498	1	000124134412		8000000000001001	01/05
498	2	000224231745		8000000000001001	01/05
498	3	000328075620		8000000000001001	01/05
498	4	000425235207		8000000000001001	01/05
498	5	000529070158		8000000000001001	01/05
498	6	000603091457		8000000000001001	01/05
499	1	000113153724		8000000000001001	01/05
499	2	000227114247		8000000000001001	01/05
499	3	000311144354		8000000000001001	01/05
500	1	000123111502		8000000000001001	01/05
500	2	000219074123		8000000000001001	01/05
500	3	000305073052		8000000000001001	01/05
502	1	000113134916		8000000000001001	01/05
502	2	000207104557		8000000000001001	01/05
502	3	000325231717		8000000000001001	01/05
502	4	000404205220		8000000000001001	01/05
503	1	000103174423		8000000000001001	01/05
504	1	000103090113		8000000000001001	01/05
504	2	000227223117		8000000000001001	01/05
504	3	000316184843		8000000000001001	01/05
505	1	000123231151		8000000000001001	01/05
506	1	000112094309		8000000000001001	01/05
506	2	000222204706		8000000000001001	01/05
506	3	000324072448		8000000000001001	01/05
507	1	000113182532		8000000000001001	01/05
507	2	000220142831		8000000000001001	01/05
507	3	000326184302		8000000000001001	01/05
507	4	000403221025		8000000000001001	01/05
507	5	000527191727		8000000000001001	01/05
507	6	000625142506		8000000000001001	01/05
508	1	000125182629		8000000000001001	01/05
508	2	000214120434		8000000000001001	01/05
509	1	000102103825		8000000000001001	01/05
509	2	000211064256		8000000000001001	01/05
510	1	000124171233		8000000000001001	01/05
510	2	000217062712		8000000000001001	01/05
510	3	000307151056		8000000000001001	01/05
510	4	000408074419		8000000000001001	01/05
510	5	000517141703		8000000000001001	01/05
511	1	000102123636		8000000000001001	01/05
511	2	000213134202		8000000000001001	01/05
511	3	000301133437		8000000000001001	01/05
511	4	000421063958		8000000000001001	01/05
511	5	000507162242		8000000000001001	01/05
512	1	000112113054		8000000000001001	01/05
512	2	000218132415		8000000000001001	01/05
513	1	000112084040		8000000000001001	01/05
513	2	000219223805		8000000000001001	01/05
514	1	000102115511		8000000000001001	01/05
514	2	000214160142		8000000000001001	01/05
514	3	000303181259		8000000000001001	01/05
514	4	000405071421		8000000000001001	01/05
514	5	000505185313		8000000000001001	01/05
514	6	000624072657		8000000000001001	01/05
515	1	000129060157		8000000000001001	01/05
515	2	000219114400		8000000000001001	01/05
515	3	000309113708		8000000000001001	01/05
516	1	000106192518		8000000000001001	01/05
517	1	000124095706		8000000000001001	01/05
518	1	000122144244		8000000000001001	01/05
519	1	000129101818		8000000000001001	01/05
520	1	000129154805		8000000000001001	01/05
520	2	000219104755		8000000000001001	01/05
521	1	000113173759		8000000000001001	01/05
521	2	000225103650		8000000000001001	01/05
522	1	000119164916		8000000000001001	01/05
522	2	000207161128		8000000000001001	01/05
522	3	000319185537		8000000000001001	01/05
522	4	000402124024		8000000000001001	01/05
522	5	000518172714		8000000000001001	01/05
522	6	000602115728		8000000000001001	01/05
523	1	000110090517		8000000000001001	01/05
523	2	000218093910		8000000000001001	01/05
523	3	000308105940		8000000000001001	01/05
524	1	000101234644		8000000000001001	01/05
525	1	000120125308		8000000000001001	01/05
526	1	000129120227		8000000000001001	01/05
527	1	000126114509		8000000000001001	01/05
527	2	000216230253		8000000000001001	01/05
527	3	000302094157		8000000000001001	01/05
527	4	000402215637		8000000000001001	01/05
527	5	000504155640		8000000000001001	01/05
528	1	000112184516		8000000000001001	01/05
528	2	000224185936		8000000000001001	01/05
528	3	000303102443		8000000000001001	01/05
528	4	000426081418		8000000000001001	01/05
528	5	000524231518		8000000000001001	01/05
528	6	000619223057		8000000000001001	01/05
529	1	000110224935		8000000000001001	01/05
530	1	000118092413		8000000000001001	01/05
530	2	000202101752		8000000000001001	01/05
530	3	000313221651		8000000000001001	01/05
530	4	000424165239		8000000000001001	01/05
530	5	000518195750		8000000000001001	01/05
531	1	000125235538		8000000000001001	01/05
531	2	000206225333		8000000000001001	01/05
531	3	000317200129		8000000000001001	01/05
531	4	000411200957		8000000000001001	01/05
531	5	000518083846		8000000000001001	01/05
531	6	000618223757		8000000000001001	01/05
532	1	000116214506		8000000000001001	01/05
533	1	000126141033		8000000000001001	01/05
533	2	000206130240		8000000000001001	01/05
533	3	000310072812		8000000000001001	01/05
533	4	000420133624		8000000000001001	01/05
533	5	000506204151		8000000000001001	01/05
534	1	000104115832		8000000000001001	01/05
534	2	000211133220		8000000000001001	01/05
534	3	000302131726		8000000000001001	01/05
534	4	000404123939		8000000000001001	01/05
535	1	000118082025		8000000000001001	01/05
535	2	000229133419		8000000000001001	01/05
535	3	000323130523		8000000000001001	01/05
535	4	000429170545		8000000000001001	01/05
535	5	000521065859		8000000000001001	01/05
536	1	000129184637		8000000000001001	01/05
536	2	000226115433		8000000000001001	01/05
536	3	000325235248		8000000000001001	01/05
536	4	000411144302		8000000000001001	01/05
536	5	000515195043		8000000000001001	01/05
537	1	000113124454		8000000000001001	01/05
537	2	000207080809		8000000000001001	01/05
537	3	000319173916		8000000000001001	01/05
537	4	000409141724		8000000000001001	01/05
538	1	000122060622		8000000000001001	01/05
538	2	000204184139		8000000000001001	01/05
539	1	000114215230		8000000000001001	01/05
539	2	000222080927		8000000000001001	01/05
539	3	000309073124		8000000000001001	01/05
540	1	000119144707		8000000000001001	01/05
540	2	000209205755		8000000000001001	01/05
540	3	000303173043		8000000000001001	01/05
540	4	000421134729		8000000000001001	01/05
540	5	000502083908		8000000000001001	01/05
540	6	000629092428		8000000000001001	01/05
541	1	000112114528		8000000000001001	01/05
541	2	000221125904		8000000000001001	01/05
541	3	000301223224		8000000000001001	01/05
542	1	000104180212		8000000000001001	01/05
542	2	000224081813		8000000000001001	01/05
543	1	000102230345		8000000000001001	01/05
544	1	000118190854		8000000000001001	01/05
544	2	000214090626		8000000000001001	01/05
544	3	000307113542		8000000000001001	01/05
544	4	000418101103		8000000000001001	01/05
544	5	000522061326		8000000000001001	01/05
544	6	000625222102		8000000000001001	01/05
545	1	000123081412		8000000000001001	01/05
545	2	000204114037		8000000000001001	01/05
546	1	000113165935		8000000000001001	01/05
546	2	000206223619		8000000000001001	01/05
546	3	000318060202		8000000000001001	01/05
546	4	000411232350		8000000000001001	01/05
547	1	000105185734		8000000000001001	01/05
547	2	000216073806		8000000000001001	01/05
547	3	000308174721		8000000000001001	01/05
547	4	000428070357		8000000000001001	01/05
547	5	000507165439		8000000000001001	01/05
548	1	000128101819		8000000000001001	01/05
548	2	000206181144		8000000000001001	01/05
548	3	000324212205		8000000000001001	01/05
549	1	000101162048		8000000000001001	01/05
549	2	000202072137		8000000000001001	01/05
549	3	000326121654		8000000000001001	01/05
549	4	000428170943		8000000000001001	01/05
549	5	000516075829		8000000000001001	01/05
549	6	000606230047		8000000000001001	01/05
550	1	000111172037		8000000000001001	01/05
550	2	000210073107		8000000000001001	01/05
550	3	000301104557		8000000000001001	01/05
550	4	000421203220		8000000000001001	01/05
550	5	000520065014		8000000000001001	01/05
550	6	000621140109		8000000000001001	01/05
551	1	000112081631		8000000000001001	01/05
552	1	000125110217		8000000000001001	01/05
552	2	000223084817		8000000000001001	01/05
552	3	000308124505		8000000000001001	01/05
553	1	000129133343		8000000000001001	01/05
553	2	000214092226		8000000000001001	01/05
553	3	000310072135		8000000000001001	01/05
553	4	000425161436		8000000000001001	01/05
554	1	000128104830		8000000000001001	01/05
555	1	000121234556		8000000000001001	01/05
556	1	000111072706		8000000000001001	01/05
556	2	000216094221		8000000000001001	01/05
556	3	000304114447		8000000000001001	01/05
557	1	000116191722		8000000000001001	01/05
558	1	000128234908		8000000000001001	01/05
558	2	000228172044		8000000000001001	01/05
558	3	000302140522		8000000000001001	01/05
558	4	000414152925		8000000000001001	01/05
559	1	000125175054		8000000000001001	01/05
560	1	000126215104		8000000000001001	01/05
560	2	000203210649		8000000000001001	01/05
560	3	000324164337		8000000000001001	01/05
561	1	000110135710		8000000000001001	01/05
561	2	000223194140		8000000000001001	01/05
562	1	000124201458		8000000000001001	01/05
562	2	000207153849		8000000000001001	01/05
562	3	000310225126		8000000000001001	01/05
563	1	000120174853		8000000000001001	01/05
563	2	000211210437		8000000000001001	01/05
563	3	000309230705		8000000000001001	01/05
564	1	000123095514		8000000000001001	01/05
564	2	000217202213		8000000000001001	01/05
565	1	000112075936		8000000000001001	01/05
565	2	000202232028		8000000000001001	01/05
565	3	000309121817		8000000000001001	01/05
566	1	000120230307		8000000000001001	01/05
567	1	000128063054		8000000000001001	01/05
567	2	000214221825		8000000000001001	01/05
567	3	000322165422		8000000000001001	01/05
567	4	000410160918		8000000000001001	01/05
567	5	000514230922		8000000000001001	01/05
568	1	000126060829		8000000000001001	01/05
568	2	000227213228		8000000000001001	01/05
569	1	000110220621		8000000000001001	01/05
569	2	000211235713		8000000000001001	01/05
569	3	000329222818		8000000000001001	01/05
569	4	000401171622		8000000000001001	01/05
569	5	000521140440		8000000000001001	01/05
569	6	000613200120		8000000000001001	01/05
571	1	000106072259		8000000000001001	01/05
571	2	000213154959		8000000000001001	01/05
571	3	000309193251		8000000000001001	01/05
571	4	000401143348		8000000000001001	01/05
572	1	000111191756		8000000000001001	01/05
572	2	000221233008		8000000000001001	01/05
572	3	000319225941		8000000000001001	01/05
572	4	000404212532		8000000000001001	01/05
573	1	000106143309		8000000000001001	01/05
573	2	000224064451		8000000000001001	01/05
574	1	000103182511		8000000000001001	01/05
574	2	000219150326		8000000000001001	01/05
575	1	000104201408		8000000000001001	01/05
575	2	000225062417		8000000000001001	01/05
575	3	000327190851		8000000000001001	01/05
576	1	000108181657		8000000000001001	01/05
576	2	000216143233		8000000000001001	01/05
576	3	000329071609		8000000000001001	01/05
577	1	000120065319		8000000000001001	01/05
577	2	000215123810		8000000000001001	01/05
577	3	000307235019		8000000000001001	01/05
578	1	000124083644		8000000000001001	01/05
578	2	000202112906		8000000000001001	01/05
579	1	000109081709		8000000000001001	01/05
579	2	000224160711		8000000000001001	01/05
579	3	000313113752		8000000000001001	01/05
580	1	000115214810		8000000000001001	01/05
580	2	000208072634		8000000000001001	01/05
580	3	000323154910		8000000000001001	01/05
580	4	000425103220		8000000000001001	01/05
581	1	000125124002		8000000000001001	01/05
581	2	000207102533		8000000000001001	01/05
581	3	000308182007		8000000000001001	01/05
581	4	000419184947		8000000000001001	01/05
581	5	000512170510		8000000000001001	01/05
582	1	000103082235		8000000000001001	01/05
582	2	000223173656		8000000000001001	01/05
582	3	000325140738		8000000000001001	01/05
583	1	000103080127		8000000000001001	01/05
583	2	000203205734		8000000000001001	01/05
583	3	000313101928		8000000000001001	01/05
583	4	000413134530		8000000000001001	01/05
583	5	000512180742		8000000000001001	01/05
583	6	000622232649		8000000000001001	01/05
584	1	000128141508		8000000000001001	01/05
584	2	000219185523		8000000000001001	01/05
584	3	000301234811		8000000000001001	01/05
584	4	000428090133		8000000000001001	01/05
585	1	000119174750		8000000000001001	01/05
585	2	000209084529		8000000000001001	01/05
585	3	000314065848		8000000000001001	01/05
585	4	000413221906		8000000000001001	01/05
585	5	000514122556		8000000000001001	01/05
586	1	000108083111		8000000000001001	01/05
586	2	000225090235		8000000000001001	01/05
586	3	000316140502		8000000000001001	01/05
587	1	000118142810		8000000000001001	01/05
587	2	000228094224		8000000000001001	01/05
588	1	000113151110		8000000000001001	01/05
588	2	000222180708		8000000000001001	01/05
588	3	000329090747		8000000000001001	01/05
588	4	000412153155		8000000000001001	01/05
589	1	000108120650		8000000000001001	01/05
590	1	000123115514		8000000000001001	01/05
590	2	000206194236		8000000000001001	01/05
590	3	000307160844		8000000000001001	01/05
590	4	000411150913		8000000000001001	01/05
591	1	000101112255		8000000000001001	01/05
591	2	000219174809		8000000000001001	01/05
591	3	000312200355		8000000000001001	01/05
591	4	000427174845		8000000000001001	01/05
592	1	000104201255		8000000000001001	01/05
593	1	000113155146		8000000000001001	01/05
594	1	000114161631		8000000000001001	01/05
595	1	000106072602		8000000000001001	01/05
595	2	000229065322		8000000000001001	01/05
595	3	000324073425		8000000000001001	01/05
595	4	000410134502		8000000000001001	01/05
596	1	000107201617		8000000000001001	01/05
596	2	000205154523		8000000000001001	01/05
597	1	000113232014		8000000000001001	01/05
597	2	000215182309		8000000000001001	01/05
597	3	000308131253		8000000000001001	01/05
597	4	000410125043		8000000000001001	01/05
598	1	000109130842		8000000000001001	01/05
598	2	000211114532		8000000000001001	01/05
598	3	000303235959		8000000000001001	01/05
598	4	000420121223		8000000000001001	01/05
598	5	000527124800		8000000000001001	01/05
598	6	000613060842		8000000000001001	01/05
599	1	000106135138		8000000000001001	01/05
599	2	000226115639		8000000000001001	01/05
599	3	000325202114		8000000000001001	01/05
600	1	000118134210		8000000000001001	01/05
600	2	000202204052		8000000000001001	01/05
600	3	000327155038		8000000000001001	01/05
600	4	000416172419		8000000000001001	01/05
600	5	000512152704		8000000000001001	01/05
601	1	000103113444		8000000000001001	01/05
601	2	000220070003		8000000000001001	01/05
601	3	000327132637		8000000000001001	01/05
601	4	000414195551		8000000000001001	01/05
601	5	000523145050		8000000000001001	01/05
601	6	000628215022		8000000000001001	01/05
602	1	000121195048		8000000000001001	01/05
602	2	000225232412		8000000000001001	01/05
602	3	000319103408		8000000000001001	01/05
603	1	000118125443		8000000000001001	01/05
603	2	000207093826		8000000000001001	01/05
603	3	000329204943		8000000000001001	01/05
603	4	000409225554		8000000000001001	01/05
603	5	000528231446		8000000000001001	01/05
604	1	000129101840		8000000000001001	01/05
604	2	000201155534		8000000000001001	01/05
604	3	000322230343		8000000000001001	01/05
605	1	000101204026		8000000000001001	01/05
605	2	000211082512		8000000000001001	01/05
605	3	000312093105		8000000000001001	01/05
605	4	000412153758		8000000000001001	01/05
605	5	000515081108		8000000000001001	01/05
606	1	000104073038		8000000000001001	01/05
606	2	000227150450		8000000000001001	01/05
606	3	000317065720		8000000000001001	01/05
606	4	000405080050		8000000000001001	01/05
606	5	000505205829		8000000000001001	01/05
607	1	000120152948		8000000000001001	01/05
607	2	000222213914		8000000000001001	01/05
607	3	000303192300		8000000000001001	01/05
607	4	000408094711		8000000000001001	01/05
607	5	000519185542		8000000000001001	01/05
608	1	000128213308		8000000000001001	01/05
608	2	000217132016		8000000000001001	01/05
608	3	000328095236		8000000000001001	01/05
608	4	000413080533		8000000000001001	01/05
609	1	000120184443		8000000000001001	01/05
609	2	000224062906		8000000000001001	01/05
609	3	000304225252		8000000000001001	01/05
609	4	000404103158		8000000000001001	01/05
609	5	000515162044		8000000000001001	01/05
609	6	000623100711		8000000000001001	01/05
610	1	000123124020		8000000000001001	01/05
610	2	000219214054		8000000000001001	01/05
610	3	000305194551		8000000000001001	01/05
610	4	000428190615		8000000000001001	01/05
610	5	000510074305		8000000000001001	01/05
610	6	000608195220		8000000000001001	01/05
611	1	000127072826		8000000000001001	01/05
612	1	000119150531		8000000000001001	01/05
612	2	000202062044		8000000000001001	01/05
613	1	000121204913		8000000000001001	01/05
613	2	000211190036		8000000000001001	01/05
613	3	000315185508		8000000000001001	01/05
614	1	000127161525		8000000000001001	01/05
614	2	000215152329		8000000000001001	01/05
615	1	000103111625		8000000000001001	01/05
615	2	000226075348		8000000000001001	01/05
616	1	000107060945		8000000000001001	01/05
616	2	000214173703		8000000000001001	01/05
617	1	000101214845		8000000000001001	01/05
617	2	000215135609		8000000000001001	01/05
617	3	000301104109		8000000000001001	01/05
617	4	000405181501		8000000000001001	01/05
617	5	000519132936		8000000000001001	01/05
617	6	000623144413		8000000000001001	01/05
618	1	000106210000		8000000000001001	01/05
618	2	000218220422		8000000000001001	01/05
619	1	000127090947		8000000000001001	01/05
619	2	000228234311		8000000000001001	01/05
619	3	000320123115		8000000000001001	01/05
620	1	000121121055		8000000000001001	01/05
620	2	000204190719		8000000000001001	01/05
621	1	000103070258		8000000000001001	01/05
622	1	000104162634		8000000000001001	01/05
622	2	000229133605		8000000000001001	01/05
623	1	000124072019		8000000000001001	01/05
623	2	000207180733		8000000000001001	01/05
623	3	000322214254		8000000000001001	01/05
623	4	000401075629		8000000000001001	01/05
624	1	000119215432		8000000000001001	01/05
624	2	000229075803		8000000000001001	01/05
624	3	000326083046		8000000000001001	01/05
624	4	000404162909		8000000000001001	01/05
624	5	000517141511		8000000000001001	01/05
624	6	000601065233		8000000000001001	01/05
625	1	000113135752		8000000000001001	01/05
625	2	000207231531		8000000000001001	01/05
625	3	000329192423		8000000000001001	01/05
626	1	000113180018		8000000000001001	01/05
626	2	000214204652		8000000000001001	01/05
626	3	000310232245		8000000000001001	01/05
626	4	000410214449		8000000000001001	01/05
626	5	000523072205		8000000000001001	01/05
626	6	000621235721		8000000000001001	01/05
627	1	000105125053		8000000000001001	01/05
627	2	000218102458		8000000000001001	01/05
628	1	000110161605		8000000000001001	01/05
629	1	000114141136		8000000000001001	01/05
629	2	000223081245		8000000000001001	01/05
629	3	000326153543		8000000000001001	01/05
629	4	000404124018		8000000000001001	01/05
629	5	000515093527		8000000000001001	01/05
629	6	000602080542		8000000000001001	01/05
630	1	000103183851		8000000000001001	01/05
630	2	000227094736		8000000000001001	01/05
630	3	000306071242		8000000000001001	01/05
630	4	000425120225		8000000000001001	01/05
630	5	000511230945		8000000000001001	01/05
630	6	000609114251		8000000000001001	01/05
631	1	000104155505		8000000000001001	01/05
631	2	000214071056		8000000000001001	01/05
632	1	000124161454		8000000000001001	01/05
633	1	000115185108		8000000000001001	01/05
633	2	000207205109		8000000000001001	01/05
634	1	000118095544		8000000000001001	01/05
634	2	000228070806		8000000000001001	01/05
634	3	000326183157		8000000000001001	01/05
635	1	000111202027		8000000000001001	01/05
635	2	000224113330		8000000000001001	01/05
635	3	000304230047		8000000000001001	01/05
635	4	000426110827		8000000000001001	01/05
635	5	000523123759		8000000000001001	01/05
636	1	000106162049		8000000000001001	01/05
637	1	000123205917		8000000000001001	01/05
637	2	000226103227		8000000000001001	01/05
638	1	000121093530		8000000000001001	01/05
638	2	000202094424		8000000000001001	01/05
639	1	000110111100		8000000000001001	01/05
639	2	000225095215		8000000000001001	01/05
639	3	000327140215		8000000000001001	01/05
639	4	000424130247		8000000000001001	01/05
640	1	000109091931		8000000000001001	01/05
640	2	000203064127		8000000000001001	01/05
640	3	000329071544		8000000000001001	01/05
640	4	000410180130		8000000000001001	01/05
640	5	000501090458		8000000000001001	01/05
641	1	000104165742		8000000000001001	01/05
641	2	000202221924		8000000000001001	01/05
642	1	000112212706		8000000000001001	01/05
642	2	000202072408		8000000000001001	01/05
643	1	000124092505		8000000000001001	01/05
643	2	000222204906		8000000000001001	01/05
644	1	000125151444		8000000000001001	01/05
644	2	000214085417		8000000000001001	01/05
644	3	000311220553		8000000000001001	01/05
645	1	000119200356		8000000000001001	01/05
645	2	000205195537		8000000000001001	01/05
645	3	000305070413		8000000000001001	01/05
645	4	000426122300		8000000000001001	01/05
645	5	000520214312		8000000000001001	01/05
646	1	000118205702		8000000000001001	01/05
646	2	000228095808		8000000000001001	01/05
646	3	000306114609		8000000000001001	01/05
646	4	000411222319		8000000000001001	01/05
646	5	000525085414		8000000000001001	01/05
647	1	000109225921		8000000000001001	01/05
647	2	000210125312		8000000000001001	01/05
647	3	000303092829		8000000000001001	01/05
647	4	000414191418		8000000000001001	01/05
647	5	000512101843		8000000000001001	01/05
648	1	000111194024		8000000000001001	01/05
648	2	000209094932		8000000000001001	01/05
648	3	000324235411		8000000000001001	01/05
648	4	000419222327		8000000000001001	01/05
649	1	000108174811		8000000000001001	01/05
650	1	000111211045		8000000000001001	01/05
650	2	000212060755		8000000000001001	01/05
-1	7	\N	\N	\N	\N
-1	8	\N	\N	\N	\N
-1	9	\N	\N	\N	\N
-1	10	\N	\N	\N	\N
-1	11	\N	\N	\N	\N
-1	12	\N	\N	\N	\N
-1	13	\N	\N	\N	\N
-1	14	\N	\N	\N	\N
-1	15	\N	\N	\N	\N
-1	16	\N	\N	\N	\N
-1	17	\N	\N	\N	\N
-1	18	\N	\N	\N	\N
-1	19	\N	\N	\N	\N
-1	20	\N	\N	\N	\N
-1	21	\N	\N	\N	\N
-1	22	\N	\N	\N	\N
-1	23	\N	\N	\N	\N
-1	24	\N	\N	\N	\N
-1	25	\N	\N	\N	\N
-1	26	\N	\N	\N	\N
-1	27	\N	\N	\N	\N
-1	28	\N	\N	\N	\N
-1	29	\N	\N	\N	\N
-1	30	\N	\N	\N	\N
-1	31	\N	\N	\N	\N
-1	32	\N	\N	\N	\N
-1	33	\N	\N	\N	\N
-1	34	\N	\N	\N	\N
-1	35	\N	\N	\N	\N
-1	36	\N	\N	\N	\N
-1	37	\N	\N	\N	\N
-1	38	\N	\N	\N	\N
-1	39	\N	\N	\N	\N
-1	40	\N	\N	\N	\N
-1	41	\N	\N	\N	\N
-1	42	\N	\N	\N	\N
-1	43	\N	\N	\N	\N
-1	44	\N	\N	\N	\N
-1	45	\N	\N	\N	\N
-1	46	\N	\N	\N	\N
-1	47	\N	\N	\N	\N
-1	48	\N	\N	\N	\N
-1	49	\N	\N	\N	\N
-1	50	\N	\N	\N	\N
-1	51	\N	\N	\N	\N
-1	52	\N	\N	\N	\N
-1	53	\N	\N	\N	\N
-1	54	\N	\N	\N	\N
-1	55	\N	\N	\N	\N
\.


--
-- Data for Name: region; Type: TABLE DATA; Schema: public; Owner: -
--

COPY region (region_id, region_name) FROM stdin;
1	All
2	Goulburn Valley
3	Rutherglen
4	Coonawarra
5	Upper Hunter Valley
6	Lower Hunter Valley
7	Barossa Valley
8	Riverland
9	Margaret River
10	Swan Valley
\.


--
-- Data for Name: titles; Type: TABLE DATA; Schema: public; Owner: -
--

COPY titles (title_id, title) FROM stdin;
1	Mr        
2	Mrs       
3	Miss      
4	Ms        
5	Dr        
6	Prof      
7	Rev       
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: -
--

COPY users (cust_id, user_name, password) FROM stdin;
\.


--
-- Data for Name: wine; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wine (wine_id, wine_name, wine_type, year, winery_id, description) FROM stdin;
1	Archibald	2	1997	1	\N
2	Pattendon	3	1975	1	\N
3	Lombardi	4	1985	2	\N
4	Tonkin	2	1984	2	\N
5	Titshall	5	1986	2	\N
6	Serrong	6	1995	2	\N
7	Mettaxus	5	1996	2	\N
8	Titshall	4	1987	3	\N
9	Serrong	3	1981	3	\N
10	Chester	5	1999	3	\N
11	Chemnis	2	1980	3	\N
12	Holdenson	6	1979	4	\N
13	Skerry	2	1975	4	\N
14	Pattendon	5	1978	4	\N
15	Titshall	3	1999	4	\N
16	Belcombe	3	1998	4	\N
17	Dimitria	5	1981	5	\N
18	Titshall	5	1977	5	\N
19	Holdenson	4	1986	6	\N
20	Sears	2	1999	6	\N
21	Sorrenti	3	1970	6	\N
22	Belcombe	3	1972	7	\N
23	Sears	4	1986	8	\N
24	Kinsala	3	1985	8	\N
25	Skerry	5	1973	8	\N
26	Keisling	6	1973	8	\N
27	Mellaseca	6	1973	8	\N
28	Keisling	6	1985	8	\N
29	Chester	2	1983	9	\N
30	Nancarral	5	1988	9	\N
31	Mockridge	6	1970	9	\N
32	Dimitria	6	1977	9	\N
33	Barneshaw	2	1973	10	\N
34	Lombardi	4	1985	10	\N
35	Chester	4	1970	10	\N
36	Mettaxus	2	1971	10	\N
37	Stribling	6	1975	10	\N
38	Tonkin	6	1986	10	\N
39	Sears	3	1975	11	\N
40	Rosenthal	5	1981	12	\N
41	Florenini	6	1998	13	\N
42	Mettaxus	3	1982	13	\N
43	Pattendon	6	1984	14	\N
44	Barneshaw	4	1978	14	\N
45	Ruscina	3	1977	15	\N
46	Marzalla	3	1991	15	\N
47	Marzalla	6	1999	16	\N
48	Lombardi	3	1975	17	\N
49	Oaton	3	1993	17	\N
50	Sears	5	1993	17	\N
51	Mockridge	4	1983	17	\N
52	Tonkin	6	1989	17	\N
53	Triskit	6	1990	18	\N
54	Eggelston	2	1996	18	\N
55	Morfooney	3	1985	18	\N
56	Oaton	3	1980	18	\N
57	Stribling	2	1998	19	\N
58	Florenini	3	1996	19	\N
59	Galti	5	1984	19	\N
60	Serrong	6	1998	20	\N
61	Mockridge	3	1975	20	\N
62	Marzalla	6	1979	20	\N
63	Serrong	2	1970	20	\N
64	Triskit	2	1988	20	\N
65	Sears	3	1971	20	\N
66	Ruscina	5	1975	21	\N
67	Krennan	3	1976	21	\N
68	Mockridge	5	1996	21	\N
69	Holdenson	6	1972	21	\N
70	Taggendharf	3	1974	21	\N
71	Barneshaw	5	1998	22	\N
72	Barneshaw	3	1972	23	\N
73	Nancarral	3	1991	23	\N
74	Ruscina	5	1970	23	\N
75	Tonkin	2	1988	23	\N
76	Ritterman	3	1974	23	\N
77	Marzalla	4	1994	24	\N
78	Dalion	3	1992	24	\N
79	Mellaseca	2	1990	24	\N
80	Leramonth	3	1976	24	\N
81	Kinsala	5	1976	24	\N
82	Oaton	5	1979	24	\N
83	Belcombe	3	1977	25	\N
84	Dalion	4	1971	25	\N
85	Morfooney	5	1989	26	\N
86	Mellili	6	1988	26	\N
87	Mockridge	3	1984	26	\N
88	Dalion	2	1975	26	\N
89	Mellili	5	1980	26	\N
90	Chemnis	6	1990	27	\N
91	Nancarral	6	1987	27	\N
92	Florenini	6	1985	27	\N
93	Stribling	6	1976	28	\N
94	Oaton	5	1972	29	\N
95	Chester	5	1977	29	\N
96	Skerry	3	1976	29	\N
97	Galti	5	1986	29	\N
98	Triskit	6	1987	29	\N
99	Stribling	6	1975	29	\N
100	Titshall	5	1984	30	\N
101	Woodestock	6	1980	30	\N
102	Morfooney	5	1991	30	\N
103	Krennan	2	1984	30	\N
104	Sears	3	1986	30	\N
105	Eggelston	5	1994	30	\N
106	Archibald	3	1971	31	\N
107	Ruscina	2	1971	31	\N
108	Taggendharf	6	1988	32	\N
109	Holdenson	3	1974	32	\N
110	Oaton	6	1975	32	\N
111	Dalion	5	1991	32	\N
112	Nancarral	6	1975	32	\N
113	Sears	5	1971	32	\N
114	Patton	4	1982	33	\N
115	Dalion	6	1982	33	\N
116	Florenini	2	1979	33	\N
117	Patton	6	1996	33	\\x54686973206973206f6e652073686f74206f7574206f662074686520626c756521205769746820737472696b696e6720636f6c6f75722c207468697320697320612066727569742d64726976656e2077696e652c207769746820696e74656e736520626c7565626572727920616e6420626c61636b62657272792066727569742061726f6d61732c207768696368206361727279206f6e20746f207468652077656c6c20637261667465642c2066756c6c2d666c61766f757265642070616c6174652c207768696368206861732067726561742070657273697374656e6365206f6620666c61766f75722e
118	Belcombe	5	1976	33	\N
119	Chemnis	3	1988	34	\N
120	Skerry	3	1996	35	\N
121	Holdenson	3	1974	36	\N
122	Stribling	6	1979	36	\N
123	Sorrenti	2	1973	36	\N
124	Titshall	3	1995	36	\N
125	Mockridge	3	1976	37	\N
126	Mettaxus	5	1971	37	\N
127	Belcombe	2	1984	37	\N
128	Taggendharf	6	1982	37	\N
129	Ritterman	6	1998	37	\N
130	Florenini	2	1972	37	\N
131	Mellili	5	1995	38	\N
132	Krennan	4	1995	39	\N
133	Galti	5	1999	39	\N
134	Kinsala	4	1970	39	\N
135	Florenini	5	1989	39	\N
136	Chemnis	5	1998	39	\N
137	Archibald	6	1994	39	\N
138	Morfooney	2	1987	40	\N
139	Woodburne	6	1985	40	\N
140	Galti	5	1979	40	\N
141	Dalion	2	1970	40	\N
142	Eggelston	6	1999	41	\N
143	Belcombe	5	1974	41	\N
144	Mockridge	5	1991	41	\N
145	Mockridge	6	1991	42	\N
146	Galti	5	1990	43	\N
147	Taggendharf	6	1987	44	\N
148	Belcombe	5	1984	44	\N
149	Marzalla	5	1989	44	\N
150	Dimitria	3	1980	45	\N
151	Serrong	5	1970	46	\N
152	Woodburne	2	1986	46	\N
153	Skerry	5	1986	46	\N
154	Florenini	6	1975	46	\N
155	Barneshaw	5	1987	46	\N
156	Pattendon	5	1988	46	\N
157	Lombardi	3	1972	47	\N
158	Tonnibrook	6	1977	47	\N
159	Woodburne	5	1975	47	\N
160	Lombardi	6	1993	47	\N
161	Galti	2	1988	47	\N
162	Mettaxus	2	1994	48	\N
163	Galti	5	1995	48	\N
164	Mellili	2	1990	48	\N
165	Mockridge	2	1999	48	\\x41207665727920676f6f64206578616d706c65206f66204d617267617265742052697665722063686172646f6e6e61792e20506561636820616e64206d656c6f6e207363656e74732061726520666f6c6c6f77656420627920612066756c6c2070616c617465206f6620726970652066727569742074617374657320616e642061206c6f6e672c206472792066696e6973682e20436869636b656e206469736865732c20686f74206f7220636f6c642c2077696c6c206d617463682069742077656c6c206e6f77206f722063656c6c617220697420756e74696c20323030332e
166	Belcombe	5	1996	49	\N
167	Cassisi	6	1983	49	\N
168	Rosenthal	6	1984	49	\N
169	Barneshaw	2	1989	49	\N
170	Cassisi	2	1988	49	\N
171	Morfooney	6	1979	50	\N
172	Kinsala	6	1970	50	\N
173	Barneshaw	2	1975	50	\N
174	Mettaxus	5	1996	50	\N
175	Titshall	5	1972	51	\N
176	Oaton	2	1984	51	\N
177	Mockridge	5	1992	52	\N
178	Sorrenti	3	1970	52	\N
179	Keisling	6	1982	53	\N
180	Serrong	2	1979	54	\N
181	Ritterman	5	1993	54	\N
182	Belcombe	6	1975	54	\N
183	Chester	4	1980	55	\N
184	Chester	2	1994	55	\N
185	Florenini	6	1983	55	\N
186	Eggelston	6	1988	55	\N
187	Skerry	5	1982	55	\N
188	Krennan	3	1995	55	\N
189	Sorrenti	4	1976	56	\N
190	Sears	5	1991	57	\N
191	Belcombe	6	1997	58	\\x52696368207275627920636f6c6f7572207769746820707572706c652074696e6765732069742068617320612073756767657374696f6e206f66207065707065726d696e7420616e642072697065207265642062657272792061726f6d61732e2049742773206120626967676973682077696e65207769746820612077656c6c2d737472756374757265642c20726963686c7920666c61766f757265642070616c61746520616e64206669726d2074616e6e696e732e2054727920697420776974682072617265206265656620646973686573206f722063656c6c617220756e74696c20323030352e
192	Sorrenti	6	1977	58	\N
193	Skerry	6	1984	58	\N
194	Rosenthal	5	1990	58	\N
195	Mellili	2	1996	59	\N
196	Eggelston	3	1990	59	\N
197	Serrong	2	1997	59	\N
198	Sears	2	1989	59	\N
199	Marzalla	4	1971	59	\N
200	Skerry	2	1978	59	\N
201	Keisling	6	1982	60	\N
202	Galti	4	1996	60	\N
203	Sorrenti	5	1987	60	\N
204	Keisling	5	1998	60	\N
205	Triskit	6	1985	60	\N
206	Stribling	5	1980	61	\N
207	Galti	5	1973	61	\N
208	Woodburne	3	1971	61	\N
209	Stribling	5	1975	62	\N
210	Mockridge	6	1984	62	\N
211	Belcombe	3	1987	62	\N
212	Morfooney	6	1998	62	\N
213	Titshall	6	1974	62	\N
214	Dimitria	6	1977	63	\N
215	Eggelston	6	1998	63	\N
216	Taggendharf	3	1993	63	\N
217	Taggendharf	5	1972	63	\N
218	Mettaxus	6	1975	63	\N
219	Lombardi	3	1992	64	\N
220	Mellili	6	1991	64	\N
221	Leramonth	3	1994	64	\N
222	Serrong	5	1981	64	\N
223	Dimitria	5	1970	64	\N
224	Mockridge	2	1976	64	\N
225	Chemnis	2	1995	65	\N
226	Mellaseca	4	1991	65	\N
227	Sears	6	1989	65	\N
228	Woodburne	5	1982	65	\N
229	Triskit	6	1977	66	\N
230	Nancarral	2	1988	66	\N
231	Chemnis	6	1995	67	\\x5468697320726963686c7920666c61766f757265642c20736f667420616e642073756363756c656e74206472696e6b20697320737572656c79206f6e65206f66207468652062657374206472792072656420627579732061726f756e642e2043726166746564206279206d617374657220536f757468204175737472616c69616e2077696e656d616b6572204a6f686e20506574657273656e2c2074686973206973206120736572696f75732077696e65207468617427732061206772656174206163636f6d70616e696d656e7420666f72207665616c206469736865732e
232	Stribling	3	1997	67	\N
233	Marzalla	6	1977	67	\N
234	Dalion	5	1988	67	\N
235	Mockridge	3	1986	68	\N
236	Chester	3	1982	68	\N
237	Kinsala	6	1989	68	\N
238	Mockridge	2	1982	68	\N
239	Sorrenti	5	1987	68	\N
240	Tonkin	5	1973	69	\N
241	Woodburne	5	1973	69	\N
242	Chemnis	5	1984	69	\N
243	Dalion	3	1998	69	\N
244	Triskit	5	1999	70	\N
245	Holdenson	6	1999	70	\N
246	Krennan	6	1972	70	\N
247	Titshall	3	1977	70	\N
248	Leramonth	3	1993	71	\N
249	Skerry	6	1974	71	\N
250	Mellili	4	1981	71	\N
251	Skerry	2	1998	71	\N
252	Lombardi	6	1972	71	\N
253	Nancarral	3	1994	72	\N
254	Stribling	6	1978	72	\N
255	Ritterman	6	1979	72	\N
256	Dalion	2	1982	72	\N
257	Mettaxus	6	1980	72	\N
258	Sears	2	1996	72	\N
259	Kinsala	2	1995	73	\N
260	Woodestock	6	1990	74	\N
261	Woodestock	6	1996	74	\N
262	Mellaseca	2	1987	74	\N
263	Ruscina	5	1974	74	\N
264	Chemnis	2	1996	74	\N
265	Sorrenti	2	1990	75	\N
266	Ritterman	5	1998	75	\N
267	Belcombe	5	1988	75	\N
268	Morfooney	4	1976	75	\N
269	Titshall	3	1988	75	\N
270	Titshall	2	1975	75	\N
271	Holdenson	3	1976	76	\N
272	Morfooney	5	1977	76	\N
273	Cassisi	2	1989	76	\N
274	Barneshaw	3	1973	76	\N
275	Chester	6	1970	76	\N
276	Barneshaw	5	1971	77	\N
277	Chemnis	5	1975	77	\N
278	Florenini	4	1993	78	\N
279	Kinsala	3	1974	78	\N
280	Mettaxus	3	1971	79	\N
281	Chemnis	3	1990	80	\N
282	Kinsala	3	1982	80	\N
283	Krennan	2	1994	80	\N
284	Woodestock	2	1985	80	\N
285	Skerry	4	1997	80	\N
286	Holdenson	2	1993	80	\N
287	Holdenson	5	1987	81	\N
288	Rosenthal	6	1971	81	\N
289	Cassisi	3	1973	81	\N
290	Ritterman	2	1978	81	\N
291	Kinsala	2	1992	81	\N
292	Mockridge	2	1987	82	\N
293	Mellaseca	5	1988	83	\N
294	Stribling	4	1984	83	\N
295	Pattendon	5	1992	83	\N
296	Archibald	6	1978	83	\N
297	Dimitria	4	1984	84	\N
298	Archibald	4	1994	84	\N
299	Belcombe	5	1976	84	\N
300	Triskit	5	1976	85	\N
301	Chemnis	3	1982	85	\N
302	Cassisi	3	1978	85	\N
303	Woodburne	2	1974	85	\N
304	Skerry	4	1993	85	\N
305	Skerry	3	1999	86	\N
306	Nancarral	6	1973	86	\N
307	Dalion	5	1970	87	\N
308	Sears	5	1984	87	\N
309	Chemnis	6	1973	87	\N
310	Pattendon	3	1973	87	\N
311	Taggendharf	5	1979	88	\N
312	Patton	5	1986	88	\N
313	Belcombe	3	1979	88	\N
314	Triskit	6	1971	88	\N
315	Stribling	6	1973	88	\N
316	Tonkin	3	1986	88	\N
317	Mellili	5	1985	89	\N
318	Krennan	5	1970	90	\N
319	Cassisi	2	1986	90	\N
320	Rosenthal	6	1987	91	\N
321	Patton	6	1981	91	\N
322	Sears	3	1987	92	\N
323	Ritterman	5	1990	92	\N
324	Archibald	6	1988	92	\N
325	Rosenthal	2	1977	93	\N
326	Chester	2	1987	93	\N
327	Mellili	2	1997	93	\N
328	Galti	6	1974	93	\N
329	Lombardi	6	1978	94	\N
330	Barneshaw	5	1975	94	\N
331	Krennan	2	1978	94	\N
332	Rosenthal	4	1997	94	\N
333	Skerry	3	1984	94	\N
334	Pattendon	6	1986	95	\N
335	Dimitria	5	1988	95	\N
336	Marzalla	6	1974	96	\N
337	Morfooney	2	1977	96	\N
338	Titshall	3	1977	97	\N
339	Mockridge	5	1971	97	\N
340	Belcombe	2	1986	97	\N
341	Lombardi	2	1988	97	\N
342	Mockridge	5	1971	97	\N
343	Mellaseca	2	1982	98	\N
344	Mellili	3	1972	98	\N
345	Archibald	3	1998	98	\N
346	Mockridge	6	1980	98	\N
347	Belcombe	5	1990	98	\N
348	Barneshaw	6	1996	99	\N
349	Mellili	4	1996	100	\N
350	Taggendharf	2	1999	101	\N
351	Mettaxus	2	1984	101	\N
352	Sorrenti	3	1992	101	\N
353	Dalion	3	1981	101	\N
354	Titshall	4	1990	101	\N
355	Mellili	5	1991	102	\N
356	Chemnis	2	1985	103	\N
357	Florenini	6	1979	103	\N
358	Woodburne	5	1970	104	\N
359	Mellili	6	1988	104	\N
360	Mettaxus	6	1993	104	\N
361	Keisling	2	1984	105	\N
362	Eggelston	6	1975	105	\N
363	Taggendharf	4	1984	105	\N
364	Patton	4	1989	106	\N
365	Mettaxus	5	1996	106	\N
366	Archibald	6	1989	106	\N
367	Morfooney	5	1985	107	\N
368	Titshall	5	1984	108	\N
369	Ritterman	4	1993	108	\N
370	Leramonth	2	1999	108	\N
371	Mockridge	5	1990	108	\N
372	Woodestock	3	1976	109	\N
373	Sorrenti	4	1983	109	\N
374	Chester	5	1989	109	\N
375	Holdenson	5	1995	109	\N
376	Lombardi	5	1992	109	\N
377	Galti	5	1996	110	\N
378	Galti	3	1973	110	\N
379	Mettaxus	2	1974	110	\N
380	Ruscina	2	1995	110	\N
381	Galti	6	1982	110	\\x536f7272792c2062757420796f752073686f756c646e2774207265616c6c792062652061626c6520746f206765742074686973206d75636820666c61766f75722066726f6d20612077696e6520746869732063686561702e205468656e20616761696e2c20696620796f752063616e2c2077687920636f6d706c61696e3f2054686572652773206865617073206f6620617474726163746976652066726573682067726170657920667275697420666c61766f757220616e6420657665722d736f2d736c696768746c792073776565742067726170652d70756c7079206a756963696e6573732e204a757374206472696e6b2069742e20446f6e2774207175657374696f6e207468652070726963652e
382	Ruscina	2	1970	110	\N
383	Titshall	3	1982	111	\N
384	Taggendharf	6	1992	111	\N
385	Woodburne	4	1982	112	\N
386	Belcombe	5	1986	112	\N
387	Galti	4	1974	112	\N
388	Sorrenti	6	1991	112	\N
389	Mellili	6	1999	112	\N
390	Woodburne	6	1978	112	\N
391	Cassisi	3	1970	113	\N
392	Ritterman	5	1986	113	\N
393	Holdenson	4	1993	113	\N
394	Stribling	5	1995	113	\N
395	Eggelston	5	1997	113	\N
396	Marzalla	6	1973	113	\N
397	Holdenson	2	1990	114	\N
398	Stribling	5	1992	114	\N
399	Sears	5	1981	114	\N
400	Ritterman	3	1985	114	\N
401	Serrong	6	1995	115	\N
402	Rosenthal	5	1996	115	\N
403	Kinsala	2	1997	115	\N
404	Holdenson	3	1980	115	\N
405	Woodestock	6	1981	116	\N
406	Florenini	6	1995	117	\N
407	Dimitria	4	1993	118	\N
408	Triskit	2	1976	119	\N
409	Sorrenti	2	1999	119	\N
410	Sears	6	1971	119	\N
411	Kinsala	6	1975	119	\N
412	Skerry	2	1977	119	\N
413	Morfooney	3	1970	119	\N
414	Morfooney	5	1976	120	\N
415	Dalion	6	1987	120	\N
416	Kinsala	6	1995	120	\N
417	Eggelston	6	1993	120	\N
418	Galti	4	1988	121	\N
419	Archibald	2	1982	121	\N
420	Triskit	5	1970	122	\N
421	Sorrenti	5	1980	122	\N
422	Leramonth	6	1990	122	\N
423	Skerry	4	1977	122	\N
424	Lombardi	6	1997	122	\N
425	Woodestock	3	1976	122	\N
426	Holdenson	6	1970	123	\N
427	Holdenson	5	1997	123	\N
428	Nancarral	4	1998	123	\N
429	Rosenthal	5	1986	123	\N
430	Belcombe	6	1975	124	\N
431	Marzalla	2	1977	124	\N
432	Leramonth	5	1988	124	\N
433	Rosenthal	2	1987	124	\N
434	Triskit	6	1972	124	\N
435	Patton	5	1979	124	\N
436	Holdenson	2	1974	125	\N
437	Mockridge	3	1985	125	\N
438	Pattendon	6	1973	125	\N
439	Kinsala	6	1975	125	\N
440	Pattendon	5	1978	125	\N
441	Pattendon	5	1997	125	\N
442	Cassisi	5	1974	126	\N
443	Ritterman	6	1975	126	\N
444	Barneshaw	6	1978	127	\N
445	Mellaseca	3	1970	127	\N
446	Skerry	3	1984	127	\N
447	Dalion	2	1979	128	\N
448	Pattendon	2	1996	129	\N
449	Ruscina	6	1972	129	\N
450	Skerry	4	1977	130	\N
451	Woodburne	6	1980	131	\N
452	Tonnibrook	4	1971	131	\N
453	Skerry	3	1998	131	\N
454	Mellili	5	1974	131	\N
455	Pattendon	5	1993	131	\N
456	Triskit	2	1982	131	\N
457	Sears	5	1985	132	\N
458	Woodburne	2	1998	132	\N
459	Ruscina	6	1997	132	\N
460	Holdenson	5	1986	132	\N
461	Holdenson	3	1999	132	\N
462	Taggendharf	2	1978	133	\N
463	Woodestock	6	1987	133	\N
464	Holdenson	3	1982	134	\N
465	Mettaxus	6	1983	134	\N
466	Mettaxus	5	1985	134	\N
467	Ruscina	2	1991	134	\N
468	Archibald	6	1984	134	\N
469	Holdenson	4	1972	135	\N
470	Tonkin	5	1976	135	\N
471	Mellili	5	1991	135	\N
472	Cassisi	5	1984	136	\N
473	Triskit	6	1985	137	\N
474	Florenini	4	1975	137	\N
475	Archibald	2	1986	137	\N
476	Dalion	6	1999	137	\N
477	Mellaseca	5	1971	138	\N
478	Archibald	3	1994	138	\N
479	Titshall	6	1985	138	\N
480	Woodburne	6	1998	139	\N
481	Barneshaw	3	1976	139	\N
482	Stribling	4	1987	139	\N
483	Taggendharf	3	1972	139	\N
484	Mellaseca	6	1972	139	\N
485	Stribling	5	1996	140	\N
486	Triskit	5	1997	140	\N
487	Dalion	6	1980	141	\N
488	Patton	5	1978	141	\N
489	Sears	5	1974	141	\N
490	Oaton	3	1986	141	\N
491	Mellaseca	4	1997	142	\N
492	Belcombe	2	1975	142	\N
493	Taggendharf	5	1992	142	\N
494	Ruscina	3	1981	142	\N
495	Triskit	5	1971	142	\N
496	Tonkin	4	1979	142	\N
497	Woodburne	4	1985	143	\N
498	Leramonth	3	1981	143	\N
499	Rosenthal	6	1979	144	\N
500	Nancarral	3	1998	144	\N
501	Taggendharf	6	1980	144	\N
502	Nancarral	3	1981	144	\N
503	Sorrenti	6	1973	145	\N
504	Chemnis	2	1990	145	\N
505	Galti	6	1986	145	\N
506	Stribling	5	1982	145	\N
507	Keisling	5	1975	145	\N
508	Chester	2	1996	146	\N
509	Archibald	6	1987	146	\N
510	Mellaseca	6	1976	146	\N
511	Woodburne	3	1986	147	\N
512	Taggendharf	2	1972	147	\N
513	Mockridge	5	1997	147	\N
514	Mettaxus	2	1985	147	\N
515	Sorrenti	6	1985	147	\N
516	Eggelston	2	1973	148	\N
517	Tonnibrook	6	1987	148	\N
518	Tonkin	5	1989	149	\N
519	Chemnis	5	1991	149	\N
520	Mockridge	6	1980	149	\N
521	Woodestock	5	1980	149	\N
522	Marzalla	3	1977	149	\N
523	Titshall	2	1988	150	\N
524	Mellaseca	5	1982	150	\N
525	Krennan	2	1978	151	\N
526	Cassisi	6	1989	151	\N
527	Dalion	6	1992	151	\N
528	Sorrenti	6	1992	151	\N
529	Pattendon	6	1994	152	\N
530	Mellaseca	6	1991	152	\N
531	Belcombe	6	1975	152	\N
532	Sorrenti	6	1984	152	\N
533	Kinsala	5	1984	152	\N
534	Woodburne	6	1986	153	\N
535	Sears	3	1975	153	\N
536	Woodburne	2	1971	153	\N
537	Archibald	6	1987	153	\N
538	Ritterman	2	1981	154	\N
539	Galti	3	1991	155	\N
540	Triskit	4	1983	155	\N
541	Leramonth	5	1983	155	\N
542	Oaton	5	1985	155	\N
543	Florenini	2	1975	155	\N
544	Krennan	5	1988	155	\N
545	Lombardi	5	1976	156	\N
546	Ritterman	6	1981	156	\N
547	Archibald	5	1971	156	\N
548	Nancarral	3	1992	156	\N
549	Lombardi	4	1970	156	\N
550	Skerry	6	1999	157	\N
551	Marzalla	2	1981	157	\N
552	Titshall	5	1983	158	\N
553	Patton	5	1992	158	\N
554	Mellili	3	1997	159	\N
555	Pattendon	2	1979	159	\N
556	Mettaxus	6	1983	159	\N
557	Marzalla	6	1986	159	\N
558	Ritterman	5	1998	160	\N
559	Woodburne	2	1970	160	\N
560	Kinsala	6	1996	160	\N
561	Galti	2	1972	160	\N
562	Marzalla	2	1973	161	\N
563	Ruscina	2	1988	161	\N
564	Dalion	5	1998	161	\N
565	Morfooney	6	1982	162	\N
566	Leramonth	6	1987	162	\N
567	Woodburne	6	1989	162	\N
568	Galti	5	1998	162	\N
569	Woodestock	3	1999	162	\N
570	Morfooney	4	1977	163	\N
571	Taggendharf	6	1991	163	\N
572	Leramonth	5	1970	163	\N
573	Dimitria	3	1991	163	\N
574	Titshall	5	1978	163	\N
575	Mockridge	3	1981	163	\N
576	Keisling	2	1999	164	\N
577	Mellili	2	1980	165	\N
578	Ruscina	5	1982	165	\N
579	Skerry	6	1977	165	\N
580	Cassisi	3	1996	165	\N
581	Oaton	5	1994	165	\N
582	Mockridge	2	1985	166	\N
583	Mellaseca	5	1992	166	\N
584	Galti	4	1996	166	\N
585	Ruscina	3	1994	167	\N
586	Tonkin	2	1998	167	\N
587	Mellaseca	6	1997	167	\N
588	Mettaxus	6	1982	167	\N
589	Patton	6	1974	168	\N
590	Dalion	3	1978	168	\N
591	Ritterman	5	1983	169	\N
592	Holdenson	5	1982	169	\N
593	Mellaseca	2	1999	169	\N
594	Woodburne	3	1979	169	\N
595	Stribling	3	1979	170	\N
596	Barneshaw	5	1997	170	\N
597	Dimitria	3	1988	170	\N
598	Kinsala	5	1980	170	\N
599	Tonkin	2	1979	170	\N
600	Woodburne	5	1984	171	\N
601	Mockridge	5	1988	171	\N
602	Patton	5	1973	171	\N
603	Krennan	3	1994	171	\N
604	Nancarral	5	1986	171	\N
605	Holdenson	2	1997	171	\N
606	Chester	5	1974	172	\N
607	Woodburne	6	1989	172	\N
608	Triskit	5	1999	172	\N
609	Mettaxus	6	1983	172	\N
610	Ruscina	6	1993	172	\N
611	Pattendon	5	1987	172	\N
612	Barneshaw	6	1991	173	\N
613	Oaton	3	1972	174	\N
614	Ruscina	4	1987	174	\N
615	Woodburne	2	1984	175	\N
616	Mockridge	3	1986	175	\N
617	Holdenson	6	1985	175	\N
618	Mellaseca	3	1982	175	\N
619	Triskit	3	1990	176	\N
620	Titshall	5	1983	176	\N
621	Woodestock	6	1995	176	\N
622	Barneshaw	5	1991	176	\N
623	Mettaxus	3	1976	176	\N
624	Morfooney	6	1999	176	\\x5468652077696e6520697320612064656570206372696d736f6e20636f6c6f757220776974682061726f6d6173206f6620726970652063727573686564206265727269657320616e6420706c756d732061732077656c6c2061732073706963792076616e696c6c696e206f616b2e205468652070616c6174652064656c6976657273206c6179657273206f66207374726f6e6720666c61766f757273206f66207269706520706c756d20616e642062657272696573206275696c742061726f756e64206120726f6275737420737472756374757265206f66206669726d206f616b20616e642073696c6b792074616e6e696e732e205468652063656c6c6172696e6720706f74656e7469616c206f6620746869732077696e652069732031352d32352079656172732e20546869732077696e652069732073756974656420746f20726963686c7920666c61766f7572656420726564206d65617420616e642067616d65206469736865732e
625	Eggelston	3	1982	177	\N
626	Mellaseca	6	1983	178	\N
627	Archibald	4	1987	178	\N
628	Barneshaw	5	1995	179	\N
629	Barneshaw	3	1991	179	\N
630	Dimitria	5	1983	179	\N
631	Ruscina	6	1972	180	\N
632	Skerry	2	1978	180	\N
633	Sorrenti	3	1993	180	\N
634	Patton	2	1970	180	\N
635	Sears	5	1978	180	\N
636	Serrong	6	1973	181	\N
637	Barneshaw	4	1987	181	\N
638	Florenini	5	1995	181	\N
639	Lombardi	2	1980	181	\N
640	Marzalla	4	1986	181	\N
641	Chemnis	4	1980	181	\N
642	Stribling	2	1998	182	\N
643	Ruscina	2	1977	182	\N
644	Florenini	4	1982	182	\N
645	Kinsala	3	1974	182	\N
646	Morfooney	4	1981	183	\N
647	Tonkin	2	1982	183	\N
648	Mockridge	4	1970	183	\N
649	Titshall	4	1992	184	\N
650	Skerry	4	1990	184	\N
651	Kinsala	4	1982	184	\N
652	Sorrenti	5	1998	185	\N
653	Ritterman	3	1985	185	\N
654	Tonkin	5	1989	185	\N
655	Rosenthal	2	1977	186	\N
656	Titshall	3	1999	186	\N
657	Oaton	5	1972	186	\N
658	Krennan	5	1981	186	\N
659	Patton	5	1985	187	\N
660	Mockridge	6	1970	187	\N
661	Mellili	2	1998	187	\N
662	Morfooney	5	1987	188	\N
663	Eggelston	4	1989	188	\N
664	Dalion	2	1973	189	\N
665	Mettaxus	3	1994	189	\N
666	Cassisi	6	1972	189	\N
667	Titshall	5	1981	189	\N
668	Mockridge	5	1978	189	\N
669	Galti	6	1981	189	\N
670	Taggendharf	3	1998	190	\N
671	Leramonth	6	1974	190	\N
672	Kinsala	5	1971	190	\N
673	Keisling	6	1980	191	\N
674	Woodburne	2	1997	191	\N
675	Oaton	5	1996	192	\N
676	Pattendon	3	1998	193	\N
677	Keisling	2	1980	193	\N
678	Chester	2	1976	193	\N
679	Leramonth	3	1991	194	\N
680	Tonnibrook	5	1986	194	\N
681	Krennan	5	1989	194	\N
682	Dimitria	2	1981	194	\N
683	Woodburne	5	1980	194	\N
684	Stribling	3	1976	195	\N
685	Belcombe	6	1984	195	\N
686	Krennan	2	1986	195	\N
687	Florenini	2	1990	195	\N
688	Belcombe	6	1977	195	\N
689	Woodestock	5	1998	195	\N
690	Mellili	6	1993	196	\N
691	Mellaseca	3	1987	196	\N
692	Cassisi	4	1997	196	\N
693	Lombardi	5	1982	196	\N
694	Belcombe	5	1993	196	\N
695	Titshall	6	1978	196	\N
696	Mockridge	2	1987	197	\N
697	Pattendon	3	1970	197	\N
698	Stribling	5	1994	197	\N
699	Stribling	6	1995	197	\N
700	Mockridge	5	1978	197	\N
701	Krennan	5	1999	197	\N
702	Tonnibrook	4	1971	198	\N
703	Dimitria	6	1995	198	\N
704	Mettaxus	3	1979	198	\N
705	Tonnibrook	6	1981	198	\N
706	Sears	5	1992	198	\N
707	Dimitria	5	1978	198	\N
708	Kinsala	5	1986	199	\N
709	Dalion	2	1999	199	\N
710	Kinsala	3	1986	199	\N
711	Triskit	5	1992	199	\N
712	Eggelston	6	1996	199	\N
713	Woodestock	5	1989	200	\N
714	Stribling	3	1993	200	\N
715	Archibald	2	1973	200	\N
716	Keisling	6	1973	200	\N
717	Mockridge	5	1980	201	\N
718	Mellaseca	4	1993	201	\N
719	Skerry	5	1992	202	\N
720	Oaton	4	1975	202	\N
721	Chemnis	3	1982	202	\N
722	Oaton	5	1986	202	\N
723	Kinsala	6	1979	202	\N
724	Rosenthal	6	1977	203	\N
725	Mellaseca	2	1976	203	\N
726	Keisling	4	1984	203	\N
727	Oaton	3	1987	203	\N
728	Rosenthal	5	1975	204	\N
729	Chemnis	5	1989	204	\N
730	Krennan	6	1993	204	\N
731	Morfooney	2	1974	204	\N
732	Woodestock	6	1997	204	\N
733	Tonnibrook	6	1977	205	\N
734	Lombardi	5	1995	205	\N
735	Morfooney	3	1998	205	\N
736	Eggelston	6	1989	205	\N
737	Galti	6	1977	206	\N
738	Dimitria	2	1978	206	\N
739	Holdenson	5	1990	206	\N
740	Chester	5	1980	206	\N
741	Mockridge	5	1976	207	\N
742	Titshall	6	1973	207	\N
743	Keisling	2	1999	207	\N
744	Dimitria	4	1989	207	\N
745	Mettaxus	3	1976	208	\N
746	Mockridge	5	1991	208	\N
747	Mellili	6	1991	208	\N
748	Krennan	6	1997	208	\N
749	Galti	5	1990	208	\N
750	Barneshaw	5	1982	209	\N
751	Dalion	4	1976	209	\N
752	Kinsala	2	1987	210	\N
753	Skerry	2	1976	210	\N
754	Sears	2	1973	210	\N
755	Woodestock	6	1974	210	\N
756	Sorrenti	5	1984	211	\N
757	Mettaxus	5	1973	211	\N
758	Pattendon	5	1993	212	\N
759	Marzalla	5	1973	212	\N
760	Nancarral	6	1983	212	\N
761	Leramonth	4	1973	212	\N
762	Cassisi	6	1985	213	\N
763	Woodburne	3	1999	214	\N
764	Kinsala	5	1978	214	\N
765	Morfooney	5	1988	214	\N
766	Skerry	5	1978	214	\N
767	Mockridge	6	1995	214	\N
768	Chester	3	1974	215	\N
769	Ruscina	5	1989	215	\N
770	Lombardi	6	1985	215	\N
771	Sears	5	1982	215	\N
772	Nancarral	4	1992	215	\N
773	Patton	3	1981	215	\N
774	Stribling	6	1983	216	\N
775	Cassisi	2	1975	216	\N
776	Pattendon	2	1993	216	\N
777	Sorrenti	2	1999	217	\N
778	Archibald	4	1993	217	\N
779	Mellaseca	3	1987	218	\N
780	Belcombe	5	1985	218	\N
781	Morfooney	5	1986	218	\N
782	Keisling	5	1987	219	\N
783	Sorrenti	3	1987	219	\N
784	Archibald	6	1993	219	\N
785	Eggelston	5	1987	219	\N
786	Tonnibrook	6	1991	220	\N
787	Serrong	4	1981	221	\N
788	Nancarral	6	1976	221	\N
789	Skerry	3	1999	221	\N
790	Cassisi	5	1972	221	\N
791	Mellili	3	1999	221	\N
792	Chemnis	5	1982	221	\N
793	Galti	2	1999	222	\N
794	Sears	5	1977	222	\N
795	Galti	5	1985	222	\N
796	Marzalla	5	1983	222	\N
797	Cassisi	3	1973	223	\N
798	Serrong	2	1983	223	\N
799	Nancarral	2	1996	223	\N
800	Barneshaw	5	1987	223	\N
801	Archibald	3	1998	224	\\x51756974652061206c757363696f75732061667465722d64696e6e6572206472696e6b206d6164652066726f6d2073757065722d7269706520706564726f206772617065732e20546865206e6f736520697320616c69766520776974682074686520736d656c6c73206f662062757474657273636f7463682c20686f6e657920616e6420636172616d656c20616e64207468652077696e652066696c6c7320746865206d6f757468207769746820737765657420726963686e6573732c20796574207468652066696e6973682069732073757270726973696e676c79206472792e20536572766520697420776974682064657373657274206f722063656c6c617220756e74696c20323031322e
802	Cassisi	5	1996	225	\N
803	Rosenthal	5	1996	226	\N
804	Chemnis	5	1992	226	\N
805	Barneshaw	6	1984	226	\N
806	Belcombe	6	1970	226	\N
807	Woodestock	5	1997	226	\N
808	Rosenthal	5	1982	226	\N
809	Titshall	3	1972	227	\N
810	Kinsala	6	1983	228	\N
811	Dimitria	2	1998	228	\N
812	Woodestock	2	1971	228	\N
813	Belcombe	6	1972	228	\N
814	Morfooney	5	1988	229	\N
815	Mettaxus	3	1994	229	\N
816	Stribling	6	1976	229	\N
817	Taggendharf	5	1996	229	\N
818	Mellili	5	1982	230	\N
819	Chester	5	1983	230	\N
820	Mockridge	4	1976	231	\N
821	Tonnibrook	6	1994	232	\N
822	Mellaseca	3	1989	232	\N
823	Lombardi	2	1999	232	\N
824	Taggendharf	6	1991	233	\N
825	Galti	5	1988	233	\N
826	Woodburne	2	1988	234	\N
827	Sorrenti	3	1970	234	\N
828	Rosenthal	6	1971	235	\N
829	Rosenthal	5	1997	235	\N
830	Dalion	5	1978	236	\N
831	Morfooney	6	1982	236	\N
832	Woodburne	2	1974	236	\N
833	Barneshaw	6	1993	236	\N
834	Barneshaw	2	1972	236	\N
835	Marzalla	3	1972	236	\N
836	Mettaxus	6	1992	237	\N
837	Tonkin	3	1976	238	\N
838	Archibald	5	1985	238	\N
839	Chester	5	1975	239	\N
840	Galti	3	1999	239	\N
841	Sorrenti	2	1996	239	\N
842	Rosenthal	6	1979	239	\N
843	Nancarral	2	1983	240	\N
844	Skerry	5	1978	240	\N
845	Dimitria	6	1992	240	\N
846	Serrong	3	1978	241	\N
847	Barneshaw	5	1970	241	\N
848	Tonnibrook	3	1991	241	\N
849	Krennan	3	1970	241	\N
850	Galti	6	1995	241	\N
851	Mockridge	3	1980	242	\N
852	Barneshaw	6	1981	242	\N
853	Eggelston	5	1983	243	\N
854	Taggendharf	5	1988	243	\N
855	Sorrenti	6	1985	244	\N
856	Pattendon	2	1993	245	\N
857	Barneshaw	4	1976	245	\N
858	Cassisi	6	1981	246	\N
859	Archibald	3	1986	247	\N
860	Dalion	5	1987	247	\N
861	Kinsala	2	1975	247	\N
862	Mettaxus	2	1998	247	\N
863	Skerry	6	1973	247	\N
864	Tonnibrook	6	1994	247	\N
865	Kinsala	5	1987	248	\N
866	Ritterman	5	1980	248	\N
867	Kinsala	5	1978	248	\N
868	Tonnibrook	4	1993	248	\N
869	Mockridge	2	1993	248	\N
870	Kinsala	2	1989	249	\N
871	Sorrenti	5	1995	249	\N
872	Stribling	3	1973	249	\N
873	Ritterman	2	1977	250	\N
874	Sears	6	1987	251	\N
875	Keisling	5	1985	252	\N
876	Chemnis	2	1973	252	\N
877	Sorrenti	4	1983	252	\N
878	Lombardi	5	1989	252	\N
879	Leramonth	2	1986	253	\N
880	Mellaseca	2	1995	254	\N
881	Barneshaw	3	1987	254	\N
882	Nancarral	2	1975	255	\N
883	Dimitria	2	1980	255	\N
884	Patton	6	1993	255	\N
885	Tonkin	6	1997	255	\N
886	Skerry	2	1982	256	\N
887	Sorrenti	6	1978	256	\N
888	Dimitria	5	1985	256	\N
889	Morfooney	5	1988	257	\N
890	Keisling	2	1997	257	\N
891	Serrong	6	1977	257	\N
892	Woodestock	2	1991	257	\N
893	Ritterman	5	1974	257	\N
894	Barneshaw	4	1973	258	\N
895	Galti	6	1981	258	\N
896	Mellili	5	1971	258	\N
897	Patton	2	1997	258	\N
898	Skerry	6	1982	258	\N
899	Chester	2	1991	259	\N
900	Mettaxus	4	1999	259	\N
901	Belcombe	2	1971	259	\N
902	Chemnis	5	1998	259	\N
903	Keisling	2	1982	259	\N
904	Mellaseca	3	1977	260	\N
905	Marzalla	6	1983	260	\N
906	Morfooney	6	1982	260	\N
907	Krennan	5	1991	260	\N
908	Belcombe	3	1989	260	\N
909	Serrong	6	1974	261	\N
910	Dimitria	6	1970	261	\N
911	Krennan	6	1984	261	\N
912	Ruscina	5	1977	261	\N
913	Nancarral	2	1970	261	\N
914	Patton	3	1975	262	\N
915	Florenini	5	1973	263	\N
916	Mellaseca	5	1993	263	\N
917	Keisling	2	1976	263	\N
918	Skerry	3	1980	264	\N
919	Nancarral	6	1997	264	\N
920	Galti	5	1976	264	\N
921	Rosenthal	5	1987	264	\N
922	Leramonth	5	1994	265	\N
923	Kinsala	5	1985	265	\N
924	Barneshaw	6	1973	265	\N
925	Krennan	4	1975	266	\N
926	Patton	5	1996	266	\N
927	Titshall	4	1995	266	\N
928	Kinsala	5	1974	266	\N
929	Eggelston	2	1995	267	\N
930	Florenini	5	1984	267	\N
931	Pattendon	5	1990	267	\N
932	Dimitria	2	1976	267	\N
933	Titshall	2	1971	267	\N
934	Cassisi	6	1978	267	\N
935	Skerry	6	1997	268	\N
936	Ritterman	6	1975	268	\N
937	Mettaxus	5	1983	268	\N
938	Mockridge	5	1972	269	\N
939	Morfooney	2	1974	269	\N
940	Serrong	2	1983	269	\N
941	Tonnibrook	3	1984	270	\N
942	Patton	2	1971	270	\N
943	Woodburne	6	1996	270	\N
944	Nancarral	4	1979	270	\N
945	Mettaxus	6	1983	270	\N
946	Woodburne	5	1975	271	\N
947	Sears	6	1973	271	\N
948	Triskit	6	1995	271	\N
949	Galti	3	1993	271	\N
950	Nancarral	5	1974	271	\N
951	Sorrenti	5	1993	271	\N
952	Tonnibrook	5	1988	272	\N
953	Marzalla	3	1993	272	\N
954	Sears	5	1973	273	\N
955	Dimitria	3	1985	273	\N
956	Marzalla	3	1995	273	\N
957	Ruscina	5	1973	274	\N
958	Serrong	5	1990	274	\N
959	Cassisi	5	1975	274	\N
960	Krennan	2	1996	274	\N
961	Rosenthal	5	1972	275	\N
962	Woodestock	2	1998	275	\N
963	Tonnibrook	6	1983	275	\N
964	Cassisi	6	1978	276	\N
965	Taggendharf	6	1984	276	\N
966	Dalion	2	1997	276	\N
967	Keisling	6	1978	277	\N
968	Mellaseca	5	1978	277	\N
969	Belcombe	3	1999	277	\N
970	Krennan	5	1985	277	\N
971	Ritterman	3	1999	277	\N
972	Sears	5	1989	277	\N
973	Barneshaw	2	1989	278	\N
974	Oaton	5	1972	278	\N
975	Skerry	6	1997	278	\N
976	Galti	5	1974	279	\N
977	Titshall	6	1984	279	\N
978	Stribling	3	1994	279	\N
979	Skerry	2	1981	279	\N
980	Tonkin	6	1998	279	\N
981	Nancarral	5	1993	279	\N
982	Marzalla	2	1987	280	\N
983	Woodestock	3	1998	281	\N
984	Ruscina	2	1977	281	\N
985	Chester	5	1981	281	\N
986	Chester	4	1971	281	\N
987	Sears	6	1977	282	\N
988	Galti	3	1998	282	\N
989	Ruscina	6	1982	282	\N
990	Kinsala	2	1992	283	\N
991	Sorrenti	6	1970	284	\N
992	Cassisi	6	1992	284	\N
993	Patton	5	1981	284	\N
994	Eggelston	5	1977	284	\N
995	Cassisi	5	1975	284	\N
996	Barneshaw	2	1982	285	\N
997	Sorrenti	5	1986	286	\N
998	Mellaseca	6	1995	286	\N
999	Cassisi	5	1985	286	\N
1000	Triskit	6	1993	286	\N
1001	Mellili	5	1991	286	\N
1002	Rosenthal	3	1983	287	\N
1003	Mellili	6	1984	287	\N
1004	Mellaseca	6	1981	288	\N
1005	Mellaseca	3	1990	288	\N
1006	Chemnis	5	1992	288	\N
1007	Chemnis	3	1977	288	\N
1008	Chemnis	5	1985	288	\N
1009	Nancarral	3	1971	289	\N
1010	Tonnibrook	6	1978	289	\N
1011	Triskit	5	1977	289	\N
1012	Galti	6	1999	289	\N
1013	Dimitria	3	1980	289	\N
1014	Cassisi	4	1988	290	\N
1015	Rosenthal	5	1988	290	\N
1016	Chemnis	6	1981	290	\N
1017	Mockridge	6	1987	290	\N
1018	Skerry	2	1998	291	\N
1019	Sorrenti	5	1996	291	\N
1020	Galti	2	1994	291	\N
1021	Chester	5	1971	291	\N
1022	Krennan	3	1995	291	\N
1023	Serrong	6	1975	292	\N
1024	Serrong	5	1983	292	\N
1025	Holdenson	6	1978	292	\N
1026	Sears	6	1986	292	\N
1027	Ruscina	3	1986	293	\N
1028	Pattendon	6	1993	293	\N
1029	Taggendharf	2	1994	293	\N
1030	Mettaxus	5	1997	293	\N
1031	Belcombe	3	1977	293	\N
1032	Tonkin	4	1987	294	\N
1033	Rosenthal	5	1979	294	\N
1034	Taggendharf	2	1978	294	\N
1035	Mellili	3	1989	295	\N
1036	Archibald	5	1997	296	\N
1037	Oaton	6	1985	296	\N
1038	Keisling	2	1989	296	\N
1039	Triskit	2	1989	296	\N
1040	Skerry	2	1975	297	\N
1041	Tonnibrook	2	1984	297	\N
1042	Sears	6	1998	298	\N
1043	Ruscina	6	1974	298	\N
1044	Woodestock	3	1999	298	\N
1045	Tonkin	3	1988	298	\N
1046	Leramonth	6	1972	298	\N
1047	Mettaxus	4	1984	299	\N
1048	Titshall	5	1983	300	\N
\.


--
-- Data for Name: wine_type; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wine_type (wine_type_id, wine_type) FROM stdin;
1	All
2	Sparkling
3	Fortified
4	Sweet
5	White
6	Red
\.


--
-- Data for Name: wine_variety; Type: TABLE DATA; Schema: public; Owner: -
--

COPY wine_variety (wine_id, variety_id, id) FROM stdin;
1	19	1
1	20	2
2	16	1
3	14	1
4	18	1
5	5	1
5	3	2
5	4	3
6	12	1
6	11	2
7	6	1
7	7	2
8	14	1
9	16	1
10	2	1
11	18	1
12	11	1
12	13	2
13	19	1
13	21	2
14	2	1
15	15	1
16	17	1
17	3	1
17	4	2
18	1	1
19	14	1
20	18	1
21	15	1
22	16	1
23	14	1
24	17	1
25	5	1
26	9	1
27	13	1
28	11	1
28	3	2
29	19	1
29	20	2
30	6	1
30	7	2
31	11	1
31	13	2
32	11	1
32	3	2
33	18	1
34	14	1
35	14	1
36	18	1
37	13	1
38	12	1
38	11	2
39	16	1
40	5	1
40	3	2
40	4	3
41	11	1
41	13	2
42	17	1
43	6	1
43	10	2
44	14	1
45	16	1
46	16	1
47	12	1
47	11	2
47	13	3
48	17	1
49	16	1
50	1	1
51	14	1
52	12	1
52	11	2
53	9	1
54	18	1
55	17	1
56	15	1
57	19	1
57	20	2
58	17	1
59	2	1
60	12	1
60	11	2
61	17	1
62	9	1
63	18	1
64	19	1
64	21	2
65	15	1
66	1	1
67	15	1
68	3	1
68	4	2
69	12	1
70	17	1
71	6	1
71	7	2
72	17	1
73	15	1
74	5	1
75	18	1
76	15	1
77	14	1
78	17	1
79	19	1
79	21	2
80	16	1
81	2	1
82	5	1
83	15	1
84	14	1
85	5	1
85	3	2
85	4	3
86	12	1
86	11	2
87	16	1
88	19	1
88	20	2
89	3	1
89	4	2
90	12	1
90	11	2
91	12	1
92	12	1
92	11	2
93	12	1
93	11	2
93	13	3
94	3	1
94	4	2
95	5	1
95	3	2
95	4	3
96	16	1
97	5	1
97	3	2
97	4	3
98	11	1
98	3	2
99	11	1
99	3	2
100	3	1
100	4	2
101	9	1
102	3	1
102	4	2
103	19	1
103	20	2
104	17	1
105	5	1
106	16	1
107	18	1
108	6	1
108	10	2
109	17	1
110	11	1
110	13	2
111	1	1
112	11	1
112	3	2
113	3	1
113	4	2
114	14	1
115	11	1
115	3	2
116	18	1
117	11	1
117	3	2
118	8	1
119	16	1
120	16	1
121	15	1
122	9	1
123	19	1
123	20	2
124	15	1
125	15	1
126	6	1
126	7	2
127	19	1
127	21	2
128	6	1
128	10	2
129	12	1
129	11	2
130	19	1
130	21	2
131	8	1
132	14	1
133	8	1
134	14	1
135	5	1
136	1	1
137	12	1
138	19	1
138	20	2
139	12	1
139	11	2
140	5	1
140	3	2
140	4	3
141	19	1
141	20	2
142	12	1
143	1	1
144	5	1
144	3	2
144	4	3
145	13	1
146	3	1
146	4	2
147	11	1
147	13	2
148	5	1
148	3	2
148	4	3
149	1	1
150	15	1
151	1	1
152	19	1
152	21	2
153	8	1
154	11	1
154	3	2
155	3	1
155	4	2
156	5	1
156	3	2
156	4	3
157	16	1
158	11	1
158	3	2
159	6	1
159	7	2
160	9	1
161	18	1
162	19	1
162	21	2
163	8	1
164	19	1
164	20	2
165	19	1
165	21	2
166	5	1
166	3	2
166	4	3
167	6	1
167	10	2
168	9	1
169	19	1
169	21	2
170	19	1
170	21	2
171	6	1
171	10	2
172	9	1
173	18	1
174	5	1
175	5	1
175	3	2
175	4	3
176	19	1
176	21	2
177	1	1
178	15	1
179	6	1
179	10	2
180	19	1
180	20	2
181	2	1
182	12	1
182	11	2
182	13	3
183	14	1
184	19	1
184	20	2
185	9	1
186	11	1
186	13	2
187	5	1
188	17	1
189	14	1
190	1	1
191	9	1
192	11	1
192	3	2
193	9	1
194	1	1
195	19	1
195	21	2
196	15	1
197	19	1
197	21	2
198	18	1
199	14	1
200	19	1
200	21	2
201	11	1
201	3	2
202	14	1
203	3	1
203	4	2
204	5	1
205	12	1
205	11	2
206	3	1
206	4	2
207	1	1
208	16	1
209	6	1
209	7	2
210	12	1
210	11	2
211	16	1
212	11	1
212	3	2
213	11	1
213	13	2
214	11	1
214	3	2
215	9	1
216	17	1
217	8	1
218	6	1
218	10	2
219	16	1
220	6	1
220	10	2
221	17	1
222	2	1
223	8	1
224	19	1
224	20	2
225	18	1
226	14	1
227	12	1
228	1	1
229	12	1
229	11	2
230	19	1
230	20	2
231	6	1
231	10	2
232	16	1
233	11	1
233	13	2
234	5	1
234	3	2
234	4	3
235	17	1
236	17	1
237	9	1
238	18	1
239	3	1
239	4	2
240	8	1
241	3	1
241	4	2
242	2	1
243	16	1
244	2	1
245	12	1
246	9	1
247	16	1
248	17	1
249	11	1
249	13	2
250	14	1
251	19	1
251	20	2
252	13	1
253	16	1
254	12	1
255	11	1
255	13	2
256	18	1
257	12	1
257	11	2
258	18	1
259	19	1
259	20	2
260	12	1
261	9	1
262	18	1
263	2	1
264	19	1
264	20	2
265	18	1
266	3	1
266	4	2
267	2	1
268	14	1
269	15	1
270	19	1
270	20	2
271	16	1
272	1	1
273	19	1
273	21	2
274	17	1
275	11	1
275	13	2
276	1	1
277	5	1
277	3	2
277	4	3
278	14	1
279	15	1
280	17	1
281	17	1
282	17	1
283	19	1
283	20	2
284	19	1
284	20	2
285	14	1
286	19	1
286	21	2
287	5	1
287	3	2
287	4	3
288	6	1
288	10	2
289	15	1
290	19	1
290	21	2
291	19	1
291	20	2
292	18	1
293	2	1
294	14	1
295	8	1
296	13	1
297	14	1
298	14	1
299	5	1
300	8	1
301	15	1
302	16	1
303	19	1
303	20	2
304	14	1
305	15	1
306	6	1
306	10	2
307	5	1
308	8	1
309	6	1
309	10	2
310	17	1
311	2	1
312	2	1
313	17	1
314	12	1
314	11	2
315	12	1
315	11	2
316	17	1
317	6	1
317	7	2
318	3	1
318	4	2
319	18	1
320	9	1
321	11	1
321	3	2
322	16	1
323	2	1
324	12	1
325	19	1
325	21	2
326	19	1
326	20	2
327	19	1
327	21	2
328	12	1
328	11	2
328	13	3
329	12	1
329	11	2
329	13	3
330	5	1
331	19	1
331	20	2
332	14	1
333	17	1
334	11	1
334	13	2
335	8	1
336	12	1
337	18	1
338	15	1
339	6	1
339	7	2
340	18	1
341	19	1
341	21	2
342	5	1
342	3	2
342	4	3
343	18	1
344	15	1
345	15	1
346	9	1
347	5	1
348	13	1
349	14	1
350	19	1
350	21	2
351	18	1
352	16	1
353	17	1
354	14	1
355	6	1
355	7	2
356	19	1
356	21	2
357	9	1
358	5	1
359	12	1
360	12	1
360	11	2
360	13	3
361	18	1
362	12	1
362	11	2
363	14	1
364	14	1
365	5	1
365	3	2
365	4	3
366	12	1
366	11	2
367	5	1
367	3	2
367	4	3
368	5	1
368	3	2
368	4	3
369	14	1
370	19	1
370	21	2
371	8	1
372	16	1
373	14	1
374	5	1
375	5	1
375	3	2
375	4	3
376	5	1
377	5	1
377	3	2
377	4	3
378	17	1
379	19	1
379	20	2
380	19	1
380	21	2
381	11	1
381	3	2
382	18	1
383	16	1
384	13	1
385	14	1
386	8	1
387	14	1
388	12	1
389	12	1
389	11	2
390	11	1
390	3	2
391	15	1
392	5	1
393	14	1
394	8	1
395	5	1
395	3	2
395	4	3
396	11	1
396	13	2
397	18	1
398	1	1
399	3	1
399	4	2
400	15	1
401	11	1
401	13	2
402	8	1
403	18	1
404	16	1
405	6	1
405	10	2
406	12	1
406	11	2
407	14	1
408	19	1
408	21	2
409	18	1
410	11	1
410	3	2
411	12	1
411	11	2
411	13	3
412	18	1
413	17	1
414	5	1
414	3	2
414	4	3
415	6	1
415	10	2
416	12	1
416	11	2
416	13	3
417	6	1
417	10	2
418	14	1
419	18	1
420	5	1
421	2	1
422	11	1
422	13	2
423	14	1
424	12	1
425	15	1
426	6	1
426	10	2
427	8	1
428	14	1
429	5	1
430	11	1
430	13	2
431	19	1
431	21	2
432	1	1
433	19	1
433	20	2
434	12	1
434	11	2
435	6	1
435	7	2
436	19	1
436	21	2
437	15	1
438	6	1
438	10	2
439	12	1
440	5	1
441	2	1
442	5	1
443	11	1
443	13	2
444	6	1
444	10	2
445	17	1
446	16	1
447	19	1
447	20	2
448	18	1
449	12	1
449	11	2
449	13	3
450	14	1
451	12	1
451	11	2
452	14	1
453	16	1
454	5	1
455	6	1
455	7	2
456	18	1
457	5	1
457	3	2
457	4	3
458	18	1
459	9	1
460	6	1
460	7	2
461	16	1
462	19	1
462	20	2
463	11	1
463	3	2
464	17	1
465	9	1
466	6	1
466	7	2
467	19	1
467	21	2
468	12	1
468	11	2
468	13	3
469	14	1
470	3	1
470	4	2
471	8	1
472	5	1
473	12	1
473	11	2
473	13	3
474	14	1
475	19	1
475	20	2
476	12	1
477	6	1
477	7	2
478	17	1
479	11	1
479	3	2
480	12	1
480	11	2
481	17	1
482	14	1
483	17	1
484	11	1
484	3	2
485	1	1
486	2	1
487	9	1
488	1	1
489	2	1
490	17	1
491	14	1
492	18	1
493	2	1
494	17	1
495	6	1
495	7	2
496	14	1
497	14	1
498	15	1
499	6	1
499	10	2
500	16	1
501	11	1
501	13	2
502	16	1
503	6	1
503	10	2
504	18	1
505	12	1
506	6	1
506	7	2
507	1	1
508	19	1
508	21	2
509	13	1
510	12	1
510	11	2
510	13	3
511	16	1
512	18	1
513	2	1
514	18	1
515	12	1
516	19	1
516	20	2
517	12	1
518	5	1
519	2	1
520	13	1
521	8	1
522	17	1
523	19	1
523	21	2
524	1	1
525	19	1
525	21	2
526	12	1
526	11	2
527	9	1
528	11	1
528	3	2
529	12	1
529	11	2
529	13	3
530	12	1
531	11	1
531	13	2
532	11	1
532	3	2
533	8	1
534	11	1
534	3	2
535	17	1
536	19	1
536	21	2
537	11	1
537	13	2
538	19	1
538	20	2
539	17	1
540	14	1
541	8	1
542	2	1
543	18	1
544	6	1
544	7	2
545	5	1
546	12	1
546	11	2
546	13	3
547	3	1
547	4	2
548	16	1
549	14	1
550	11	1
550	3	2
551	18	1
552	5	1
553	6	1
553	7	2
554	17	1
555	19	1
555	20	2
556	13	1
557	11	1
557	3	2
558	6	1
558	7	2
559	18	1
560	6	1
560	10	2
561	19	1
561	21	2
562	18	1
563	19	1
563	20	2
564	2	1
565	11	1
565	3	2
566	6	1
566	10	2
567	9	1
568	2	1
569	16	1
570	14	1
571	12	1
571	11	2
571	13	3
572	3	1
572	4	2
573	16	1
574	2	1
575	16	1
576	19	1
576	21	2
577	19	1
577	20	2
578	8	1
579	13	1
580	15	1
581	8	1
582	19	1
582	21	2
583	2	1
584	14	1
585	17	1
586	18	1
587	11	1
587	13	2
588	11	1
588	3	2
589	11	1
589	3	2
590	16	1
591	1	1
592	3	1
592	4	2
593	19	1
593	21	2
594	16	1
595	17	1
596	6	1
596	7	2
597	17	1
598	2	1
599	18	1
600	5	1
601	3	1
601	4	2
602	5	1
603	15	1
604	1	1
605	19	1
605	21	2
606	5	1
607	13	1
608	6	1
608	7	2
609	12	1
609	11	2
610	6	1
610	10	2
611	5	1
612	13	1
613	16	1
614	14	1
615	19	1
615	20	2
616	17	1
617	11	1
617	13	2
618	17	1
619	16	1
620	5	1
621	12	1
621	11	2
621	13	3
622	3	1
622	4	2
623	17	1
624	11	1
624	3	2
625	17	1
626	11	1
626	3	2
627	14	1
628	6	1
628	7	2
629	15	1
630	5	1
631	11	1
631	13	2
632	18	1
633	16	1
634	19	1
634	21	2
635	1	1
636	13	1
637	14	1
638	5	1
639	19	1
639	21	2
640	14	1
641	14	1
642	19	1
642	21	2
643	19	1
643	21	2
644	14	1
645	17	1
646	14	1
647	19	1
647	20	2
648	14	1
649	14	1
650	14	1
651	14	1
652	2	1
653	17	1
654	1	1
655	19	1
655	21	2
656	15	1
657	8	1
658	1	1
659	6	1
659	7	2
660	12	1
660	11	2
660	13	3
661	18	1
662	8	1
663	14	1
664	19	1
664	21	2
665	15	1
666	11	1
666	3	2
667	3	1
667	4	2
668	1	1
669	11	1
669	3	2
670	16	1
671	9	1
672	5	1
672	3	2
672	4	3
673	13	1
674	19	1
674	20	2
675	2	1
676	17	1
677	18	1
678	19	1
678	20	2
679	17	1
680	5	1
681	1	1
682	19	1
682	21	2
683	8	1
684	16	1
685	11	1
685	13	2
686	19	1
686	21	2
687	19	1
687	21	2
688	12	1
688	11	2
688	13	3
689	5	1
690	9	1
691	17	1
692	14	1
693	5	1
693	3	2
693	4	3
694	8	1
695	11	1
695	3	2
696	18	1
697	16	1
698	8	1
699	6	1
699	10	2
700	5	1
700	3	2
700	4	3
701	3	1
701	4	2
702	14	1
703	6	1
703	10	2
704	16	1
705	6	1
705	10	2
706	3	1
706	4	2
707	8	1
708	6	1
708	7	2
709	19	1
709	21	2
710	15	1
711	8	1
712	11	1
712	3	2
713	5	1
714	17	1
715	19	1
715	21	2
716	6	1
716	10	2
717	1	1
718	14	1
719	5	1
719	3	2
719	4	3
720	14	1
721	17	1
722	8	1
723	9	1
724	11	1
724	13	2
725	18	1
726	14	1
727	15	1
728	5	1
728	3	2
728	4	3
729	2	1
730	9	1
731	18	1
732	11	1
732	3	2
733	12	1
734	1	1
735	15	1
736	12	1
737	11	1
737	3	2
738	19	1
738	21	2
739	5	1
739	3	2
739	4	3
740	8	1
741	6	1
741	7	2
742	9	1
743	19	1
743	20	2
744	14	1
745	16	1
746	3	1
746	4	2
747	11	1
747	13	2
748	11	1
748	13	2
749	8	1
750	6	1
750	7	2
751	14	1
752	19	1
752	20	2
753	19	1
753	21	2
754	19	1
754	20	2
755	12	1
756	2	1
757	5	1
758	8	1
759	3	1
759	4	2
760	13	1
761	14	1
762	13	1
763	17	1
764	5	1
765	3	1
765	4	2
766	6	1
766	7	2
767	12	1
768	17	1
769	5	1
769	3	2
769	4	3
770	6	1
770	10	2
771	5	1
771	3	2
771	4	3
772	14	1
773	16	1
774	11	1
774	13	2
775	19	1
775	20	2
776	18	1
777	19	1
777	21	2
778	14	1
779	16	1
780	2	1
781	1	1
782	1	1
783	17	1
784	6	1
784	10	2
785	6	1
785	7	2
786	11	1
786	13	2
787	14	1
788	12	1
788	11	2
788	13	3
789	16	1
790	6	1
790	7	2
791	16	1
792	2	1
793	18	1
794	8	1
795	8	1
796	3	1
796	4	2
797	17	1
798	19	1
798	21	2
799	19	1
799	20	2
800	1	1
801	15	1
802	1	1
803	3	1
803	4	2
804	6	1
804	7	2
805	13	1
806	11	1
806	3	2
807	1	1
808	6	1
808	7	2
809	15	1
810	11	1
810	3	2
811	19	1
811	20	2
812	19	1
812	20	2
813	12	1
813	11	2
814	5	1
814	3	2
814	4	3
815	16	1
816	12	1
817	3	1
817	4	2
818	6	1
818	7	2
819	1	1
820	14	1
821	9	1
822	15	1
823	18	1
824	6	1
824	10	2
825	5	1
826	19	1
826	21	2
827	15	1
828	12	1
828	11	2
829	3	1
829	4	2
830	8	1
831	12	1
831	11	2
831	13	3
832	19	1
832	21	2
833	11	1
833	13	2
834	19	1
834	20	2
835	16	1
836	12	1
836	11	2
837	16	1
838	5	1
839	3	1
839	4	2
840	15	1
841	19	1
841	20	2
842	12	1
842	11	2
843	19	1
843	21	2
844	2	1
845	12	1
845	11	2
846	17	1
847	1	1
848	16	1
849	15	1
850	13	1
851	15	1
852	13	1
853	1	1
854	5	1
854	3	2
854	4	3
855	13	1
856	18	1
857	14	1
858	12	1
858	11	2
858	13	3
859	15	1
860	8	1
861	18	1
862	19	1
862	21	2
863	12	1
863	11	2
863	13	3
864	9	1
865	2	1
866	5	1
866	3	2
866	4	3
867	3	1
867	4	2
868	14	1
869	18	1
870	19	1
870	21	2
871	2	1
872	15	1
873	18	1
874	12	1
874	11	2
874	13	3
875	3	1
875	4	2
876	18	1
877	14	1
878	5	1
878	3	2
878	4	3
879	18	1
880	19	1
880	20	2
881	15	1
882	19	1
882	21	2
883	19	1
883	20	2
884	12	1
884	11	2
885	12	1
885	11	2
885	13	3
886	19	1
886	20	2
887	6	1
887	10	2
888	2	1
889	3	1
889	4	2
890	18	1
891	12	1
891	11	2
891	13	3
892	19	1
892	21	2
893	5	1
893	3	2
893	4	3
894	14	1
895	12	1
895	11	2
896	1	1
897	19	1
897	20	2
898	11	1
898	13	2
899	19	1
899	20	2
900	14	1
901	19	1
901	21	2
902	2	1
903	19	1
903	21	2
904	16	1
905	12	1
906	12	1
907	6	1
907	7	2
908	17	1
909	11	1
909	3	2
910	11	1
910	13	2
911	11	1
911	13	2
912	2	1
913	19	1
913	21	2
914	16	1
915	3	1
915	4	2
916	3	1
916	4	2
917	18	1
918	16	1
919	12	1
920	8	1
921	8	1
922	3	1
922	4	2
923	1	1
924	6	1
924	10	2
925	14	1
926	6	1
926	7	2
927	14	1
928	1	1
929	19	1
929	21	2
930	1	1
931	8	1
932	19	1
932	20	2
933	19	1
933	20	2
934	9	1
935	13	1
936	12	1
936	11	2
936	13	3
937	5	1
937	3	2
937	4	3
938	5	1
939	19	1
939	20	2
940	19	1
940	21	2
941	17	1
942	19	1
942	21	2
943	12	1
944	14	1
945	12	1
945	11	2
945	13	3
946	5	1
946	3	2
946	4	3
947	11	1
947	13	2
948	12	1
948	11	2
949	15	1
950	2	1
951	6	1
951	7	2
952	6	1
952	7	2
953	15	1
954	6	1
954	7	2
955	16	1
956	15	1
957	8	1
958	8	1
959	8	1
960	19	1
960	21	2
961	5	1
962	19	1
962	21	2
963	11	1
963	13	2
964	12	1
964	11	2
964	13	3
965	12	1
965	11	2
966	19	1
966	20	2
967	11	1
967	13	2
968	5	1
969	17	1
970	5	1
971	17	1
972	5	1
973	18	1
974	1	1
975	11	1
975	3	2
976	5	1
976	3	2
976	4	3
977	12	1
977	11	2
978	16	1
979	19	1
979	21	2
980	12	1
980	11	2
980	13	3
981	5	1
982	19	1
982	20	2
983	16	1
984	19	1
984	20	2
985	1	1
986	14	1
987	11	1
987	13	2
988	17	1
989	12	1
990	19	1
990	21	2
991	9	1
992	6	1
992	10	2
993	6	1
993	7	2
994	8	1
995	1	1
996	19	1
996	20	2
997	5	1
998	11	1
998	13	2
999	1	1
1000	12	1
1001	8	1
1002	16	1
1003	9	1
1004	11	1
1004	3	2
1005	17	1
1006	8	1
1007	15	1
1008	3	1
1008	4	2
1009	16	1
1010	12	1
1010	11	2
1010	13	3
1011	1	1
1012	9	1
1013	16	1
1014	14	1
1015	5	1
1015	3	2
1015	4	3
1016	12	1
1016	11	2
1017	12	1
1018	18	1
1019	5	1
1020	19	1
1020	20	2
1021	2	1
1022	15	1
1023	9	1
1024	5	1
1025	12	1
1025	11	2
1026	9	1
1027	16	1
1028	11	1
1028	13	2
1029	19	1
1029	20	2
1030	6	1
1030	7	2
1031	17	1
1032	14	1
1033	8	1
1034	18	1
1035	16	1
1036	2	1
1037	13	1
1038	19	1
1038	20	2
1039	19	1
1039	20	2
1040	18	1
1041	19	1
1041	20	2
1042	12	1
1042	11	2
1043	6	1
1043	10	2
1044	16	1
1045	16	1
1046	6	1
1046	10	2
1047	14	1
1048	5	1
1048	3	2
1048	4	3
\.


--
-- Data for Name: winery; Type: TABLE DATA; Schema: public; Owner: -
--

COPY winery (winery_id, winery_name, region_id) FROM stdin;
1	Hanshaw Estates Winery	2
2	De Morton and Sons Wines	5
3	Jones's Premium Wines	3
4	Borg Daze Premium Wines	5
5	Binns Group	6
6	Davie Brook Vineyard	3
7	Eglington Creek Premium Wines	4
8	McKay Station Vineyard	4
9	Dennis and Sons Wines	5
10	Beard Brothers Vineyard	4
11	Rowley Brook Group	10
12	Borg Creek	10
13	Bickley Station	6
14	Ryan Estates Premium Wines	7
15	Waugh	4
16	Rogerson Station Group	9
17	Grabowski's Vineyard	10
18	Greenfield Brothers	2
19	Anderson and Sons Premium Wines	4
20	Durham Hill Winery	8
21	Parker Station	4
22	Durham Brook Group	3
23	Buettner Hill	9
24	Grabowski Gully Premium Wines	9
25	Borg Creek Vineyard	6
26	Williams's Wines	7
27	Jones Gully Vineyard	2
28	Bickley and Sons Premium Wines	5
29	Durham Ridge	7
30	Rogerson and Sons	3
31	Rowley Gully Group	4
32	Bell Daze Wines	9
33	Lane Gully Premium Wines	5
34	Gosk Estates Vineyard	10
35	Rowley Brothers Vineyard	8
36	Lane Ridge Group	2
37	De Morton Wines	7
38	Bickley Vineyard	8
39	Gosk Ridge Wines	5
40	Hayne Creek	8
41	Dennis's	2
42	Ryan Brothers Winery	10
43	Rowley Vineyard	10
44	Grabowski Brook Winery	8
45	Beard Hill Vineyard	9
46	Grabowski Station Group	10
47	Eglington Ridge Winery	8
48	Greenfield Vineyard	9
49	Jones and Sons Winery	5
50	Anderson Daze Group	3
51	Anderson Station Winery	6
52	Parker Creek Wines	4
53	Dennis's Wines	6
54	Pearce and Sons Winery	5
55	Macdonald Brook Vineyard	7
56	Hanshaw Estates	6
57	Pearce Daze Vineyard	2
58	Anderson and Sons Wines	4
59	Rowley Creek	5
60	Pearce Creek Wines	5
61	Macdonald Estates Vineyard	5
62	Doswell Hill Premium Wines	2
63	Anderson Daze Wines	7
64	Pearce Ridge Wines	10
65	Eglington Gully Vineyard	3
66	Borg Brothers Group	4
67	Doswell Vineyard	7
68	Bell Daze Premium Wines	7
69	Hanshaw Ridge	8
70	Lane Estates Group	3
71	Grehan Group	4
72	Macdonald and Sons	8
73	Hubel Daze Winery	8
74	Rowley Gully Wines	7
75	Lord Gully Premium Wines	9
76	Gosk	10
77	Binns Hill	10
78	Bickley Daze Group	9
79	Durham Station	3
80	Eglington Ridge Vineyard	5
81	Binns Wines	4
82	Grehan Daze	5
83	De Morton Vineyard	7
84	Williams Estates Group	8
85	Ryan Gully Vineyard	9
86	Davie Brook Winery	8
87	Scally's Group	9
88	Pearce Brook Wines	5
89	Jones Brook Winery	5
90	Lord Daze	7
91	De Morton Station Wines	3
92	Williams Daze Vineyard	6
93	Beard Ridge Premium Wines	2
94	Gosk Group	6
95	Binns Premium Wines	2
96	Scally Gully Wines	5
97	Borg Gully Winery	3
98	Lane's Vineyard	5
99	De Morton and Sons Vineyard	5
100	Dennis Brook Group	2
101	Bell	5
102	Eglington and Sons Vineyard	3
103	Jones Creek Wines	10
104	Jones Creek Group	5
105	Rowley Station	2
106	Scally Brothers	9
107	Buonopane Estates Wines	4
108	Lord Station Vineyard	6
109	Macdonald Gully Vineyard	5
110	Grehan's Vineyard	2
111	Dennis and Sons Premium Wines	2
112	Buettner Station Vineyard	9
113	Buettner Estates	10
114	Buettner Creek Wines	4
115	Ryan Estates Vineyard	8
116	Gosk Creek Premium Wines	9
117	Durham Creek	9
118	Grehan Brothers Wines	4
119	Ryan Hill	9
120	Buettner Daze Premium Wines	3
121	Ryan Ridge Winery	8
122	Borg Hill Premium Wines	9
123	Macdonald Brothers Wines	8
124	Bickley and Sons Vineyard	3
125	Gosk Brothers Group	5
126	Falconer Brook Vineyard	9
127	Falconer's Vineyard	2
128	Hanshaw Brothers Group	10
129	Bickley Estates Wines	10
130	Lord Vineyard	2
131	Grabowski Ridge	6
132	Grabowski Brothers	3
133	Buonopane Vineyard	2
134	Bickley Brothers	7
135	Dennis Hill Vineyard	6
136	Scally Station Premium Wines	2
137	Greenfield's Group	4
138	Greenfield Ridge Premium Wines	9
139	Greenfield Wines	5
140	Greenfield Group	5
141	Rowley Estates Vineyard	9
142	Doswell Daze Wines	4
143	Lane Creek Premium Wines	5
144	Buonopane Brothers Wines	5
145	Rogerson Ridge	8
146	Bickley Gully	3
147	Borg Premium Wines	5
148	Grabowski Vineyard	7
149	Buettner	2
150	Ryan Daze Winery	3
151	Buonopane Creek Winery	9
152	Pearce Estates Winery	8
153	De Morton and Sons Winery	4
154	Williams Creek Premium Wines	2
155	Beard Brothers Premium Wines	10
156	Gosk Gully	4
157	Durham Ridge Winery	9
158	Jones Ridge Wines	5
159	Anderson Brothers Group	3
160	Hayne Hill Winery	5
161	Jones Daze Wines	4
162	Williams and Sons Wines	4
163	De Morton Creek Winery	2
164	Lord Estates Winery	10
165	Falconer Daze Premium Wines	2
166	Buonopane and Sons Group	8
167	Hanshaw Hill	5
168	Davie Hill Wines	9
169	Greenfield Brothers Winery	3
170	Macdonald's Group	7
171	Jones Creek	10
172	Grabowski Gully Vineyard	10
173	Ryan and Sons Group	10
174	Hayne Wines	6
175	Jones	8
176	Binns Hill Vineyard	4
177	Greenfield Estates	7
178	Greenfield Estates Winery	3
179	Korab Estates Premium Wines	5
180	Falconer	2
181	Anderson's Wines	7
182	Waugh Ridge Winery	10
183	Lord Ridge Vineyard	10
184	Lord Daze Vineyard	10
185	Rowley Hill Wines	4
186	Waugh Gully Vineyard	4
187	Korab Hill Vineyard	9
188	Lane Wines	6
189	Waugh Estates Wines	2
190	Borg's	7
191	Jones Brook Vineyard	3
192	Bickley's	4
193	Grehan Ridge Winery	6
194	Davie Station Group	5
195	De Morton Hill Group	7
196	Anderson Creek Group	8
197	Waugh Brook Premium Wines	8
198	Hayne Brothers Premium Wines	6
199	Rogerson Gully Premium Wines	9
200	Buettner's Premium Wines	2
201	Doswell Estates Vineyard	10
202	Grehan Creek	5
203	Doswell Premium Wines	2
204	Rogerson Gully	7
205	Parker Station Vineyard	9
206	Ryan and Sons Vineyard	2
207	Hubel Ridge Winery	3
208	Grehan Station Vineyard	4
209	Borg Brook Winery	3
210	Rogerson Estates Winery	6
211	Durham Estates	9
212	Rowley Premium Wines	10
213	Anderson's Winery	10
214	Rogerson's Group	5
215	Doswell Brook	4
216	Waugh Gully Wines	4
217	Durham and Sons Premium Wines	8
218	Scally Daze Vineyard	6
219	Gosk and Sons Wines	2
220	Eglington Winery	7
221	Bickley Hill Winery	5
222	McKay and Sons Group	7
223	Hanshaw Station Vineyard	7
224	Macdonald Hill Wines	3
225	Scally Gully	4
226	Grehan Gully	6
227	Scally Winery	3
228	Rowley Ridge Winery	9
229	Hayne Daze Group	6
230	Durham Premium Wines	3
231	Beard and Sons Vineyard	4
232	Hanshaw and Sons	7
233	Doswell Estates Premium Wines	8
234	Rowley Ridge Group	7
235	Parker Brothers Winery	7
236	Hayne Station Group	10
237	McKay Ridge Winery	9
238	Scally Hill Vineyard	6
239	Anderson Ridge Wines	6
240	Hayne's Premium Wines	8
241	Anderson's Group	3
242	Lord's Winery	7
243	Macdonald Hill Vineyard	9
244	Lord Ridge	4
245	Binns Station Group	5
246	McKay Ridge Group	10
247	Durham Hill Vineyard	5
248	Buettner Daze Winery	10
249	Hayne Vineyard	10
250	Rogerson Estates Wines	2
251	Davie Hill Premium Wines	3
252	Grehan's Group	2
253	McKay and Sons	2
254	Borg and Sons	8
255	Grabowski and Sons Wines	6
256	Davie Brook Group	9
257	Waugh Creek	5
258	Bell Winery	8
259	Gosk Winery	7
260	Dennis Winery	9
261	Williams Brothers Vineyard	3
262	Rowley Creek Winery	2
263	Rogerson and Sons Winery	4
264	Bickley Ridge Winery	5
265	Binns Creek Premium Wines	5
266	Rogerson Hill Group	4
267	Rowley Daze Premium Wines	3
268	Borg Gully	6
269	Macdonald Ridge Vineyard	10
270	Dennis Gully Wines	3
271	Borg Brothers	4
272	Pearce Ridge Premium Wines	7
273	Binns Daze Premium Wines	4
274	Scally Brothers Group	8
275	Waugh Brothers Vineyard	10
276	Lane Hill Wines	3
277	Williams Creek Group	4
278	Korab Estates Winery	6
279	Anderson Daze Vineyard	9
280	Doswell Gully Winery	8
281	Williams Brook Premium Wines	7
282	Beard Daze Winery	9
283	Hubel's	9
284	Parker Hill Premium Wines	5
285	Eglington Vineyard	2
286	Grabowski Estates Winery	8
287	Buonopane Brook Winery	6
288	Rogerson's Winery	7
289	Greenfield Brothers Wines	2
290	Durham	2
291	Korab Station Wines	8
292	De Morton Gully Vineyard	6
293	Pearce Winery	7
294	McKay Daze Vineyard	9
295	Doswell Gully Wines	9
296	Jones Estates	9
297	Ryan	5
298	Rowley Brook Winery	3
299	Grabowski Creek Vineyard	10
300	Ryan Estates Group	3
\.


--
-- Name: countries_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY countries
    ADD CONSTRAINT countries_pkey PRIMARY KEY (country_id);


--
-- Name: customer_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY customer
    ADD CONSTRAINT customer_pkey PRIMARY KEY (cust_id);


--
-- Name: grape_variety_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY grape_variety
    ADD CONSTRAINT grape_variety_pkey PRIMARY KEY (variety_id);


--
-- Name: inventory_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY inventory
    ADD CONSTRAINT inventory_pkey PRIMARY KEY (wine_id, inventory_id);


--
-- Name: items_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (cust_id, order_id, item_id);


--
-- Name: orders_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY orders
    ADD CONSTRAINT orders_pkey PRIMARY KEY (cust_id, order_id);


--
-- Name: region_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY region
    ADD CONSTRAINT region_pkey PRIMARY KEY (region_id);


--
-- Name: titles_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY titles
    ADD CONSTRAINT titles_pkey PRIMARY KEY (title_id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_name);


--
-- Name: wine_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wine
    ADD CONSTRAINT wine_pkey PRIMARY KEY (wine_id);


--
-- Name: wine_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wine_type
    ADD CONSTRAINT wine_type_pkey PRIMARY KEY (wine_type_id);


--
-- Name: wine_variety_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY wine_variety
    ADD CONSTRAINT wine_variety_pkey PRIMARY KEY (wine_id, variety_id);


--
-- Name: winery_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY winery
    ADD CONSTRAINT winery_pkey PRIMARY KEY (winery_id);

--
-- PostgreSQL database dump complete
--
