DROP TABLE IF EXISTS clients;
DROP TABLE IF EXISTS marketers;
DROP TABLE IF EXISTS mc;

CREATE TABLE clients (
    id     integer primary key
);

CREATE TABLE marketers (
    name   text
);

CREATE TABLE mc (
    m      text,
    c      integer
);

INSERT INTO clients VALUES (1);
INSERT INTO clients VALUES (2);
INSERT INTO clients VALUES (3);
INSERT INTO clients VALUES (42);

INSERT INTO marketers VALUES ('a');
INSERT INTO marketers VALUES ('b');
INSERT INTO marketers VALUES ('c');

INSERT INTO mc VALUES ('a', 1);
INSERT INTO mc VALUES ('c', 1);
INSERT INTO mc VALUES ('c', 2);
INSERT INTO mc VALUES ('c', 3);


