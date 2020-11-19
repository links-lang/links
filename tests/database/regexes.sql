DROP TABLE IF EXISTS staff;
DROP TABLE IF EXISTS depts;

CREATE TABLE staff (
    name text,
    dept text
);

CREATE TABLE depts (
    name text,
    coffee_budget int
);
