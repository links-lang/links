DROP TABLE IF EXISTS contacts;
DROP TABLE IF EXISTS departments;
DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS tasks;

CREATE TABLE contacts (
    dept   text,
    name   text primary key,
    client boolean
) WITH OIDS;

CREATE TABLE departments (
    name   text primary key
) WITH OIDS;

CREATE TABLE employees (
    dept   text,
    name   text primary key,
    salary integer
) WITH OIDS;

CREATE TABLE tasks (
    employee text,
    task     text
    -- The setup code does not respect this. Do we care? 
    -- , PRIMARY KEY(employee, task)
) WITH OIDS;
