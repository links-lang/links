DROP TABLE IF EXISTS contacts CASCADE;
DROP TABLE IF EXISTS departments CASCADE;
DROP TABLE IF EXISTS employees CASCADE;
DROP TABLE IF EXISTS tasks CASCADE;

CREATE TABLE contacts (
    id     integer primary key,
    dept   text,
    name   text,
    client boolean
);

CREATE TABLE departments (
    id     integer primary key,
    name   text
);

CREATE TABLE employees (
    id     integer primary key,
    dept   text,
    name   text,
    salary integer
);

CREATE TABLE tasks (
    id       integer primary key,
    employee text,
    task     text
);
