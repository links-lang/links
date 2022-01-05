DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS departments;
DROP TABLE IF EXISTS salaries;

CREATE TABLE departments (
    department_id SERIAL,
    name text,
    PRIMARY KEY(department_id)
);

CREATE TABLE employees (
    name text,
    salary_band text,
    department_id int,
    valid_from timestamptz,
    valid_to timestamptz,
    FOREIGN KEY (department_id) REFERENCES departments(department_id)
);

CREATE TABLE salaries (
    salary_band text,
    salary int,
    valid_from timestamptz,
    valid_to timestamptz
);
