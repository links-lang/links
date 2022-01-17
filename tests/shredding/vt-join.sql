DROP TABLE IF EXISTS vtj_employees;
DROP TABLE IF EXISTS vtj_departments;
DROP TABLE IF EXISTS vtj_salaries;

CREATE TABLE vtj_departments (
    department_id SERIAL,
    name text,
    PRIMARY KEY(department_id)
);

CREATE TABLE vtj_employees (
    name text,
    salary_band text,
    department_id int,
    valid_from timestamptz,
    valid_to timestamptz,
    FOREIGN KEY (department_id) REFERENCES vtj_departments(department_id)
);

CREATE TABLE vtj_salaries (
    salary_band text,
    salary int,
    valid_from timestamptz,
    valid_to timestamptz
);
