DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS positions;

CREATE TABLE employees (
    "name" text NOT NULL,
    "salary" integer NOT NULL,
    "position_id" integer NOT NULL,
    "valid_from" timestamp with time zone NOT NULL,
    "valid_to" timestamp with time zone NOT NULL
);

CREATE TABLE positions (
  "position_id" SERIAL,
  "position" text
);
