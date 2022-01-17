DROP TABLE IF EXISTS vt_employees;
DROP TABLE IF EXISTS vt_positions;

CREATE TABLE vt_employees (
    "name" text NOT NULL,
    "salary" integer NOT NULL,
    "position_id" integer NOT NULL,
    "valid_from" timestamp with time zone NOT NULL,
    "valid_to" timestamp with time zone NOT NULL
);

CREATE TABLE vt_positions (
  "position_id" SERIAL,
  "position" text
);
