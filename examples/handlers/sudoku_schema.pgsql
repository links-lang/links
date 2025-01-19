-- Postgres schema for Links Sudoku application
CREATE TABLE puzzles (
  id   SERIAL                PRIMARY KEY,
  grid character varying(81) NOT NULL
);
