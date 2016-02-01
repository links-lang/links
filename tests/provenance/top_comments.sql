DROP TABLE IF EXISTS top_comments;

CREATE TABLE top_comments (
    id integer primary key,
    text text,
    origin_table text,
    origin_column text,
    origin_row integer
);

INSERT INTO top_comments VALUES (1, 'foo', 'bar', 'baz', 5);
INSERT INTO top_comments VALUES (2, 'qux', 'bar', 'baz', 7);
INSERT INTO top_comments VALUES (3, 'quuix', 'fasl', 'lror', 42);
