create table if not exists top_comments (
  "id" int primary key not null,
  "text" text,
  origin_table text,
  origin_column text,
  origin_row int);

insert into top_comments (id, text, origin_table, origin_column, origin_row)
values (1, 'foo', 'bar', 'baz', 5);

insert into top_comments (id, text, origin_table, origin_column, origin_row)
values (2, 'qux', 'bar', 'baz', 7);

insert into top_comments (id, text, origin_table, origin_column, origin_row)
values (3, 'quuix', 'fasl', 'lror', 42);
