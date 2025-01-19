-- This is a bit harsh, but making it run in both CI and locally is a
-- bit difficult.
drop table if exists xml;

create table xml (
  id int primary key,
  parent int,
  name text,
  pre int,
  post int
  );

insert into xml (id, parent, name, pre, post) values
  (0, -1, '#doc', 0, 13),
  (1, 0, 'a', 1, 12),
  (2, 1, 'b', 2, 5),
  (3, 2, 'c', 3, 4),
  (4, 1, 'd', 6, 11),
  (5, 4, 'e', 7, 8),
  (6, 4, 'f', 9, 10);
