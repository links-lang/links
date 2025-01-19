-- wordlist : for use with the classic dict-suggest.links

create table wordlist (
  word varchar(50),
  type varchar(50),
  meaning text,
  primary key (word, type, meaning)
);

-- definitions : for use with the fancier, editable
-- dict-suggest-update.links

create sequence def_id_seq;
create table definitions (id bigint default (nextval('def_id_seq')), word varchar(80), meaning text);
