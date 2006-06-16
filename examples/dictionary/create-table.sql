create table wordlist (
word varchar(255),
type varchar (40),
meaning text
);
create index wordlist_word_index on wordlist (word);
