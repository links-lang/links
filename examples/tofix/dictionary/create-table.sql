create table wordlist (
word varchar(255),
type varchar (20),
meaning text
);
create index wordlist_word_index on wordlist (word);