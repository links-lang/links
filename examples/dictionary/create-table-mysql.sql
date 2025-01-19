create table wordlist (
word varchar(50),
type varchar(50),
meaning text,
primary key (word, type, meaning(100))
);

