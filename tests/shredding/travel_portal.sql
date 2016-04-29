DROP TABLE IF EXISTS agencies;
DROP TABLE IF EXISTS externaltours;

CREATE TABLE agencies (
    id       integer primary key,
    name     text,
    based_in text,
    phone    text
) WITH OIDS;

CREATE TABLE externaltours (
    id          integer primary key,
    name        text,
    destination text,
    type        text,
    price       integer
) WITH OIDS;

insert into agencies (id, name, based_in, phone) values
  (1, 'BayTours', 'San Francisco', '415-1200'),
  (2, 'HarborCruz', 'Santa Cruz', '831-3000');

insert into externaltours (id, name, destination, type, price) values
  (3, 'BayTours', 'San Francisco', 'cable car', 50),
  (4, 'BayTours', 'Santa Cruz', 'bus', 100),
  (5, 'BayTours', 'Santa Cruz', 'boat', 250),
  (6, 'BayTours', 'Monterey', 'boat', 400),
  (7, 'HarborCruz', 'Monterey', 'boat', 200),
  (8, 'HarborCruz', 'Carmel', 'train', 90);
