
DROP TABLE IF EXISTS Albums;

CREATE TABLE Albums (
	Album VARCHAR(50) NOT NULL,
	Quantity INT NOT NULL,
	PRIMARY KEY (Album)
);

DROP TABLE IF EXISTS Tracks;

CREATE TABLE Tracks (
	Track VARCHAR(50) NOT NULL,
	Date INT NOT NULL,
	Rating INT NOT NULL,
	Album VARCHAR(50) NOT NULL,
	PRIMARY KEY (Track, Album)
);

INSERT INTO Albums (Album, Quantity) VALUES
	('Disintegration', 6),
	('Show', 3),
	('Galore', 1),
	('Paris', 4),
	('Wish', 5);

INSERT INTO Tracks (Track, Date, Rating, Album) VALUES
	('Lullaby', 1989, 3, 'Galore'),
	('Lullaby', 1989, 3, 'Show'),
	('Lovesong', 1989, 5, 'Galore'),
	('Lovesong', 1989, 5, 'Paris'),
	('Trust', 1992, 4, 'Wish');

