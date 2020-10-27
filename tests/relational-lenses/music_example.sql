DROP TABLE IF EXISTS albums;

CREATE TABLE albums (
    album VARCHAR(50) NOT NULL,
    quantity INT NOT NULL,
    PRIMARY KEY (album)
);

DROP TABLE IF EXISTS tracks;

CREATE TABLE tracks (
    track VARCHAR(50) NOT NULL,
    date INT NOT NULL,
    rating INT NOT NULL,
    album VARCHAR(50) NOT NULL,
    PRIMARY KEY (track, album)
);

INSERT INTO albums (album, quantity) VALUES
    ('Disintegration', 6),
    ('Show', 3),
    ('Galore', 1),
    ('Paris', 4),
    ('Wish', 5);

INSERT INTO tracks (track, date, rating, album) VALUES
    ('Lullaby', 1989, 3, 'Galore'),
    ('Lullaby', 1989, 3, 'Show'),
    ('Lovesong', 1989, 5, 'Galore'),
    ('Lovesong', 1989, 5, 'Paris'),
    ('Trust', 1992, 4, 'Wish');

