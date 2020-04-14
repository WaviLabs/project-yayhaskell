CREATE TYPE source AS ENUM ('Arxiv', 'HackerNews', 'Reddit', 'Yay');

CREATE TABLE IF NOT EXISTS users
( id SERIAL PRIMARY KEY
-- TODO: Make (email,_hash) primary key
, email TEXT UNIQUE NOT NULL
, _hash TEXT UNIQUE NOT NULL
, salt TEXT NOT NULL
, lambdas INT NOT NULL
);

CREATE TABLE IF NOT EXISTS posts
( id SERIAL PRIMARY KEY
, userId SERIAL REFERENCES users(id)
, createdAt BIGINT NOT NULL
, source source NOT NULL
, title TEXT NOT NULL
, link TEXT NOT NULL
, comments_link TEXT NOT NULL
, lambdas INT NOT NULL
);

CREATE TABLE IF NOT EXISTS comments
( id SERIAL PRIMARY KEY
, userId SERIAL REFERENCES users(id)
, postId SERIAL REFERENCES posts(id)
, editedAt BIGINT
, content TEXT NOT NULL
, link TEXT NOT NULL
, lambdas INT NOT NULL
);
