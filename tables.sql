CREATE TABLE registration (

    id SERIAL PRIMARY KEY,

    firstname TEXT DEFAULT '' NOT NULL,
    lastname TEXT DEFAULT '' NOT NULL,
    dob TEXT DEFAULT '' NOT NULL
);
