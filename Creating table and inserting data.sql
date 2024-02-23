-- One example of creating a table within a database, then inserting data into it --

CREATE TABLE product2(
type_id INTEGER NOT NULL,
name VARCHAR(30) NOT NULL,
supplier VARCHAR(30) NOT NULL,
description TEXT,
id SERIAL PRIMARY KEY);		


INSERT INTO product2 VALUES
(1, 'Nike', 'John Davies', 'Classic shoe'),
(1, 'Nike', 'John Davies', 'Classic shoe'),
(1, 'Adidas', 'Greg Culbert', 'Old school shoe'),
(1, 'Adidas', 'Greg Culbert', 'Old school shoe'),
(2, 'Puma', 'Ben Talbort', 'Very old school'),
(2, 'Puma', 'Ben Talbort', 'Very old school'),
(2, 'Puma', 'Ben Talbort', 'Very old school'),
(2, 'Puma', 'Ben Talbort', 'Very old school'),
(2, 'Umbro', 'Ron Peters', 'All timer shoe'),
(3, 'Clarks', 'Pete Rumbous', 'Classy shoe'),
(3, 'Clarks', 'Pete Rumbous', 'Classy shoe'),
(3, 'Clarks', 'Pete Rumbous', 'Classy shoe'),
(3, 'Slazenger', 'Penelope White', 'Cheap shoe');

select * from product2;