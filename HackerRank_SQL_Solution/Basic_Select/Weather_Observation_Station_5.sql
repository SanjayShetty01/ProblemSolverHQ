SELECT CITY, LENGTH(CITY)
FROM station
WHERE CITY = (SELECT CITY
              FROM station
              ORDER BY LENGTH(CITY), CITY
              LIMIT 1)
   OR CITY = (SELECT CITY
              FROM station
              ORDER BY LENGTH(CITY) DESC, CITY
              LIMIT 1);