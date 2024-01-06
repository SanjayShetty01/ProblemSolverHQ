SET @num = 0;
SELECT REPEAT('* ', @num := @num + 1)
FROM information_schema.tables
WHERE @num < 20 ;