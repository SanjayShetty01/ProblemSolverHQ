SET @num = 21;
SELECT REPEAT('* ', @num := @num - 1)
FROM information_schema.tables
WHERE @num > 0;