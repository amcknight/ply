SELECT first_name, last_name
FROM test/fixtures/tables/people
WHERE age < 15 * 2 AND last_name = "Mc" ++ "Knight"
