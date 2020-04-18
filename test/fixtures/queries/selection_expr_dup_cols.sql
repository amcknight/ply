SELECT first_name ++ " B" ++ first_name ++ " " ++ last_name aS long_name, age * age * 2 AS overbig_age
FROM test/fixtures/tables/people
WHERE last_name = "McKnight"
