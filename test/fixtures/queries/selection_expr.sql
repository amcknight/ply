SELECT "Hello " ++ first_name AS hello_name, age + 5 * 2 as big_age
FROM test/fixtures/tables/people
WHERE last_name = "McKnight"
