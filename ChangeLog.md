# Changelog for csvsql

## v0.0.8
* Added Bool support in input tables
* Added headers in output tables
* Factoring and code cleanup in prep of v0.1

## v0.0.7
* SELECT Expressions
* SELECT AS
* SELECT Expression type checking
* SELECT * support

### v0.0.6
* Type Checking of WHERE clause

### v0.0.5
* Converted Unit testing over to golden tests
* Moved Main code into CsvSql

### v0.0.4
* Created an Expression AST with parse and eval methods
* WHERE Clause works (though fails silently and isn't type-safe)

### v0.0.3
* Switched out query parsing to use megaparsec
* SELECT statements parse with commas

### v0.0.2

* Unit tests exist
* A limited form of SELECT ... FROM queries work
  * The output column order is indeterminate
  * When selecting the same column twice, it only displays once
  * Must include an empty WHERE at the end of the query
  * Query considers commas to be part of the name
  * Separate column names by spaces
  * Only a single FROM table can be listed
  * Ints and String can be pulled from tables
  * But rows are not guaranteed to have the same type for elements in the same column
* Checks only one specific directory for files
* Values can't be escaped or quoted in any way to add spaces to values
* Table Loader is relatively untested and brittle

### v0.0.1

* Initial commits
* Doesn't work at all