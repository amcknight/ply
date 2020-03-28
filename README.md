# csvsql
A tool for running SQL queries on CSVs and other delimited row data.

## Roadmap
Subject to drastic changes
### v0.1
* A CLI that lets you input SQL queries
* SELECT clause should look up headers in the given CSV
* FROM clause takes a single CSV file and loads columns as Strings without nulls
* WHERE clause supports equality
### v0.2
* FROM can handle more than one file and WHERE clauses can check equality between them (join-like capabilities)
* Basic Bool, Int, Float, and String types can be pulled from CSVs
* Nulls are supported in the CSV files and in the WHERE query
* WHERE clause supports number inequalities
### Beyond
* CREATE, INSERT, and GROUP BY
* Console to allow query building auto-completion
* Start thinking about performance, maybe
* Streaming sources and sinks
* Flexible formats beyond CSV (JSONL, Common Log Format, etc)
* etc
