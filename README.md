# csvsql ![Stack Test CI](https://github.com/amcknight/csvsql/workflows/Stack%20Test%20CI/badge.svg)
A tool for running SQL queries on CSVs

## Roadmap
Subject to drastic changes
### v0.1
* A CLI that runs SQL using SELECT, FROM, and WHERE
* single table FROM
* Basic Bool, Int, and String support in queries and CSVs
* Unit tests
### v0.2
* FROM can handle more than one file
* WHERE can check equality between tables (join-like capabilities)
* Add support for Decimals
* WHERE clause supports number inequalities
### Beyond
* CREATE, SELECT INTO, INSERT, JOIN, GROUP BY, ORDER BY, LIMIT
* Console to allow query building auto-completion
* Start thinking about performance, maybe
* Streaming sources and sinks
* Flexible formats beyond CSV (JSONL, Common Log Format, etc)
* Integration with NuShell
* etc
