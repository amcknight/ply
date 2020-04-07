# csvsql
A tool for running SQL queries on CSVs

## Roadmap
Subject to drastic changes

Currently on v0.0.2
### v0.1
* A CLI that runs SQL using SELECT, FROM, and WHERE
* single table FROM
* WHERE clause supports equality
* Unit tests
### v0.2
* FROM can handle more than one file
* WHERE can check equality between tables (join-like capabilities)
* Basic Bool, Int, Decimal, and String types can be pulled from CSVs
* WHERE clause supports number inequalities
### Beyond
* CREATE, SELECT INTO, INSERT, JOIN, GROUP BY, ORDER BY, LIMIT
* Console to allow query building auto-completion
* Start thinking about performance, maybe
* Streaming sources and sinks
* Flexible formats beyond CSV (JSONL, Common Log Format, etc)
* Integration with NuShell
* etc
