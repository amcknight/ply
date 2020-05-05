# ply ![Stack Test CI](https://github.com/amcknight/ply/workflows/Stack%20Test%20CI/badge.svg)
A tool for running SQL queries on CSVs

[ChangeLog](ChangeLog.md) | [Roadmap](Roadmap.md)

## Installation
```shell script
git clone https://github.com/amcknight/ply.git
cd ply
stack build
stack install
```
I put `~/.local/bin` in my PATH to make it available in my shell after install.

## Usage
With a CSV that has headers 'name' and 'age' in "my/dir/" you could, for example, run:

`ply "SELECT name, "Mc" ++ name ++ "face" AS mcname FROM my/dir/people WHERE age < 30"`
