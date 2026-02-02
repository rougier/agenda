# Plain Text Agenda Format

The plain text agenda format is directly & heavily inspired by the
calendar.txt idea by [Tero
Karvinen](https://terokarvinen.com/2021/calendar-txt/). The major
difference is that the proposed format sticks to one event per line,
but the same spirit remains with the following principles:

- Human-readable and editable: openable in any text editor
- Portable and scriptable: works with Unix tools, shell scripts, or Emacs
- Minimalist and structured: each line represents a single event
- Deterministic identifiers: every timed event has a unique ID
- Safe to modify: tags, actions, or states can be updated without breaking parsing

# Syntax

## Entry

An entry has a mandatory format with several fields but only the first
is mandatory:

`YYYY-MM-DD [wNN] [Day] [HH:MM-HH:MM] Summary [@ACTIONS] [+TAGS] [REFERENCE]`

with

Field         | Mandatory | Description
--------------|-----------|------------
`YYYY-MM-DD`  | Yes       | Date of the event
`wNN`         | No        | ISO week number
`Weekday`     | No        | Short weekday (Mon, Tue, …)
`Time`        | No        | Start-End (HH:MM-HH:MM), start only, or no time
`Title`       | No        | Text describing the event
`ACTION`      | No        | Action(s) associated with the event
`TAG`         | No        | Tag(s) for classification
`REFERENCE`   | No        | Reference UID

## Metadata

Any event can be associated to metadata through the reference UID:

`[REFERENCE:KEY]: Content`

with 

Field         | Mandatory | Description
--------------|-----------|------------
`REFERENCE`   | Yes       | Reference UID
`KEY`         | Yes       | Name of the field
`Content`     | Yes       | Value of the field


## CLI Integration

Since the format is strictly line-oriented, you can query your agenda from any shell.
Here are a few command-line examples showing how to filter an agenda based on time

When          | Command
--------------|--------------------------------------------------------------------
Today         | ``` grep `date +"%Y-%m-%d"` agenda.txt ```
Tomorrow      | ``` grep `date -v+1d +"%Y-%m-%d"` agenda.txt ```
Current week  | ``` grep `date +"^%Y"` agenda.txt \| grep `date +"w%V"` ```
Next week     | ``` grep `date -v+1w +"^%Y"` agenda.txt \| grep -v+1w `date +"w%V" ```
Current month | ``` grep `date +"^%Y-%m"` agenda.txt ```
Next month    | ``` grep `date -v+1m +"^%Y-%m"` agenda.txt ```
Future        | ``` awk -v today=$(date +"%Y-%m%d") '$1 > today' agenda.txt ```

**Note**: The -v flag for date is used in BSD/macOS. For GNU/Linux, use the --date="next day" syntax.

There is also a more elaborate example [agenda.sh](./agenda.sh) that display the agenda for the current week.


## Emacs

The emacs package provides two modes:
- A minor edit mode to manipulate the txt file
- A major view mode to display the agenda for a day, a week, a month, 3 month or the whole year.
