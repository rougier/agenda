
### Interactive agenda in the console

This Python script reads an [org agenda](https://orgmode.org/) file
(i.e. a regular org file with some active dates) and displays an
interactive and colored year calendar with detailed information for
each day when the mouse hovers a specific date. Else, it shows event
for a the current week.

### Usage

`./agenda agenda.org --holidays France`

### Dependencies

```
pip install orgparse  # See https://github.com/karlicoss/orgparse
pip install holidays   # See https://github.com/dr-prodigy/python-holidays
```

### Example output

![](agenda.png)
