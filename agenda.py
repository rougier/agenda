# Console agenda - copyright (c) 2021 Nicolas P. Rougier
# Released under the GNU General Public Licence version 3

import re
import calendar
import datetime
import orgparse
    
class Event:
    """ Representation of an agenda event as (start, end, description) """
    
    def __init__(self, description, start, end=None, special=False):
        """ Create a new event. """
        
        self.description = description
        self.special = special

        # Start date and time
        if isinstance(start, datetime.datetime):
            self.start_date = start.date()
            self.start_time = start.time()
        else:
            self.start_date = start
            self.start_time = None

        # End date and time
        if isinstance(end, datetime.datetime):
            self.end_date = end.date()
            self.end_time = end.time()
        elif isinstance(end, datetime.date):
            self.end_date = end
            self.end_time = None
        else:
            self.end_date = None
            self.end_time = None


    def __eq__(self, other):
        return self.start_date == other.start_date and self.start_time == other.start_time

    def __ne__(self, other):
        return not (self == other)

    def __lt__(self, other):

        if self.start_date != other.start_date:
            return self.start_date < other.start_date
        elif self.start_time and other.start_time:
            return self.start_time < other.start_time
        elif self.start_time:
            return False
        elif other.start_time:
            return True
        return True

    
    def info(self, style, details=False, prefix=""):
        """ Return a descriptive string of the event """

        s = style.none
        if self.start_date < datetime.date.today():
            s += style.event_past + prefix
        elif self.start_date > datetime.date.today():
            s += style.event_future + prefix
        elif self.start_date == datetime.date.today():
            s += style.event_today + prefix
        if self.special:
            s += style.event_special + "!! "

            
        # Same day event
        if self.start_date == self.end_date or self.end_date is None:
            if not details:
                if self.start_date == datetime.date.today():
                    s += style.event_today
                    s += "Today at {0:%H:%M} ".format(self.start_time)
                    s += style.none
                else:
                    # s += "{0:%d %b %Y} ".format(self.start_date)
                    s += "{0:%d %b %Y} ".format(self.start_date)
                    
                # if self.start_time:
                #     s += "({0:%H:%M})".format(self.start_time)
                # else:
                #     s += "......."
            else:
                if self.start_time is not None:
                    s += "{0:%H:%M} ".format(self.start_time)
                    if self.end_time is not None:
                        s += "- {0:%H:%M} ".format(self.end_time)
                    else:
                        s += "....... "
                #else:
                #    s += "............."

        # Event spanning several days
        else:
            s += "{0:%d/%m}".format(self.start_date)
            s += " - {0:%d/%m}".format(self.end_date)

        s += self.description + style.none
        return s


# ----------------------------------------------------------------------
class Agenda:
    """ Agenda class """

    def __init__(self, filenames, holidays, style):
        """ Build a new agenda from given filenames, style and holidays"""

        self.origin = 1,1
        self.year = datetime.date.today().year
        self.style = style
        self.holidays = holidays
        self.filenames = filenames
        self.terminal = None
        self.populate()
        

    def is_weekend(self, year, month, day):
        """ Check if date is Saturday or Sunday """
        
        return datetime.date(year, month, day).weekday() in [5,6]

    def is_today(self, year, month, day):
        """ Check if date is today """
        
        return datetime.date(year, month, day) == datetime.date.today()

    def is_vacant(self, year, month, day):
        """ Check if day is vacant """
        
        return datetime.date(year, month, day) in self.holidays


    def get_day(self, mouse):
        """ Find the day and month under mouse. """

        year = self.year
        month_cols = 7*3
        month_rows = 8
        months_per_row = 4

        x, y = mouse[0]-self.origin[0], mouse[1]-self.origin[1]
        month = 1 + ( x // (month_cols+1)) + months_per_row * (y // (month_rows+1))
        if month < 1 or month > 12:
            return None, None, None

        mx = (month_cols+1)*((month-1) % months_per_row)
        x -= mx
    
        my = (month_rows+1)*((month-1) // months_per_row)
        y -= my

        # x can be greater than month_cols because of separation between months
        if x >= month_cols:
            return None, None, None
    
        first = calendar.weekday(year, month, 1)
        last  = calendar.monthrange(year, month)[1]
        day = (y-2)*7 + x // 3 - first + 1
        if day < 1 or day > last:
            return None, None, None
        else:
            x = self.origin[0] + mx + 3*(x//3)
            y = self.origin[1] + my + y
            return month, day, (x,y)


    def populate(self):
        """ Populate the agenda. """
        
        self.events = {}
        for filename in self.filenames:
            root = orgparse.load(filename)
            for node in root.env.nodes[1:]:
                heading = node.heading
                heading = re.sub("\[.*\]|\<.*\>|NEXT|TODO", "", heading)
                heading = heading.strip()
                
                if node.deadline:
                    d = node.deadline.start
                    key = d.year, d.month, d.day
                    day_events = self.events.get(key, [])
                    day_events.append( Event(heading, node.deadline.start, special=True) )
                    self.events[key] = day_events
        
                for date in node.get_timestamps(active=True, point=True, range=True):
                    d = date.start
                    key = d.year, d.month, d.day
                    day_events = self.events.get(key, [])
                    day_events.append(Event(heading, date.start, date.end))
                    self.events[key] = day_events

        # Sort events inside a day
        for date in self.events.keys():
            self.events[date].sort()

        
    def format_day(self, year, month, day):
        """ Format a day string (3 characters) """
        
        key = year, month, day
        events = self.events.get(key, [])
        busy = len(events)
        style = self.style
            
        s  = style.none
        if self.is_today(year, month, day):
            s += style.today
        elif busy > 0:
            s += style.levels[min(busy-1,9)]
        elif self.is_weekend(year, month, day):
            s += style.weekend
        elif self.is_vacant(year, month, day):
            s += style.vacant
        else:
            s += style.default

        key = year, month, day
        events = self.events.get(key, [])
        busy = len(events)
        special = sum(1 for event in events if event.special)
            
        s += "%2d" % day
        if special > 0:
            s += style.special
            markers = "⠁⠃⠇"
            s += markers[min(special,len(markers))-1]
        else:
            s += " "
            
        return s + style.none

    def format_month(self, year, month):
        """ Format a month string (21x8 characters) """
        
        day_names   = [day[:2] for day in list(calendar.day_name)]
        month_name = list(calendar.month_name)[month]
        month_cols = 7*3
        month_rows = 8
        style = self.style
        
        lines = [""]*month_rows
    
        # Month name
        lines[0] = style.none + style.month + month_name.center(month_cols-1) + style.none + " "
    
        # Day names
        lines[1]  = style.weekday_name + " ".join(day_names[:5]) + " " + style.none
        lines[1] += style.weekend_name + " ".join(day_names[5:]) + style.none
        
        # Week days
        first = calendar.weekday(year, month, 1)
        last  = calendar.monthrange(year, month)[1]
        for row in range(month_rows-2):
            lines[2+row] = style.none
            for column in range(7):
                day = row*7 + column - first + 1
                if day < 1 or day > last:
                    lines[2+row] += "   "
                else:
                    lines[2+row] += self.format_day(year, month, day)
        return lines


    
    def display_calendar(self, year=None):
        """ """
    
        x0, y0 = self.origin
        month_cols = 7*3
        month_rows = 8
        months_per_row = 4
        year = year or self.year

        month = 1
        for row in range(12//months_per_row):
            for col in range(months_per_row):
                x, y  = x0 + col*(month_cols+1) , y0 + row*(month_rows+1)
                s = self.format_month(year, month)
                for i in range(8):
                    self.terminal.write(s[i], (x, y+i))
                month += 1

        self.terminal.flush()


    def display_events(self, start=None):

        days = 1
        month_cols = 7*3
        month_rows = 8
        months_per_row = 4
        style = self.style
        
        if start is None:
            start = datetime.date.today()
            start = start - datetime.timedelta(days=start.weekday())
            days = 7
            
        x = self.origin[0]
        y = self.origin[1] + (12//months_per_row) * (month_rows+1)

        self.terminal.clear((x,y))
        
        if days == 7:
            s = "{0:%A %d %B  %Y} - {1:%A %d %B  %Y}".format(start,
                                                             start + datetime.timedelta(days=days-1))
            self.terminal.write(style.event_header + s + style.none, (x,y))
            y += 2
        else:
            s = style.event_header + "{0:%A %d %B %Y}".format(start)
            if self.is_vacant(start.year, start.month, start.day):
                s += style.vacant + " ({0})".format(self.holidays[start])
                
            self.terminal.write(s + style.none, (x,y))
            y += 2

        if days == 1:
            key = start.year, start.month, start.day
            for event in self.events.get(key,[]):
                self.terminal.write(event.info(style, details=True), (x,y))
                y += 1
        else:
            for date in (start + datetime.timedelta(days=n) for n in range(days)):
                key = date.year, date.month, date.day
                for i, event in enumerate(self.events.get(key,[])):
                    if i == 0:
                        prefix = "{0:12s} : ".format("{0:%a. %d %b.}".format(date))
                    else:
                        prefix = " "*15
                    self.terminal.write(event.info(style, details=True, prefix=prefix), (x,y))
                    y += 1
                y += 1
                
        self.terminal.flush()
