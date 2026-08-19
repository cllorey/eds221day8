library(tidyverse)


# Creating dates & times -------------------------------------------------

# The order of the letters in the function name describe the order of the components in the date string

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

# type and class of a date
my_date <- dmy("31-Jan-2017")
typeof(my_date)
class(my_date)
unclass(dmy("31-Jan-1900"))

# Under the hood, *dates* are doubles. Days since 1970-01-01.

# Datetimes --------------------------------------------------------------

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

# Lubridate functions will do their best to infer the date format for you
ymd_hms("2017_1_31 20^11^59")
ymd_hms("2017_Janu_31 20^11^59") # Janu breaks this

# Datetimes under the hood are doubles, the number of *seconds* since 1/1/1970
my_datetime <- mdy_hm("01/31/2017 08:01")
my_datetime

class(my_datetime)
unclass(my_datetime)

# Because dates and times are just numbers, it's easy to add to them
my_datetime
my_datetime + 1
my_datetime + 60 * 60 * 24

# This can get funky, so instead use e.g., days()
my_datetime + days(3)
my_datetime + weeks(3)

# Getting components -----------------------------------------------------
# Functions like year(), month(), hour(), minute() all extract components
year(my_datetime)
month(my_datetime)

# Setting label = TRUE gives us a factor with the levels in the appropriate order
month(my_datetime, label = TRUE)

wday(my_datetime)
wday(my_datetime, label = TRUE)


# Datetimes in dataframes ------------------------------------------------

library(nycflights13)

glimpse(flights)

flights |>
  mutate(
    # make_datetime() assembles dates and times from components
    departure = make_datetime(year, month, day, hour, minute),
    # extract the day-of-week component from the departure
    dep_wday = wday(departure, label = TRUE)
  ) |>
  select(departure, dep_wday)
