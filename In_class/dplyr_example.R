library(dplyr)

library(nycflights13)
head(flights)
summary(flights)


filter(flights, month == 10, day == 4, carrier == 'AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA'))
#instead of using the dplyr, we can use the [] notation, it is long and messy :()
head(flights[flights$month == 10 & flights$day == 4 & flights$carrier == 'AA', ])

slice(flights, 2:15) # select first 15 rows

# arrange() in dplyr
#arrange() function works similar to filter() function except that instead of filtering or selecting rows
arrange(flights, year, month, day, arr_time)
head(arrange(flights,year,month,day,arr_time))
# if I want to use the descending time instead of accending time,
head(arrange(flights, year, month, day, desc(arr_time)))

#select() in dplyr
select(flights, carrier)
head(select(flights, carrier))
# we can add additional columns easily

head(select(flights, carrier, arr_tiem))
head(select(flights, carrier, arr_time, day))


#distinct()
distinct(select(flights, carrier))

# mutate() in dplyr

head(mutate(flights, MyNewColumn = arr_delay - dep_delay))

# or
head(transmute(flights, MyNewColumn = arr_delay - dep_delay))

#summarise() in dplyr
summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) # average airtime
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE)) # Total flight Time

#sample_n() in dplyr

sample_n(flights, 15) # random 15 rows.
sample_n(flights, 71) # random 71 rows.

#sample_frac() in dplyr
sample_frac(flights, 0.1) # sample witha 10% of rows from the total number of rows

# Pipe operator
df_mtcars <- mtcars
head(df_mtcars)

filter(df_mtcars, mpg >20)

sample_n(filter(df_mtcars, mpg > 20), 10)
# now arrange
arrange(sample_n(filter(df_mtcars, mpg >20), 10), desc(mpg))
# assign this result to a value
results_mpg <- arrange(sample_n(filter(df_mtcars, mpg > 20), 10), desc(mpg))


# Instead use the pipe operator, it's apart of the dplyr library

results <- df_mtcars %>% filter(mpg >20) %>% sample_n(size = 5) %>% arrange(desc(mpg))
