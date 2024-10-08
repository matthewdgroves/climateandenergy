# knitr::knit_hooks$set(inline = function(x) { knitr:::format_sci(x, 'md')})
# knitr::opts_chunk$set(echo = TRUE)


# library(pacman)
# p_load(zoo, xml2, tidyverse, stringr)
# source('scripts/utils.R', chdir = T)
# source('scripts/modtran.R', chdir = T)

library(tidyverse)

mlo_data = read_csv('data/mlo_data.csv', 
                    skip = 57,  # skip the first 57 rows
                    col_names = c('year', 'month', 'date.excel', 'date',
                                   'co2.raw', 'co2.raw.seas', 
                                   'co2.fit', 'co2.fit.seas',
                                   'co2.filled', 'co2.filled.seas'),   # give reasonable titles for each column
                     col_types = 'iiiddddddd', # the first three columns are integers
                                               # and the next 7 are real numbers
                     na = '-99.99' # interpret -99.99 as a missing value
                    )

mlo_simple = mlo_data %>% select(date, co2 = co2.filled) # keeping only the regular CO2 data with interpolated numbers for holes in the data


ggplot(mlo_simple, 
  aes(x = date, y = co2)) + # This line specifies the data to plot and the aesthetics that define which variables to use for the x and y axes
  geom_line() +   # This line says to plot lines between the points
  labs(x = "Year", y = "CO2 concentration (ppm)",
       title = "Measured CO2 from Mauna Loa Observatory") # This line gives the names of the axes
