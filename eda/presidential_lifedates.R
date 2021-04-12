#HOW MANY US PRESIDENTS HAVE BEEN ALIVE AT ANY ONE TIME?


library(tidyverse)
library(rvest)
library(timevis)


#Read html table
html_dl <- read_html("https://www.presidentsusa.net/birth.html")

#Pull table data (use Google Chrome & SelectorGadget to find element)
bd_dates <- html_dl %>% 
              html_element(".callout") %>% 
              html_table()

