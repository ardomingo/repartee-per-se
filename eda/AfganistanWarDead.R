

library(tidyverse)
library(readxl)
library(rvest)
library(here)
library(lubridate)
library(ggthemes)

# page <- read_html("https://en.wikipedia.org/wiki/United_States_military_casualties_in_the_War_in_Afghanistan#Casualties_by_month_and_year")
# section <- page %>% html_elements()
# tbls <- html_nodes(page, "table")


afg <- read_xlsx("X:/OneDrive/02. Data/Military History/US Afghanistan War Deaths by Month.xlsx")
afg

afg_long <- pivot_longer(afg, cols = JAN:DEC, names_to = "Month", values_to = "Deaths")
afg_long


Date <- paste(afg_long$Year, afg_long$Month, rep("01", length(afg_long)), sep = "-")
Month<-as_date(Date)


afghan_war_table <- data.frame(Month, afg_long)
#colnames(afghan_war_table)[1] <- Date

colnames(afghan_war_table)[1] <- "Date"
colnames(afghan_war_table)[3] <- "Month"

afghan_war_table$Month <- factor(afghan_war_table$Month, levels = levels)
#fct_relevel(afghan_war_table$Month, levels = levels)

levels <- c("DEC", "NOV", "OCT", "SEP", "AUG", "JUL", "JUN", "MAY", "APR", "MAR", "FEB", "JAN")                                 
#levels <-  c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

afghan_vis <- ggplot(afghan_war_table,aes(x=Year,y=Month, fill=Deaths)) +
  geom_tile(color = "white", width=0.9, height=0.9) +
  geom_label(label = afghan_war_table$Deaths, color = "white") +
  theme_hc() +
  scale_fill_gradient(low="blue", high="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "US Deaths - Afghanistan War (2001 - 20??)", 
       subtitle = "Total US Fatalities by Month",
       caption = "Source: Wikipedia") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = "serif")) +
  theme(plot.subtitle = element_text(hjust = 0.5, family = "serif")) +
  theme(legend.title = element_text(vjust = 0.85, hjust = 0.5, family = "serif")) +
  theme(axis.title = element_text(hjust = 0.5, family = "serif")) +
  theme(axis.text = element_text(hjust = 0.5, family = "serif"))

afghan_vis




#CalendarHeat(afghan_war_table$Month, afghan_war_table$Deaths, 
#             ncolors = 25, color = "r2g", varname="US Afghanistan War Deaths")
