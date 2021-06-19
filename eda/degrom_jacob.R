

library(tidyverse)

degrom <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/STATCAST/deGrom, Jacob - 06.01.21.csv")

glimpse(degrom)
unique(degrom$events)


out_events <- c("strikeout", "field_out", "force_out", "grounded_into_double_play", "field_error")

degrom_outs <- degrom %>% 
  filter(events %in% out_events)

glimpse(degrom_outs)

degrom_summary <- 
degrom_outs %>% 
  group_by(events) %>% 
  summarise(n(), )


