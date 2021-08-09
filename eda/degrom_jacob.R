

library(tidyverse)

degrom <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/STATCAST/deGrom, Jacob - 06.01.21.csv")

colnames(degrom)[1] <- "pitch_type"

glimpse(degrom)
unique(degrom$events)


out_events <- c("strikeout", "field_out", "force_out", "grounded_into_double_play", "field_error")
hit_events <- c("single", "home_run", "double", "triple", "walk")

degrom_outs <- degrom %>% 
  filter(events %in% out_events)

degrom_hits <- degrom %>% 
  filter(events %in% hit_events)

degrom_pitches <- degrom %>% 
  group_by(pitch_type) %>% 
  summarise(n(), mean(release_speed))


glimpse(degrom_outs)

degrom_summary_outs <- 
degrom_outs %>% 
  group_by(events) %>% 
  summarise(n())

degrom_summary_hits <- 
  degrom_hits %>% 
  group_by(events, pitch_type) %>% 
  summarise(n())


degrom_hits %>% 
  group_by(pitch_type, events) %>% 
  summarise(n())



degrom_Ks <- degrom %>% 
  filter(events == "strikeout") 

degrom_Ks <- 
degrom_Ks %>% 
  group_by(pitch_type) %>% 
  summarise(Ks = n())



#Set up location charts
suppressMessages(top10_locs <- 
                   MLB_pitchers_april %>% 
                   filter(events == "strikeout", player_name %in% top10list) %>% 
                   group_by(player_name) %>% 
                   summarize(player_name, pitch_name, plate_x, plate_z))


ggplot(degrom_Ks, aes(pitch_type, Ks)) +
  geom_bar(stat = "identity")
