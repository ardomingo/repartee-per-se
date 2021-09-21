

#Cabrera, Miguel - QUEST FOR 500 & 3000

library(tidyverse)
library(ggthemes)
library(plotly)

cabmig <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/STATCAST/Cabrera, Miguel - 09.19.2021.csv")
glimpse(cabmig)

events_list <- unique(cabmig$events)
hits_list <- c("single", "double", "home_run", "triple")



mc_hits <- filter(cabmig, events %in% hits_list)

#Hits by date
mc_hitdate <- mc_hits %>% group_by(game_date) %>% 
  summarise(hits = n())

#Calculate cumulative hits
mc_hitdate <- mutate(mc_hitdate, total_hits = cumsum(hits))

#Find # of season days
days <- as.numeric(as.Date("2021-10-03") - as.Date("2021-04-01"))

#Setup hits pace calculation slope
hits <- c(0, 134)
hits_per_day <- 134 / days
game_date <- seq(as.Date('2021-04-01'), as.Date('2021-10-03'), by = 'days')
game_date <- as.Date(game_date, format = "%y/%m/%d")
hits_req <- as.data.frame(cbind(as.Date(game_date, format = "%y/%m/%d"), hits_per_day))
hits_req <- mutate(hits_req, total_hits = floor(cumsum(hits_per_day)))


hits_pace <- 
ggplot(mc_hitdate, aes(game_date, total_hits)) +
  geom_line(color = "#FA4616", size = .75) +
  #geom_line(data=hits_req, aes(as.Date(game_date), total_hits)) +
  geom_smooth(data=hits_req, aes(as.Date(game_date), total_hits), color = "#0C2340", se = FALSE, size = .5, linetype = "dashed") +
  theme_hc() +
  labs(title = "Miguel Cabrera Hits Pace", subtitle = "Needs 134 Hits to Reach 3,000"  ,x = "Month", y = "Hits")


ggplotly(hits_pace) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0('Miguel Cabrera Hits Pace',
                                    '<br>',
                                    '<sup>',
                                    'Needs 134 Hits to Reach 3,000','</sup>')))

#Launch speed & angle
mc_hits_ls <- mc_hits %>% group_by(events) %>% 
  summarise(mean(launch_speed), mean(launch_angle))




#FUll count stats
full_count <- filter(cabmig, balls == 3 & strikes == 2 & events != "")
glimpse(full_count)

full_stats <- full_count %>% 
  group_by(events) %>% 
    summarise(Freq = n())
full_stats

prop.table(full_stats$Freq)


