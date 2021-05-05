devtools::install_github("BillPetti/baseballr")
library(baseballr)
library(BasketballAnalyzeR)
library(nbastatR)
library(tidyverse)
library(reactable)
library(ggthemes)

library(mmtable2)

data(package="BasketballAnalyzeR")

head(Obox)
head(Pbox)
tail(Obox)
tail(Pbox)


nbastatR::get_players_career_stats(players = "Patrick Ewing", 
                         modes = c("Totals"), assign_to_environment = TRUE,
                         add_mode_names = TRUE, return_message = TRUE)


get_players_career_stats(players = c("Joe Harris", "Myles Turner", "Spencer Dinwiddie"),
                         modes = c("Totals", "PerGame"))



cab1 <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/Cabrera, Miguel/Cabrera_Miguel2021.csv")

glimpse(cab1)
results <- unique(cab1$events)

events_set <- c("single", "double","home_run", "walk", "strikeout")

HWKs <- cab1 %>% 
  filter(events %in% events_set)

HWKs %>% 
  group_by(events) %>% 
  summarise(events, ï..pitch_type)

HWKs %>% 
  group_by(events) %>% 
  summarise(ï..pitch_type, events) %>% 
  table()


#######Trout
trout <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/Trout, Mike/Trout, Mike.csv")
glimpse(trout)

events_set <- c("single", "double","home_run", "walk", "strikeout")

trout_HWKs <- trout %>% 
  filter(events %in% events_set)

trout_HWKs %>% 
  group_by(events) %>% 
  summarise(events, pitch_name) %>% 
  table()

trout_table <- 
trout_HWKs %>% 
  group_by(events) %>% 
  summarise(events, pitch_name) %>% 
  table()

#mmtable(trout_table)


# 
# trout_table %>% 
#   mmtable2::mmtable(table_data = value) +
#   header_top(ï..pitch_type) +
#   header_left(events) +
#   header_top_left(var)  +
#   header_left_top(continent)  +
#   table_format(
#     locations = row_list,
#     style = style_list)
# 
# reactable(trout_table)

tt <- as.data.frame(trout_table)
levs <- c("single", "double", "home_run", "walk", "strikeout")
tt$events <- factor(tt$events, levels = levs)

trout_bars <- 
ggplot(tt, aes(pitch_name, Freq, fill = events)) +
  geom_bar(stat = "identity", position = "dodge", width = .85) +
  labs(title = "Mike Trout Hits, K's, & Walks", x = "Type of Pitch", y = "Total", fill = "Result") +
  scale_fill_brewer(palette = "Set1") +
  theme_fivethirtyeight() 

library(plotly)

ggplotly(trout_bars)


bogaerts <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/Bogaerts, Xander/Bogaerts, Xander.csv")

bogaerts_HWKs <- bogaerts %>% 
  filter(events %in% events_set)

bogaerts_table <- 
  bogaerts_HWKs%>% 
  group_by(events) %>% 
  summarise(events, pitch_name) %>% 
  table()

bogaerts_table <- as.data.frame(bogaerts_table)

bogaerts_table$events <- factor(bogaerts_table$events, levels = levs)

bogaerts_bars <- 
  ggplot(bogaerts_table, aes(pitch_name, Freq, fill = events)) +
  geom_bar(stat = "identity", position = "dodge", width = .85) +
  labs(title = "Xander Bogaerts Hits, K's, & Walks", x = "Type of Pitch", y = "Total", fill = "Result") +
  scale_fill_brewer(palette = "Set1") +
  theme_fivethirtyeight()
bogaerts_bars

ggplotly(bogaerts_bars)



library(fmsb)
library(ggradar)
library(baseballr)

#Import data
pitchers_april <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/STATCAST/MLB Top 5 Ks - 04.30.2021.csv")


summary(pitchers_april)
glimpse(pitchers_april)
pitchers_april$ï..pitch_type <- pitchers_april$pitch_type



#####BIEBER Set
bieber_k <- 
  filter(pitchers_april, player_name == "Bieber, Shane" & events == "strikeout")

bieber_k_table <- 
bieber_k %>% 
  group_by(pitch_name) %>% 
  summarize(events) %>% 
  table()

bieber_k_table <- as.data.frame(bieber_k_table)

bieber_k_chart <- 
  ggplot(bieber_k_table, aes(pitch_name, Freq, fill = pitch_name)) +
    geom_bar(stat = "identity", width = .75) +
    coord_polar() +
    theme(axis.ticks = element_blank(),
 #       axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
    geom_text(label = bieber_k_table$Freq, vjust = .25) +
    theme(legend.position = "none") +
    labs(title = "Shane Bieber K's by Pitch")
    
bieber_k_chart


#####BAUER Set
bauer_k <- 
  filter(MLB_pitchers_april, player_name == "Bauer, Trevor" & events == "strikeout")

bauer_k_table <- 
  bauer_k %>% 
  group_by(pitch_name) %>% 
  summarize(events) %>% 
  table()

bauer_k_table <- as.data.frame(bauer_k_table)

bauer_k_chart <- 
  ggplot(bauer_k_table, aes(pitch_name, Freq, fill = pitch_name)) +
  geom_bar(stat = "identity", width = .75) +
  coord_polar() +
  theme(axis.ticks = element_blank(),
        #       axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  geom_text(label = bauer_k_table$Freq, vjust = .25) +
  theme(legend.position = "none")+
  labs(title = "Trevor Bauer K's by Pitch")

bauer_k_chart










###ALL
MLB_k <- 
  filter(pitchers_april, events == "strikeout")


#Create Table
k_table <- 
  MLB_k %>% 
  group_by(player_name) %>% 
  summarize(pitch_name) %>% 
  table()

#Coerce to dataframe & trim 0 values
k_table <- as.data.frame(k_table) %>% 
  filter(Freq > 0)


k_charts <- 
  ggplot(k_table, aes(pitch_name, Freq, fill = pitch_name)) +
  geom_bar(stat = "identity", width = .75) +
  scale_fill_brewer(palette = "Set2") +
  coord_polar() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  geom_text(aes(pitch_name, Freq),label = k_table$Freq,
 #           position = position_dodge(width = 1),
            hjust = -.55, vjust = -.65, size = 3, color = "slategrey") +
  facet_wrap(k_table$player_name) +
  labs(fill = "Pitch Type", title = "Top Pitchers Ks by Pitch Type")
#+
#  theme(legend.position = "none")

k_charts

ggplotly(k_charts)


##########################################################################################################

MLB_pitchers_april <- read.csv("X:/OneDrive/02. Data/Baseball Data/Various/STATCAST/MLB_All_Pitchers_April2021.csv")

colnames(MLB_pitchers_april)[1] <- "pitch_type"



MLB_pitchers_set <- 
  MLB_pitchers_april %>% 
  filter(events == "strikeout") %>% 
  group_by(player_name) %>% 
  summarize(player_name, pitch_name)




top10list <- c("Bieber, Shane", "Cole, Gerrit", "deGrom, Jacob", "Glasnow, Tyler", "Bauer, Trevor",
               "Darvish, Yu", "Burnes, Corbin", "Peralta, Freddy", "Wheeler, Zack", "Musgrove, Joe")


Top_10 <- 
  MLB_pitchers_set %>% 
  filter(player_name %in% top10list) %>% 
  table()

Top_10 <- as.data.frame(Top_10)%>% 
  filter(Freq > 2)

Top_10$player_name <- factor(Top_10$player_name, levels = top10list)



top10_charts <- 
  ggplot(Top_10, aes(pitch_name, Freq, fill = pitch_name)) +
  geom_bar(stat = "identity", width = .65) +
  scale_fill_brewer(palette = "Dark2") +
  coord_polar() +
  theme_fivethirtyeight() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  geom_text(aes(pitch_name, Freq),label = Top_10$Freq, fontface=2,
            hjust = -.95, vjust = .5, size = 4, color = "black") +
  facet_wrap(~player_name, ncol = 5) +
  labs(fill = "Pitch Type", title = "Top Pitchers K's by Pitch \n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 11, color = "black", face = "bold.italic")) +
  theme(plot.background = element_rect(fill = "gray"))



top10_charts


####

top10_locs <- 
  MLB_pitchers_april %>% 
  filter(events == "strikeout", player_name %in% top10list) %>% 
    group_by(player_name) %>% 
    summarize(player_name, pitch_name, plate_x, plate_z)


top10_location_charts <- 
ggplot(top10_locs, aes(plate_x, plate_z, color = pitch_name)) +
  geom_point(size = 3, alpha = .4) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~player_name, ncol = 5) +
  theme_fivethirtyeight() +
  theme(plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
  xlim(c(-2.75, 2.75)) +
  ylim(c(-2, 5)) +
  geom_rect(xmin=-1.5, xmax=1.5, ymin=0, ymax=4, fill = NA, color = "black", linetype = "dotdash") +
  geom_hline(yintercept=2, linetype=1, color="gray", size=.5) +
  geom_vline(xintercept = 0, linetype = 1, color = "gray", size = .5) +
  labs(color = "Pitch Type", title = "Top Pitchers K's by Location \n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 11, color = "black", face = "bold.italic")) +
  theme(plot.background = element_rect(fill = "gray"))

top10_location_charts

