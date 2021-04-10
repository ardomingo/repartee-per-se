library(tidyverse)

mlb_bat_040821 <- read.csv("X:/OneDrive/02. Data/Baseball Data/Rotowire Stats/Daily/mlb-player-stats-Batters-04.08.2021.csv")



#Import Brady Career Stats
tom_brady <- read.csv("X:/OneDrive/02. Data/NFL Data/TomBradyCareer.csv")
tom_brady <- tom_brady[1:20, c(1:6, 8, 10, 12, 14)]

tom_brady_to14 <- tom_brady %>% 
  filter(Year < 2015)

tom_brady_15on <- tom_brady %>% 
  filter(Year >= 2015)


#Set season periods
seasons1 <- 15
seasons2 <- 5

tom_brady_15on




#Get Totals for each era
brady_0014 <- 
  tom_brady_to14 %>% summarise_if(is.numeric, sum)

brady_1519 <- 
  tom_brady_15on %>% summarise_if(is.numeric, sum)

career_splits <- 
  rbind(brady_0014, brady_1519)

#rownames(career_splits) <- c("Early", "Late")
career_splits


career_splits_graph1 <- career_splits[ , c(1:4, 6, 8, 10, 12)]

cs1_long <- 
pivot_longer(
  career_splits_graph1,
  cols = everything(),
  names_to = "Stat")
  
cs2_long <- 
  pivot_longer(
    caree
    )



#Get Averages for each era
brady_0014_avg <- 
  tom_brady_to14 %>% summarise_if(is.numeric, mean)

brady_1519_avg <- 
  tom_brady_15on %>% summarise_if(is.numeric, mean)

career_splits_avg <- 
  rbind(brady_0014_avg, brady_1519_avg)

rownames(career_splits_avg) <- c("Early", "Late")
career_splits_avg

cs2_long2 <- 
  pivot_longer(
  career_splits_graph,
  cols = everything(),
  names_to = "Stat")

cs1_long





transpose(career_splits_avg)


library(data.table)

# get data
data("career_splits_avg")

# transpose
t_splits_avg <- transpose(career_splits_avg)

# get row and colnames in order
colnames(t_splits_avg) <- rownames(career_splits_avg)
rownames(t_splits_avg) <- colnames(career_splits_avg)

ggplot(t_splits_avg, aes())
