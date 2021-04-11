library(tidyverse)
library(DT)




#Get yearly team passing stats
teampass_2015 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2015.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2015$Season <- 2015


teampass_2016 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2016.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2016$Season <- 2016


teampass_2017 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2017.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2017$Season <- 2017


teampass_2018 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2018.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2018$Season <- 2018


teampass_2019 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2019.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2019$Season <- 2019

teampass_2020 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataTeamPassing2020.csv") %>% 
  summarize_if(is.numeric, mean)
teampass_2020$Season <- 2020



#Combine 15-19 into table
league_avgs <- rbind(teampass_2015, teampass_2016, teampass_2017, teampass_2018, teampass_2019)



#Prep df's to match to bind

league1 <- league_avgs[ , c(1:2, 5:7)] %>% 
    rename(Yds = Pass.Yds,
           Int = INT)

league1$Years <- c(2015:2019)
league1$Party <- "League Avg."


brady_early <- brady_0014_avg[ , 3:7]
brady_early$Years <- "2000-2014"
brady_early$Party <- "BRADY Avg."

brady_late <- brady_1519_avg[ , 3:7]
brady_late$Years <- "2015-2019"
brady_late$Party <- "BRADY Avg."



#Create table with {DT} pkg
avgs_table_set <- rbind(league1, brady_early, brady_late)

datatable(avgs_table_set,           
      rownames = FALSE,
      caption = htmltools::tags$caption(style = 
            'caption-side: top; text-align: center; color:black; font-size:200% ;','Brady vs. The League'),
      options = list(
            order = list(list(2, 'dsc')),
            columnDefs = list(list(className = 'dt-center', targets = 0:6)))) %>% 
            formatRound(columns=c(1:5), digits=2)


#Get Player data and categorize by year
passing_2015 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2015.csv")
passing_2015$Year <- 2015

passing_2016 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2016.csv")
passing_2016$Year <- 2016

passing_2017 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2017.csv")
passing_2017$Year <- 2017

passing_2018 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2018.csv")
passing_2018$Year <- 2018

passing_2019 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2019.csv")
passing_2019$Year <- 2019

passing_2020 <- read.csv("X:/OneDrive/02. Data/NFL Data/NFl DataPassing2020.csv")
passing_2020$Year <- 2020

passing_combined_15to19 <- rbind(passing_2015, passing_2016, passing_2017, passing_2018, passing_2019)


passing_combined_15to19 <- passing_combined_15to19[ , c(1, 4:5, 7, 9, 11, 17)]

brady_late_insert <- brady_late[, 1:6] %>% 
  rename(Year = Years)
brady_late_insert$Player <- "T. BRADY"


brady2020 <- filter(passing_2020, grepl("Brady", Player, fixed = TRUE))
brady2020 <- brady2020[ , c(1, 4:5, 7, 9, 11, 17)]




#Add Brady 2020 stats for emphasis
percentiles <- rbind(passing_combined_15to19, brady_late_insert, brady2020)





# brady2020 <- 
# brady2020 %>%  mutate(AttPCT = ntile(Att, 100),
#                     CmpPCT = ntile(Cmp, 100),
#                     YdsPCT = ntile(Yds, 100),
#                     TDPCT = ntile(TD, 100),
#                     IntPCT = 100-ntile(Int, 100),
#                     Cmpl = Cmp / Att,
#                     CmplPCT = ntile(Cmpl, 100))
# 
# #Reorder
# brady2020 <- brady2020[, c(1, 7, 2:3, 13, 4:6, 8:9, 14, 10:12)]



#tab_table <- rbind(passing_combined_15to19, brady_late_insert, brady2020)
#tab_table$Player <- str_trim(tab_table$Player, "both")

# 
# datatable(tab_table,           
#           rownames = FALSE,
#           caption = htmltools::tags$caption(style = 
#                                               'caption-side: top; text-align: center; color:black; font-size:200% ;','Brady vs. The Players'),
#           options = list(
#             order = list(list(2, 'dsc')),
#             columnDefs = list(list(className = 'dt-center', targets = 1:6)))) %>% 
#   formatRound(columns=c(1:5), digits=2)


percentiles <- 
percentiles %>% 
  mutate(AttPCT = ntile(Att, 100),
         CmpPCT = ntile(Cmp, 100),
         YdsPCT = ntile(Yds, 100),
         TDPCT = ntile(TD, 100),
         IntPCT = 100-ntile(Int, 100),
         Cmpl = Cmp / Att,
         CmplPCT = ntile(Cmpl, 100)
         )

#Reorder
percentiles <- percentiles[, c(1, 7, 2:3, 13, 4:6, 8:9, 14, 10:12)]




#Build Table  
datatable(percentiles,
      rownames = FALSE,
      caption = htmltools::tags$caption(style = 
                    'caption-side: top; text-align: center; color:black; font-size:200% ;','Brady vs. The Players'),
      options = list(pageLength = 15,
      order = list(list(2, 'dsc')),
      columnDefs = list(list(className = 'dt-center', targets = 2:6)))) %>% 
  formatStyle(
      'Player',
      target = 'row',
      backgroundColor = styleEqual("T. BRADY", 'aqua')) %>% 
  formatStyle(
      'Year',
      target = 'row',
      backgroundColor = styleEqual("2020", 'skyblue')) %>% 
  formatPercentage(columns = 5)
