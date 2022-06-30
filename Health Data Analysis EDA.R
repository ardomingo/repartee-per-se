

library(tidyverse)
library(lubridate)

ad_stats <- read.csv(file = "X:/OneDrive/02. Data/Health Data/AD Scale Stats/Dres333 - Export Data 4-9-2022 _ 6-4-2022.csv")



ad_stats$Weight <- gsub("lb",'',ad_stats$Weight) %>% as.double()
ad_stats$Body.Fat <- gsub("%",'',ad_stats$Body.Fat) %>% as.double()
ad_stats$Fat.Free.Body.Weight <- gsub("lb",'',ad_stats$Fat.Free.Body.Weight) %>% as.double()
ad_stats$Subcutaneous.Fat <- gsub("%",'',ad_stats$Subcutaneous.Fat) %>% as.double()
ad_stats$Body.Water <- gsub("%",'',ad_stats$Body.Water) %>% as.double()
ad_stats$Skeletal.Muscles <- gsub("%",'',ad_stats$Skeletal.Muscles) %>% as.double()
ad_stats$Muscle.Mass <- gsub("lb",'',ad_stats$Muscle.Mass) %>% as.double()
ad_stats$Bone.Mass <- gsub("lb",'',ad_stats$Bone.Mass) %>% as.double()
ad_stats$Protein <- gsub("%",'',ad_stats$Protein) %>% as.double()
ad_stats$BMR <- gsub("kcal",'',ad_stats$BMR) %>% as.integer()
ad_stats$Time <- substr(ad_stats$Time, 1, nchar(ad_stats$Time)-3)
ad_stats$Time <- gsub(",", "", ad_stats$Time)
ad_stats$Time <- mdy_hm(ad_stats$Time, format = "%m%d%Y", truncated = 1) %>% as_date()
ad_stats$Time <- mdy_hm(ad_stats$Time)
ad_stats$DOW <- weekdays(ad_stats$Time)

glimpse(ad_stats)

summary(ad_stats)

day_order = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

AD_WT_Table <- 
ad_stats %>% 
  group_by(DOW) %>% 
  summarise(Avg_WT = mean(Weight), 
            Avg_BMI = mean(BMI),
            Avg_BMR = mean(BMR),
            Avg_Body.Water = mean(Body.Water),
            Avg_Body.Fat = mean(Body.Fat),
            Avg_Subcutaneous.Fat = mean(Subcutaneous.Fat),
            Avg_Visceral.Fat = mean(Visceral.Fat),
            Avg_Protein = mean(Protein),
            Avg_Muscle.Mass = mean(Muscle.Mass))

arrange(AD_WT_Table, factor(DOW, levels = day_order))
