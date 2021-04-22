library(tidyverse)
library(waffle)


mdd <- read.csv("X:/OneDrive/02. Data/Environmental Data/Mammal Diversity Database/MDD/MDD/MDD_v1.4_6533species.csv")


piv <- 
mdd %>% 
  group_by(order) %>% 
  summarize(n()) 


colnames(piv) <- c("Order", "Number of Species")

piv <- 
piv %>% 
  arrange(desc(`Number of Species`))
tail(piv)


waff_pal <- colorRampPalette(c("blue", "red"))

waffle(
    piv, rows = 20, size = .25,
    colors = waff_pal(28))

waff_pal(3)
