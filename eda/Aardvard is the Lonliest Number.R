library(tidyverse)
library(waffle)
library(plotly)
library(RColorBrewer)
library(wesanderson)
library(hrbrthemes)
library(formattable)


mdd <- read.csv("X:/OneDrive/02. Data/Environmental Data/Mammal Diversity Database/MDD/MDD/MDD_v1.4_6533species.csv")


pivot_table_set <- 
mdd %>% 
  group_by(order) %>%
  summarize(n())


colnames(pivot_table_set) <- c("Order", "Number of Species")

pivot_table_set <- 
  pivot_table_set %>% 
    arrange(desc(`Number of Species`)) %>% 
    mutate(Percent = `Number of Species`/sum(`Number of Species`))

order_table <- 
  formattable(pivot_table_set,
            align = c("c", rep("r", NCOL(pivot_table_set) -1)),
            list(
              `Number of Species` = color_bar("lightblue"),
              Percent = percent,
              Percent = color_tile("transparent", "skyblue")
            ))


#########################

#Set up color palette for 27 most distinctive colors
n <- 27
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))


#Set main waffle dataframe
mdd %>%
  count(order) %>%
  ggplot(aes(fill = order, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() -> waf

#Set rodent df
rod_df <- 
  mdd %>% 
  count(order) %>% 
    filter(order == "RODENTIA")

rod_hlt <- c(rep(alpha("white", .25), 23), "black", rep(alpha("white", .25), 3))

waf +
  geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
  scale_fill_manual(values = col_vector, length(order)) +
  theme(legend.title = element_blank()) #+
  # scale_colour_manual(
  #   values = rod_hlt1
  # )


########################################################################

#Load Data
mdd <- read.csv("X:/OneDrive/02. Data/Environmental Data/Mammal Diversity Database/MDD/MDD/MDD_v1.4_6533species.csv")

#Set main waffle dataframe
mdd %>%
  count(order) %>%
  ggplot(aes(fill = order, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() -> waf


colorset_all <- 
  c("#7FC97F", "#BEAED4", "#FDC086", "#66FF33", "#BF5B17",
    "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#E6AB02", "#A6761D", "#A6CEE3", "#1F78B4",
    "#B3E2CD", "#FDDAEC", "#E41A1C", "#80B1D3", "#B3DE69",
    "#F781BF", "#1A1AFF", "#FB8072", "#A65628", "#CCEBC5",
    "#999999", "#F0027F")

mammalia_waffle <- 
  waf +
    geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
    scale_fill_manual(values = colorset_all, length(order)) +
    theme(legend.title = element_blank())
mammalia_waffle



#RODENTIA
colorset_rodentia <- 
  c(alpha("#7FC97F", .25), alpha("#BEAED4", .25), alpha("#FDC086", .25), alpha("#66FF33", .25), alpha("#BF5B17", .25), 
    alpha("#666666", .25), alpha("#1B9E77", .25), alpha("#D95F02", .25), alpha("#7570B3", .25), alpha("#E7298A", .25),
    alpha("#66A61E", .25), alpha("#E6AB02", .25), alpha("#A6761D", .25), alpha("#A6CEE3", .25), alpha("#1F78B4", .25),
    alpha("#B3E2CD", .25), alpha("#FDDAEC", .25), alpha("#E41A1C", .25), alpha("#80B1D3", .25), alpha("#B3DE69", .25),
    alpha("#F781BF", .25), alpha("#1A1AFF", .25), alpha("#FB8072", .25), "#A65628",  alpha("#CCEBC5", .25),
    alpha("#999999", .25), alpha("#F0027F", .25))


rodentia_waffle <- 
  waf +
  geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
  scale_fill_manual(values = colorset_rodentia, length(order)) +
  theme(legend.title = element_blank())
rodentia_waffle



#CHIROPTERA
colorset_chiroptera <- 
  c(alpha("#7FC97F", .25), alpha("#BEAED4", .25), alpha("#FDC086", .25), "#66FF33", alpha("#BF5B17", .25), 
    alpha("#666666", .25), alpha("#1B9E77", .25), alpha("#D95F02", .25), alpha("#7570B3", .25), alpha("#E7298A", .25),
    alpha("#66A61E", .25), alpha("#E6AB02", .25), alpha("#A6761D", .25), alpha("#A6CEE3", .25), alpha("#1F78B4", .25),
    alpha("#B3E2CD", .25), alpha("#FDDAEC", .25), alpha("#E41A1C", .25), alpha("#80B1D3", .25), alpha("#B3DE69", .25),
    alpha("#F781BF", .25), alpha("#1A1AFF", .25), alpha("#FB8072", .25), alpha("#A65628", .25),  alpha("#CCEBC5", .25),
    alpha("#999999", .25), alpha("#F0027F", .25))


chiroptera_waffle <- 
  waf +
  geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
  scale_fill_manual(values = colorset_chiroptera, length(order)) +
  theme(legend.title = element_blank())
chiroptera_waffle






#PRIMATES
colorset_primates <- 
  c(alpha("#7FC97F", .25), alpha("#BEAED4", .25), alpha("#FDC086", .25), alpha("#66FF33", .25), alpha("#BF5B17", .25), 
    alpha("#666666", .25), alpha("#1B9E77", .25), alpha("#D95F02", .25), alpha("#7570B3", .25), alpha("#E7298A", .25),
    alpha("#66A61E", .25), alpha("#E6AB02", .25), alpha("#A6761D", .25), alpha("#A6CEE3", .25), alpha("#1F78B4", .25),
    alpha("#B3E2CD", .25), alpha("#FDDAEC", .25), alpha("#E41A1C", .25), alpha("#80B1D3", .25), alpha("#B3DE69", .25),
    alpha("#F781BF", .25), "#1A1AFF", alpha("#FB8072", .25), alpha("#A65628", .25), alpha("#CCEBC5", .25),
    alpha("#999999", .25), alpha("#F0027F", .25))


primates_waffle <- 
  waf +
  geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
  scale_fill_manual(values = colorset_primates, length(order)) +
  theme(legend.title = element_blank())
primates_waffle


#TUBULIDENTATA
colorset_tubulidentata <- 
  c(alpha("#7FC97F", .25), alpha("#BEAED4", .25), alpha("#FDC086", .25), alpha("#66FF33", .25), alpha("#BF5B17", .25), 
    alpha("#666666", .25), alpha("#1B9E77", .25), alpha("#D95F02", .25), alpha("#7570B3", .25), alpha("#E7298A", .25),
    alpha("#66A61E", .25), alpha("#E6AB02", .25), alpha("#A6761D", .25), alpha("#A6CEE3", .25), alpha("#1F78B4", .25),
    alpha("#B3E2CD", .25), alpha("#FDDAEC", .25), alpha("#E41A1C", .25), alpha("#80B1D3", .25), alpha("#B3DE69", .25),
    alpha("#F781BF", .25), alpha("#1A1AFF", .25), alpha("#FB8072", .25), alpha("#A65628", .25), alpha("#CCEBC5", .25),
    alpha("#999999", .25), "#F0027F")


tubulidentata_waffle <- 
  waf +
  geom_waffle(n_rows = 60, size = .33, colour = "white", flip = FALSE) +
  scale_fill_manual(values = colorset_tubulidentata, length(order)) +
  theme(legend.title = element_blank())
tubulidentata_waffle



#MONOTREMES
colorset_monotremata <- 
  c(alpha("#7FC97F", .25), alpha("#BEAED4", .25), alpha("#FDC086", .25), alpha("#7FFF00", .25), alpha("#BF5B17", .25), 
    alpha("#666666", .25), alpha("#1B9E77", .25), alpha("#D95F02", .25), alpha("#7570B3", .25), alpha("#E7298A", .25),
    alpha("#66A61E", .25), alpha("#E6AB02", .25), alpha("#A6761D", .25), alpha("#A6CEE3", .25), "#1F78B4",
    alpha("#B3E2CD", .25), alpha("#FDDAEC", .25), alpha("#E41A1C", .25), alpha("#80B1D3", .25), alpha("#B3DE69", .25),
    alpha("#F781BF", .25), alpha("#1A1AFF", .25), alpha("#FB8072", .25), alpha("#A65628", .25),  alpha("#CCEBC5", .25),
    alpha("#999999", .25), alpha("#F0027F", .25))


monotremata_waffle <- 
  waf +
  geom_waffle(n_rows = 40, size = .5, colour = "white", flip = FALSE) +
  scale_fill_manual(values = colorset_monotremata, length(order)) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=7)) +
  labs(title = "THE MONOTREMES", caption = "Source: ASM Mammal Diversity Database") +
  theme(plot.title = element_text(hjust = 0.5))

monotremata_waffle

