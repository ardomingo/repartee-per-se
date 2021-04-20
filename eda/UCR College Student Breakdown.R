
library(tidyverse)
library(networkD3)
library(readxl)
library(stringr)



ucr <- read_xlsx("X:/OneDrive/02. Data/Education Data/UCR/Degree Awarded_20201210.xlsx", skip = 3)
glimpse(ucr)

ucr$`IPEDS Academic Year` <- as.numeric(str_sub(ucr$`IPEDS Academic Year`, start= -4))
colnames(ucr)[1] <- "Year"
colnames(ucr)[7] <- "Race/Ethnicity"


##################################################################

str(ucr)


unique(ucr$Major)
unique(ucr$College)



# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=ucr$Major, 
  target=ucr$College, 
  value=ucr$Headcount
)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p


# Add a 'group' column to the nodes data frame:
nodes$group <- as.factor(nodes)

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, NodeGroup="group")


p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))


library(collapsibleTree)


collapsibleTree(
  ucr,
  hierarchy = c("College", "Major", "Gender"),
  width = 800,
  zoomable = FALSE
)


ucr %>%
  group_by(College, Major, Gender) %>%
  summarize(Headcount) %>%
  collapsibleTree(
    hierarchy = c("Major", "Gender"),
    root = "ucr",
    width = 1000,
    attribute = "Headcount",
    zoomable = FALSE
  )






##########################################





library(gganimate)
library(ggthemes)

an1 <- 
ggplot(ucr, aes(Headcount, College, fill = Gender)) +
  geom_bar(stat = "identity", width=0.5, position = position_dodge(width=0.75)) +
  theme_hc() +
# Here comes the gganimate code
  transition_time(
  as.integer(Year),
  #transition_length = 7,
  #state_length = 15
) +
  enter_fade() + 
  exit_fade() +
  ease_aes('quadratic-in-out') +
  labs(title = 'Year: {frame_time}')

animate(an1, renderer = gifski_renderer(), height = 600, width = 600,
        detail = 50)



###College Breakdown
ucr_college <- 
  ucr %>% 
  group_by(Year, College) %>% 
  summarize(Participants = n())


college_anim <- 
  ggplot(ucr_college, aes(Participants, College)) +
  geom_bar(stat = "identity", width=0.4, position = position_dodge(width=0.8), fill = "blue") +
  theme_hc() +
  # Here comes the gganimate code
  transition_time(
    as.integer(Year),
    #transition_length = 7,
    #state_length = 15
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('cubic-in-out') +
  labs(title = 'Year: {frame_time}')

animate(college_anim, renderer = magick_renderer(loop = TRUE), height = 600, width = 600,
        detail = 25)

animate(college_anim, renderer = av_renderer(), height = 600, width = 800,
        detail = 25)




###Degree Breakdown
ucr_degree <- 
  ucr %>% 
  group_by(Year, `Degree Type`) %>% 
  summarize(Participants = n())


degree_anim <- 
  ggplot(ucr_degree, aes(Participants, `Degree Type`)) +
  geom_bar(stat = "identity", width=0.4, position = position_dodge(width=0.8), fill = "blue") +
  theme_hc() +
  # Here comes the gganimate code
  transition_time(
    as.integer(Year),
    #transition_length = 7,
    #state_length = 15
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('cubic-in-out') +
  labs(title = 'Year: {frame_time}')

animate(degree_anim, renderer = magick_renderer(loop = TRUE), height = 600, width = 600,
        detail = 25)





###Gender Breakdown
ucr_gender <- 
  ucr %>% 
  group_by(Year, College, Gender) %>% 
  summarize(Participants = n())



gender_anim <- 
  ggplot(ucr_gender, aes(Participants, College, fill = Gender)) +
  geom_bar(stat = "identity", width=0.4, position = position_dodge(width=0.8)) +
  theme_hc() +
  # Here comes the gganimate code
  transition_time(
    as.integer(Year),
    #transition_length = 7,
    #state_length = 15
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('cubic-in-out') +
  labs(title = 'Year: {frame_time}')

animate(gender_anim, renderer = gifski_renderer(), height = 600, width = 800,
        detail = 25)

animate(gender_anim, renderer = av_renderer(), height = 600, width = 800,
        detail = 25)

animate(gender_anim, renderer = magick_renderer(loop = TRUE), height = 600, width = 800,
        detail = 25)
