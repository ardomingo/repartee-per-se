#HOW MANY US PRESIDENTS HAVE BEEN ALIVE AT ANY ONE TIME?


library(tidyverse)
library(rvest)
library(timevis)
library(lubridate)



###Life Dates

#Read html table
html_dl <- read_html("https://www.presidentsusa.net/birth.html")

#Pull table data (use Google Chrome & SelectorGadget to find element)
bd_dates <- html_dl %>% 
              html_element(".callout") %>% 
              html_table()



#Convert dates
bd_dates$`Birth Date` <- mdy(bd_dates$`Birth Date`)
bd_dates$`Death Date` <- mdy(bd_dates$`Death Date`)

#Match President Franklin Roosevelt to Franklin D. Roosevelt
bd_dates$President[31] <- "Franklin D. Roosevelt"
bd_dates$President[40] <- "George H.W. Bush"


### Term Dates

#Read table
html_terms <- 
  read_html("https://www.presidentsusa.net/presvplist.html")

term_dates <- html_terms %>% 
  html_element(".col-md-9") %>% 
  html_table()

term_dates <- term_dates[, 1:3]


#Convert dates
term_dates$`Term Began` <- mdy(term_dates$`Term Began`)
term_dates$`Term Ended` <- mdy(term_dates$`Term Ended`)

#Match "President" column
colnames(term_dates)[1] <- "President"

#Match President James Garfield to James A. Garfield
term_dates$President[20] <- "James A. Garfield"
term_dates$President[41] <- "George H.W. Bush"


#Prep timevis-usable dataframe

#Create dataframe for timeline
pres_set <- left_join(term_dates, bd_dates)            

#Set NA death dates to today() to ensure plots for living presidents extend all the way to the edge of the graph
pres_set$`Death Date`[is.na(pres_set$`Death Date`)] <- today()


###Add party affiliation from github repo: 
  #(https://gist.githubusercontent.com/namuol/2657233/raw/74135b2637e624848c163759be9cd14ae33f5153/presidents.csv)

party_table <- read.csv("https://raw.githubusercontent.com/awhstin/Dataset-List/master/presidents.csv")


#Match names to pres_set dataframe
colnames(party_table)[1] <- "President"
party_table$President[39] <- "Jimmy Carter"
party_table$President[42] <- "Bill Clinton"
party_table$President[21] <- "Chester Arthur"
party_table$President[9] <- "William Henry Harrison"
party_table$President[34] <- "Dwight Eisenhower"
#Correct and add Biden data
party_table$Years.In.Office[45] <- "2017-2021"

party_table[nrow(party_table) + 1,] = c("Joe Biden","2021-", "46th", "Democratic")

pres_set <- left_join(pres_set, party_table, by = "President")
pres_set <- pres_set[c(23,25), ]



#Set up dataframes for timeline rendering
groups <- data.frame(
  id = unique(c(pres_set$Party)), 
  content = unique(c(pres_set$Party)))



#Setup dataframe
pres_timeline <- data.frame(
  #id      = pres_set$President,
  content = pres_set$President,
  start   = pres_set$`Term Began`,
  end     = pres_set$`Death Date`,
  group = pres_set$Party)


#Render vis
timevis(pres_timeline, groups = groups)


%>% 
     addCustomItem(2006-12-26:2009-01-20, "last year") 




#Setup dataframe
pres_timeline1 <- data.frame(
  #  id      = 1:length(bd_dates),
  content = term_dates$Presidents,
  start   = term_dates$`Term Began`,
  end     = term_dates$`Term Ended`
)

timevis(pres_timeline1)


library(widgetframe)

framed_timeline <- timevis(pres_timeline)
frameWidget(framed_timeline)








saveWidget(
  pres_timeline,
  PresTimeline,
  selfcontained = TRUE,
  libdir = NULL,
  background = "white",
  title = class(widget)[[1]],
  knitrOptions = list()
)





#Render vis
timevis(pres_timeline) 

