

library(tidyverse)
library(ggmap)
library(stringr)
library(leaflet)
library(gganimate)



#Load df (Source: https://www.lasvegas360.com/3428/in-n-out-burger-locations-and-store-numbers/)
in_n_out <- 
  read.csv("X:/OneDrive/02. Data/Geographic Data/Restauraunts, Hospitality, Entertainment/In N Out Location History.csv")

key = "AIzaSyAfWDmvI2nUHBqvnGXt97cIZ1l40R93Me8"
register_google(key = "AIzaSyAfWDmvI2nUHBqvnGXt97cIZ1l40R93Me8")

#Convert dates
in_n_out$DateOpened <- lubridate::mdy(in_n_out$DateOpened)

#Rename 1st column
colnames(in_n_out)[1] <- "Number"
glimpse(in_n_out)

#Add 'addresses' column for ggmap geocoding
in_n_out <- 
in_n_out %>% 
  mutate(addresses = paste0(str_trim(Address, side = "both"), ", ", 
                                    str_trim(City, side = "both"), ", ",
                                    str_trim(State, side = "both")))
glimpse(in_n_out)



# Initialize the data frame
geocoded <- as.data.frame(stringsAsFactors = FALSE)

geo <- as.data.frame(in_n_out)


# Loop through the addresses to get the latitude and longitude of each address and add it to the
for(i in 1:nrow(geo))
  {
    result <- geocode(in_n_out$addresses[i], output = "latlona", source = "google", 
                      key = key, rownames(in_n_out)<-1:nrow(in_n_out))
    geo$lon[i] <- as.numeric(result[1])
    geo$lat[i] <- as.numeric(result[2])
    geo$geoAddress[i] <- as.character(result[3])
  }

geo$Year <- format(geo$DateOpened, "%Y")

glimpse(geo)

year_table <- 
  geo %>% group_by(Year) %>% 
  summarise(Opened = n())

state_table <- 
  geo %>% group_by(State) %>% 
  summarise(Total = n())

leaflet(geo) %>% addTiles() %>% 
  addMarkers(lat = geo$lat, lng = geo$lon) 


leaflet(geo) %>% addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

m_dots <- 
leaflet(geo) %>% addTiles() %>% 
  addCircleMarkers(lat = geo$lat, lng = geo$lon,
                   radius = 3, color = "red", 
                   clusterOptions = markerClusterOptions())

m_dots +
  transition_states(DateOpened, wrap = FALSE)


library(ggplot2)
library(maps)
library(ggthemes)
library(sf)

usa <- ggplot() +
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

map <- usa +
  geom_point(aes(x = lon, y = lat),
             data = geo, 
             colour = 'purple', alpha = .5) +
  geom_sf(data = states, color = "white") +
  coord_sf(xlim = c(-130, -90), ylim = c(25, 50), expand = TRUE)
map


  geom_point(aes(x = lon, y = lat),
             data = geo, 
             colour = 'gray80', alpha = .5)


#+
  geom_polygon(data = states, aes(x = long, y = lat, color = "white"))
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

  
  
  
  
  