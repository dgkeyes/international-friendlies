# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(maps)
library(ggmap)
library(leaflet)
library(kableExtra)


# GET DATA ----------------------------------------------------------------

# Data from https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017/kernels

intl.friendlies <- read_csv("results.csv") %>%
     filter(tournament == "Friendly") %>%
     filter(date >= as.Date("2000-01-01")) %>%
     mutate(played_outside_country = if_else(home_team != country, "Y", "N")) 


# CREATE SUMMARY DATASET --------------------------------------------------

intl.friendlies.summary <- intl.friendlies %>%
     group_by(home_team) %>%
     count(played_outside_country) %>%
     mutate(pct = prop.table(n)) %>%
     mutate(pct = round(pct, digits = 2)) %>%
     filter(played_outside_country == "Y") %>%
     filter(pct >= .25 & n > 5) %>%
     arrange(pct)

countries.to.include <- unique(intl.friendlies.summary$home_team)



# CREATE FRIENDLY LOCATIONS TIBBLE ----------------------------------------

intl.friendlies.locations <- intl.friendlies %>%
     mutate(city = str_replace(city, "Ch├óteauroux", "Chateauroux")) %>%
     mutate(country = str_replace(country, "Serbia and Montenegro", "Serbia")) %>%
     mutate(city = str_replace(city, "Kruševac", "Krusevac")) %>%
     mutate(city_country = paste(city, country, sep = ", ")) %>%
     filter(str_detect(home_team, paste(countries.to.include, collapse = "|"))) # Only keep countries in summary dataset above

# Get database of world cities

cities <- world.cities %>%
     mutate(city_country = paste(name, country.etc, sep = ", "))

# Add geocode data from above database for as many cities as match

friendly.locations <- intl.friendlies.locations %>%
     distinct(city_country, .keep_all = TRUE) %>%
     left_join(cities, by = "city_country") 


# Geocode cities that aren't matched above

# register_google(key = "AIzaSyB-xmJN6XDy-I0SAT4ORtqcEZMyfpEe-uI")
# 
# friendly.locations.no.geo <- friendly.locations %>%
#      filter(is.na(long)) %>%
#      mutate_geocode(city_country)

# Merge locations datasets

friendly.locations.merged <- left_join(friendly.locations,
                                       friendly.locations.no.geo,
                                       by = "city_country")


# CREATE LOCATIONS DATASET ----------------------------------------------------------

intl.friendlies.locations <- intl.friendlies %>%
     mutate(city_country = paste(city, country, sep = ", ")) %>%
     left_join(friendly.locations.merged, by = "city_country") %>%
     filter(str_detect(home_team.x, paste(countries.to.include, collapse = "|"))) %>%  # Only keep countries in summary dataset above 
     mutate(lat.x = if_else(is.na(lat.x), lat1, lat.x)) %>%
     mutate(long.x = if_else(is.na(long.x), lon, long.x)) %>%
     select(date:city_country, lat.x, long.x) %>%
     rename("lat" = lat.x) %>%
     rename("lon" = long.x)


# GET WORLD MAP GEO DATA --------------------------------------------------------

world.map <- map_data(map = "world")


# PLOT FRIENDLIES ---------------------------------------------------------

temp <- intl.friendlies.locations %>%
     filter(home_team == countries.to.include[47])

ggplot() +
     geom_polygon(data = world.map,
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  fill = "white",
                  color = "gray") +
     geom_polygon(data = filter(world.map, region == countries.to.include[47]),
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  color = "#DD8233",
                  fill = "white") +
     geom_point(data = filter(temp, played_outside_country == "Y"), 
                aes(x = lon, y = lat),
                color = "#DD8233") +
     geom_point(data = filter(temp, played_outside_country == "N"), 
                aes(x = lon, y = lat),
                color = "#eeeeee") +
     theme_void() 


# PLOT INTERACTIVELY ------------------------------------------------------

leaflet(options = leafletOptions(minZoom = 12))

# Define leaflet function

dk_map_friendlies <- function(filtercountry) {
     
     
     friendlies.for.map <- intl.friendlies.locations %>%
          filter(played_outside_country == "Y") %>%
          filter(home_team == filtercountry)
     
     leaflet() %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addCircleMarkers(lng=friendlies.for.map$lon, 
                      lat=friendlies.for.map$lat,
                      color = "#DD8233",
                      stroke = FALSE,
                      popup = paste("<strong>", 
                                    friendlies.for.map$home_team,
                                    " v ",
                                    friendlies.for.map$away_team, 
                                    "</strong><br>", 
                                    friendlies.for.map$date, 
                                    "<br>", friendlies.for.map$city_country),
                      fillOpacity = .9) 
}

# Mexico

dk_map_friendlies("Mexico")
dk_map_friendlies("El Salvador")

# Brazil

friendlies.for.map <- intl.friendlies.locations %>%
     filter(played_outside_country == "Y") %>%
     filter(home_team == "Brazil")

dk_map_friendlies()


