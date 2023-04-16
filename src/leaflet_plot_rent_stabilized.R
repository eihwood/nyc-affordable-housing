# LEAFLET PLOT OF RENT STABILIZED UNITS

library(leaflet)
library(tidyverse)
library(htmlwidgets)


# LIST ALL RENT STABILIZED HOUSING
files = list.files(path = "./L1/geocoded/", pattern = "_geocoded.csv", full.names = T)

# READ DELIM FOR LIST OF FILES
data = lapply(files, FUN = read_delim, delim = '\t', col_types = cols(ZIP = col_character(),
                                                                      COUNTY = col_character())) 


# FILTER OUT DWELLING B
#A Class B multiple dwelling is “a multiple dwelling which is occupied, as a rule, transiently, 
#as the more or less temporary abode of individuals or families who are lodged with or without meals. 
#This class includes hotels, lodging houses, rooming houses, boarding houses, boarding schools, 
#furnished room houses, lodgings, club houses, and college and school dormitories.” 
df = bind_rows(data) %>% filter(ZIP != "ZIP", STATUS1 != "MULTIPLE DWELLING B", !is.na(lat)) %>% distinct(loc, .keep_all = T) %>% dplyr::select(Address,loc, lat, lon)


# CONVERT TO SF OBJECT
st_df = st_as_sf(df, coords = c('lon', 'lat')) %>% st_set_crs(4326) 

# PLOT

m = leaflet(st_df) %>%
  setView(lng = mean(df$lon), lat = mean(df$lat), zoom = 10.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(popup = ~loc, label = ~loc, clusterOptions = markerClusterOptions(), color = "seagreen")
  
m
saveWidget(m, file = "./rent_stabilized_map_zoomable.html")
