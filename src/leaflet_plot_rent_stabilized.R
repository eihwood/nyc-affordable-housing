# LEAFLET PLOT OF RENT STABILIZED UNITS

library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(sf)

# LIST All Affordable housing (no duplicates)
df = read_csv("../data/L1/affordable_housing_with_tree_data_nodupes.csv")

# CONVERT TO SF OBJECT
st_df = st_as_sf(df, coords = c('longitude', 'latitude')) %>% st_set_crs(4326) 

st_df$Group = ifelse(st_df$subsidy_program_full == "Rent Stabilization", 'Rent Regulated', 'Subsidized')

# NTA polygon data
pgs = st_read("./L1/NYC_NTA_extracted.shp") %>% st_set_crs(6347) %>% st_transform(4326)

# Define palettes for NTA canopy data
qpal = colorQuantile("YlGn", pgs$cc_mean, n = 5)
# the extra code building off the existing pal
qpal_colors <- unique(qpal(sort(pgs$cc_mean))) # hex codes
qpal_labs <- round(quantile(pgs$cc_mean, seq(0, 1, .2)),1) # depends on n from pal
qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA


# Define palettes for NTA heat data
qhpal = colorQuantile("RdYlBu", pgs$ha_mean, n = 5, reverse = TRUE)
# the extra code building off the existing pal
qhpal_colors <- unique(qhpal(sort(pgs$ha_mean))) # hex codes
qhpal_labs <- round(quantile(pgs$ha_mean, seq(0, 1, .2)),1) # depends on n from pal
qhpal_labs <- paste(lag(qhpal_labs), qhpal_labs, sep = " - ")[-1] # first lag is NA



subsidized = st_df %>% filter(Group == "Subsidized")
rentreg = st_df %>% filter(Group == "Rent Regulated")

# Assign marker colors based on the subsidized % AMI Targets
subsidized$`Max Income Restriction(%AMI)` = factor(subsidized$`Max Income Restriction(%AMI)`, 
                                                   levels = c("50", "60", "80", "120", "165", "Restrictions Vary"),
                                                   labels = c("50%", "60%", "80%", "120%", "165%", "Restrictions Vary"))
markerCol <- colorFactor(palette = 'Set3', levels = levels(subsidized$`Max Income Restriction(%AMI)`))
markerCol_colors = unique(markerCol(sort(subsidized$`Max Income Restriction(%AMI)`)))

ntas = st_read("./2010 Neighborhood Tabulation Areas (NTAs)/geo_export_65077844-70de-4bb7-91ce-729f59546bad.shp")

# PLOT

m = leaflet(st_df) %>%
  setView(lng = mean(df$longitude), lat = mean(df$latitude), zoom = 10.3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add polygon NTA 
  addPolygons(data = ntas, popup = ~boro_name, label = ~ntaname,stroke = TRUE, smoothFactor = 0.2, color = 'black', fillOpacity = 0.05,opacity = 1,weight = 0.65,fillColor = NULL,group = "Neighborhood Tabulation Areas") %>%
  
  
  # Add polygon NTA canopy layer
  addPolygons(data = pgs, popup = ~boro_name, label = ~ntaname,fillOpacity = 0.75, stroke = FALSE, smoothFactor = 0.2, color = ~qpal(cc_mean), group = "NTA Canopy Cover") %>%
  
  # Add polygon NTA temperature deviation layer
  addPolygons(data = pgs, popup = ~boro_name, label = ~ntaname,fillOpacity = 0.75, stroke = FALSE, smoothFactor = 0.2, color = ~qhpal(ha_mean), group = "NTA Temperature Anomaly") %>%
  
  
  
  # Add Circle Markers for Rent Stabilized Group, coloring by Income Restriction AMI
  addCircleMarkers(data = rentreg, popup = paste("Address: ", rentreg$loc, "<br>",
                                                 "More Info: ", paste("<a href=\"", rentreg$URL , "\">", "click here", "</a>")), 
                   label = ~subsidy_program_full,
                   color = 'maroon',
                   radius = ~sqrt(`Canopy Cover`), group = 'Rent Regulated Housing Locations') %>%
  
  
  # Add Circle Markers for Subsidized Group, coloring by Income Restriction AMI
  addCircleMarkers(data = subsidized, popup = paste("Address: ", subsidized$loc, "<br>",
                                                    "More Info: ", paste("<a href=\"", subsidized$URL , "\">", "click here", "</a>")),
                   color = ~markerCol(`Max Income Restriction(%AMI)`),
                   radius = ~sqrt(`Canopy Cover`), group = 'Subsidized Housing Locations') %>%
  
  # Add legend
  addLegend('bottomright', pal = markerCol, values = subsidized$`Max Income Restriction(%AMI)`,
            title = 'Income Restrictions %AMI',
            opacity = 1, group = "Subsidized Housing Locations") %>%
  addLegend('bottomright', colors = qpal_colors, labels = qpal_labs,
            title = '% Mean Canopy Cover',
            opacity = 1, group = "NTA Canopy Cover") %>%
  addLegend('bottomright', colors = qhpal_colors, labels = qhpal_labs,
            title = 'Temperature Deviation (F)',
            opacity = 1, group = "NTA Temperature Anomaly")
  


# ADD CIRCLE MARKERS DYNAMICALLY (NO LEGEND) BY SUBSIDY PROGRAM
subsidized = subsidized %>% arrange(`Max Income Restriction(%AMI)`)
subL = split(subsidized, subsidized$subsidy_program_full)

for(i in 1:length(subL)){
  df_sub = subL[[i]]
  m = m %>% 
    addCircleMarkers(data = df_sub, popup = paste("Address: ", df_sub$loc, "<br>",
                                                  "More Info: ", paste("<a href=\"", df_sub$URL , "\">", "click here", "</a>")),
                     label = ~`Max Income Restriction(%AMI)`, color = 'blue', 
                     opacity = 0.65, group = unique(df_sub$subsidy_program_full), radius = ~sqrt(`Canopy Cover`)) %>%
    addLegend('bottomright', colors = 'blue', labels = paste('Income Restriction: ',unique(df_sub$`Max Income Restriction(%AMI)`)),
              title = paste(unique(df_sub$subsidy_program_full), " locations"),
              opacity = 1, group = unique(df_sub$subsidy_program_full))
}


  


### Add the 25% Contour Polygons KDEs
# NTA polygon data
reg_kde = st_read("./L1/rent_stabilized_kde_contours.shp") %>% st_transform(4326) %>% filter(contlabel != 99) %>% arrange(contlabel)
sub_kde = st_read("./L1/subsidized_kde_contours.shp")%>% st_transform(4326)%>% filter(contlabel != 99)%>% arrange(contlabel)

reg_kde$contlabel = factor(reg_kde$contlabel, levels = c(25, 50, 75))
reg_kde$label = c("25% KDE", "50% KDE", "75% KDE")
sub_kde$contlabel = factor(sub_kde$contlabel, levels = c(25, 50, 75))
sub_kde$label = c("25% KDE", "50% KDE", "75% KDE")


pal <- colorFactor(c("purple", "lightblue", "#008080"), reg_kde$contlabel)

m = m %>%
  addPolygons(data = reg_kde, popup = ~label, label = ~label,fillOpacity = 0.75, stroke = FALSE, smoothFactor = 0.2, color = ~pal(contlabel), group = "Rent Regulated KDE") %>%
  addPolygons(data = sub_kde, popup = ~label, label = ~label,fillOpacity = 0.75, stroke = FALSE, smoothFactor = 0.2, color = ~pal(contlabel), group = "Subsidized KDE")


m = m %>% # Layers control
  addLayersControl(
    overlayGroups = c( "Neighborhood Tabulation Areas", "NTA Canopy Cover", "NTA Temperature Anomaly", 
                       "Rent Regulated Housing Locations", "Subsidized Housing Locations", unique(subsidized$subsidy_program_full), 
                      "Rent Regulated KDE", "Subsidized KDE"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup(c(unique(subsidized$subsidy_program_full), "Rent Regulated Housing Locations", "Subsidized Housing Locations", "NTA Temperature Anomaly", "NTA Canopy Cover"))


m
saveWidget(m, file = "./rent_stabilized_map_zoomable.html")
