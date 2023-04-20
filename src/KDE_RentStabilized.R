# KDE 
library(eks)
library(ks)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
#library(sp)

setwd("~/Documents/nycdsa/projects/python-web-scraping/data")

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
df = bind_rows(data) %>% filter(ZIP != "ZIP", STATUS1 != "MULTIPLE DWELLING B", !is.na(lat), is.na(STATUS2)) %>% distinct(loc, .keep_all = T) %>% dplyr::select(Address, loc, lat, lon) %>%
  mutate(`Income Designation` = 'No Restrictions', 
  `Max Income Restriction(%AMI)` = "No Restrictions", `Occupancy Demographic` = "All Eligable", `subsidy_program_full` = "Rent Stabilization")

nyc_shp = st_read("./Borough Boundaries/", "geo_export_da053023-9b69-4e71-bbff-eb976f919a31") %>% st_transform(4326)

df_geo = st_as_sf(df, coords = c('lon', 'lat')) %>% st_set_crs(4326)
df_geo_withboro = st_join(df_geo, nyc_shp)
df_geo_withboro = df_geo_withboro %>% dplyr::select(-c(Address, boro_code, shape_area, geometry, shape_leng)) %>%
  mutate(longitude = unlist(map(geometry, 1)),
             latitude = unlist(map(geometry, 2))) %>%
  st_drop_geometry()

write_csv(df_geo_withboro, "./L1/rent_stabilized_clean.csv")
# CONVERT TO SF OBJECT
df_geo = st_as_sf(df, coords = c('lon', 'lat')) %>% st_set_crs(4326) %>% st_transform(6347) %>% mutate(easting = unlist(map(geometry, 1)),
                                                                                                       northing = unlist(map(geometry, 2))) 
x = df_geo$easting
y = df_geo$northing

X = as.matrix(x = cbind(x, y))

# Bivariate smoothed cross validation bandwidth estimator
hpi.est = Hpi(x=X)

Hscv_diag = Hscv.diag(x=X, Hstart = hpi.est, optim.fun = "nlm")


# TIDY KDE + plot
fhat = st_kde(x = df_geo, H = Hscv_diag)

gs1 <- ggplot(fhat) + geom_sf(data=df_geo, fill=NA, alpha = 0.05) +
  ggthemes::theme_map()
gs1 + geom_sf(data=st_get_contour(fhat, cont = c(25, 50, 75, 95)),
                       ggplot2::aes(fill=label_percent(contlabel))) +
  colorspace::scale_fill_discrete_sequential(palette="Heat2") 

# save out as rdata file
save(fhat, file = "./L1/rent_stabilized_st_kde_obj.rdata")

# write shapefile of contours
contour_subset = st_get_contour(fhat, c(25, 50, 75, 99))
st_write(contour_subset, "./L1/rent_stabilized_kde_contours.shp", delete_layer = TRUE)

# write to raster as well
r = raster(list(x=fhat$tidy_ks$ks[[1]]$eval.points[[2]], y = fhat$tidy_ks$ks[[1]]$eval.points[[2]], z = fhat$tidy_ks$ks[[1]]$estimate))
# set crs
crs(r) <- CRS('+init=EPSG:6347')

r_test = projectRaster(r, ref_r)

ref_r = raster(x = "./L1/tree_counts_per_ha.tiff")
# this is our reference raster (1 ha resolution)
values(ref_r)[!is.na(values(ref_r))] = 1

# crop it by nyc shape
nyc = st_read("./Borough Boundaries/", layer = "geo_export_da053023-9b69-4e71-bbff-eb976f919a31") %>% st_transform(6347)
nyc = nyc %>% st_buffer(0.5) %>% st_union() %>% st_sf()
nyc = as(nyc, "Spatial")


r = setExtent(x = r, ext = ref_r@extent)
# Resample so same resolution as tree data
x = resample(x = r, y = ref_r, method = "bilinear")

plot(x)
raster::compareRaster(x, ref_r)

# crop and mask
a = crop(x, extent(nyc))
x = mask(a, nyc)





# scale it
v = values(x)
x.scale = ((x - min(values(x), na.rm = T)) / (max(values(x), na.rm = T) - min(values(x), na.rm = T)) - 0.5 ) * 2



plot(x.scale)

# Write rasters
writeRaster(xcrop, filename = "./L1/KDE_RentStabilized_1ha_unscaled.tiff", overwrite = T)
writeRaster(x.scale, filename = "./L1/KDE_RentStabilized_1ha_scaled.tiff", overwrite = T)