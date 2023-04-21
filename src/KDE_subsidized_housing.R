# KDE 
library(eks)
library(ks)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
#library(sp)

setwd("~/Documents/nycdsa/projects/python-web-scraping/data")

# READ ALL CLEANED RENT SUBSIDIZED HOUSING
data = read_csv("./L1/subsidies.csv")



# CONVERT TO SF OBJECT
df_geo = st_as_sf(data, coords = c('longitude', 'latitude')) %>% st_set_crs(4326) %>% st_transform(6347) %>% mutate(easting = unlist(map(geometry, 1)),
                                                                                                       northing = unlist(map(geometry, 2))) 
x = df_geo$easting
y = df_geo$northing

X = as.matrix(x = cbind(x, y))


# Bivariate smoothed cross validation bandwidth estimator
# Generate a plugin estimate to use as starting values in the optimization step
hpi.est = Hpi.diag(x=X)

# Least squares - not well behaved when there are repeated coords. 
H = Hscv(x=X, Hstart = hpi.est, optim.fun = "nlm")

Hscv_diag = Hscv.diag(x=X, Hstart = hpi.est, optim.fun = "nlm")


# TIDY KDE + plot
fhat = ks::kde(x = X, H = H, binned = FALSE, xmin = c(extnyc[1], extnyc[3]), xmax = c(extnyc[2], extnyc[4]))

gs1 <- ggplot(fhat) + geom_sf(data=df_geo, fill=NA, alpha = 0.05) +
  ggthemes::theme_map()
gs1 + geom_sf(data=st_get_contour(fhat, cont = c(25, 50, 75)),
              ggplot2::aes(fill=label_percent(contlabel))) +
  colorspace::scale_fill_discrete_sequential(palette="Heat2") 

# save out as rdata file
save(fhat, file = "./L1/subsidized_st_kde_obj.rdata")

# write shapefile of contours
contour_subset = st_get_contour(fhat, c(25, 50, 75, 99))
st_write(contour_subset, "./L1/subsidized_kde_contours.shp", delete_layer = TRUE)

# write to raster as well

im.kde = image2Grid(list(x=fhat$tidy_ks$ks[[1]]$eval.points[[1]], y = fhat$tidy_ks$ks[[1]]$eval.points[[2]], z = fhat$tidy_ks$ks[[1]]$estimate))
r <- raster(im.kde)
plot(r)
# set crs
crs(r) <- CRS('+init=EPSG:6347')

im.kde =image2Grid(list(x = fhat$eval.points[[1]], 
                        y = fhat$eval.points[[2]], 
                        z = fhat$estimate))

image(fhat$eval.points[[1]], fhat$eval.points[[2]], fhat$estimate)
contour(im.kde, add = TRUE)

#convert into raster
r <- raster(spkde)
r.cont <- rasterToContour(r,levels=contourLevels(fhat, prob = c(0.25, 0.5, 0.75)))
plot(r.cont)

rgdal::writeOGR(r.cont, dsn = "./L1/", layer = "subsidized_kde_contours_revised.shp", driver = "ESRI Shapefile")

# crop it by nyc shape
nyc = st_read("./Borough Boundaries/", layer = "geo_export_da053023-9b69-4e71-bbff-eb976f919a31") %>% st_transform(6347)
nyc = nyc %>% st_buffer(0.5) %>% st_union() %>% st_sf()
nyc = as(nyc, "Spatial")


# crop and mask
a = crop(r, extent(nyc))
xcrop = mask(a, nyc)
plot(xcrop)



ref_r = raster(x = "./L1/tree_counts_per_ha.tiff")
# this is our reference raster (1 ha resolution)
#values(ref_r)[!is.na(values(ref_r))] = 1


xcrop = setExtent(x = xcrop, ext = ref_r@extent)

# Resample so same resolution as tree data
xcrop = resample(x = xcrop, y = ref_r, method = "bilinear")

plot(xcrop)
raster::compareRaster(xcrop, ref_r)

# scale it
v = values(xcrop)
x.scale = ((xcrop - min(v, na.rm = T)) / (max(v, na.rm = T) - min(v, na.rm = T)) - 0.5 ) * 2



plot(x.scale)

# Write rasters
writeRaster(xcrop, filename = "./L1/KDE_subsidized_1ha_unscaled.tiff", overwrite = T)
writeRaster(x.scale, filename = "./L1/KDE_subsidized_1ha_scaled.tiff", overwrite = T)
