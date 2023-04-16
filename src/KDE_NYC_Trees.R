# KDE 
library(eks)
library(ks)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
#library(sp)

setwd("~/Documents/nycdsa/projects/python-web-scraping/data")

# READ ALL INDIVIDUAL TREE DATA
files = list.files('./greenness/TreePoint', '*.shp$', full.names = T)
dataL = lapply(files, FUN = function(x){
  
  one = sapply(strsplit(x, "/"), "[[", 2)
  two = sapply(strsplit(x, "/"), "[[", 3)
  dsn = paste("./",one, "/", two, "/",sep = "")
  layer = sapply(strsplit(x, "/"), "[[", 4)
  layer = sapply(strsplit(layer, "[.]"), "[[", 1)
  
  st_read(dsn = dsn, layer = layer)
})

data = bind_rows(dataL)

# CONVERT TO SF OBJECT
df_geo = data %>% mutate(easting = unlist(map(geometry, 1)),
                         northing = unlist(map(geometry, 2))) 
x = df_geo$easting
y = df_geo$northing

X = as.matrix(x = cbind(x, y))

# Get gridsize
gridsize.x <- diff(range(X[,1]))/90
gridsize.y <- diff(range(X[,2]))/90



# Bivariate smoothed cross validation bandwidth estimator
# Generate a plugin estimate to use as starting values in the optimization step
hpi.est = Hpi.diag(x=X)

# Least squares - not well behaved when there are repeated coords. 
H = Hscv(x=X, Hstart = hpi.est, optim.fun = "nlm")


# TIDY KDE + plot
fhat = st_kde(x = df_geo, H = H, binned = FALSE)

#gs1 <- ggplot(fhat) + geom_sf(data=df_geo, fill=NA, alpha = 0.05) +
#  ggthemes::theme_map()
#gs1 + geom_sf(data=st_get_contour(fhat, cont = c(25, 50, 75)),
#              ggplot2::aes(fill=label_percent(contlabel))) +
#  colorspace::scale_fill_discrete_sequential(palette="Heat2") 

# save out as rdata file
save(fhat, file = "./L1/tree_kde_obj.rdata")

# write shapefile of contours
contour_subset = st_get_contour(fhat, c(25, 50, 75, 99))
st_write(contour_subset, "./L1/tree_kde_contours.shp", delete_layer = TRUE)


# write to raster as well
r = raster(list(x=fhat$tidy_ks$ks[[1]]$eval.points[[2]], y = fhat$tidy_ks$ks[[1]]$eval.points[[2]], z = fhat$tidy_ks$ks[[1]]$estimate))
# set crs
crs(r) <- CRS('+init=EPSG:6347')

ref_r = raster(x = "./L1/tree_counts_per_ha.tiff")
# this is our reference raster (1 ha resolution)
values(ref_r)[!is.na(values(ref_r))] = 1

r = setExtent(x = r, ext = ref_r@extent)


# Resample so same resolution as tree data
x = resample(x = r, y = ref_r, method = "bilinear")

plot(x)
raster::compareRaster(x, ref_r)

# scale it
v = values(x)
x.scale = ((x - min(v)) / (max(v) - min(v)) - 0.5 ) * 2

# crop it by ref raster
xcrop = x*ref_r
plot(xcrop)

xcrop.scaled = x.scale*ref_r
plot(xcrop.scaled)

writeRaster(xcrop, filename = "./L1/tree_kde_dens_1ha_unscaled.tiff", overwrite = T)
writeRaster(xcrop.scaled, filename = "./L1/tree_kde_dens_1ha_scaled.tiff", overwrite = T)
