# KDE 

library(ks)
library(tidyverse)
library(sf)
library(raster)

setwd("~/Documents/nycdsa/projects/python-web-scraping/data")

files = list.files(path = "./L1/geocoded/", pattern = "_geocoded.csv", full.names = T)

data = lapply(files, FUN = read_delim, delim = '\t', col_types = cols(ZIP = col_character(),
                                                                      COUNTY = col_character())) 


# FILTER OUT DWELLING B
#A Class B multiple dwelling is “a multiple dwelling which is occupied, as a rule, transiently, 
#as the more or less temporary abode of individuals or families who are lodged with or without meals. 
#This class includes hotels, lodging houses, rooming houses, boarding houses, boarding schools, 
#furnished room houses, lodgings, club houses, and college and school dormitories.” 
df = bind_rows(data) %>% filter(ZIP != "ZIP", STATUS1 != "MULTIPLE DWELLING B", !is.na(lat)) %>% distinct(loc, .keep_all = T) %>% dplyr::select(Address,loc, lat, lon)



df_geo = st_as_sf(df, coords = c('lon', 'lat')) %>% st_set_crs(4326) %>% st_transform(6347) %>% mutate(easting = unlist(map(geometry, 1)),
                                                                                                       northing = unlist(map(geometry, 2))) 
x = df_geo$easting
y = df_geo$northing

X = as.matrix(x = cbind(x, y))

# Hpi1 = Hpi(x=X)
# Hpi2 = Hpi.diag(x=X)
# fhat.pi1 = kde(x = X, H = Hpi1)
# fhat.pi2 = kde(x=X, H = Hpi2)


# Bivariate smoothed cross validation bandwidth estimator
hpi.est = Hpi(x=X)

Hscv_diag = Hscv.diag(x=X, Hstart = hpi.est, optim.fun = "nlm")

Hscv.est = Hscv(x=X, Hstart = hpi.est, optim.fun = "nlm")

# Least squares - not well behaved when there are repeated coords. 
# Hlscv_diag2 = Hlscv.diag(x=X, Hstart = Hlscv_diag, optim.fun = "optim")

treedens = raster(x = "./Documents/nycdsa/projects/python-web-scraping/data/L1/tree_counts_per_ha.tiff")
ext = treedens@extent
ep = rasterToPoints(x = treedens)

# Calculate grid size
gridsize.x <- diff(range(ep[,1]))/50 # range divided by resolution
gridsize.y <- diff(range(ep[,2]))/50



fhat.scv.diag = kde(x=X, H = Hscv_diag, binned = FALSE, gridsize = c(gridsize.x, gridsize.y), xmin = c(ext@xmin, ext@ymin), xmax = c(ext@xmax, ext@ymax))
fhat.scv = kde(x=X, H = Hscv.est, binned = FALSE, gridsize = c(gridsize.x, gridsize.y), xmin = c(ext@xmin, ext@ymin), xmax = c(ext@xmax, ext@ymax))

plot(fhat.scv.diag)
plot(fhat.scv)
# the scmoothed cross validation method of determining bandwidth distinguishes high density regions better

r = raster(list(x=fhat.scv.diag$eval.points[[1]], y = fhat.scv.diag$eval.points[[2]], z = fhat.scv.diag$estimate))
r1 = raster(list(x=fhat.scv$eval.points[[1]], y = fhat.scv$eval.points[[2]], z = fhat.scv$estimate))

crs(r) <- CRS('+init=EPSG:6347')
crs(r1) <- CRS('+init=EPSG:6347')

plot(r1 - r)

r1 = setExtent(x = r1, ext = treedens@extent, keepres = FALSE)
r = setExtent(x = r, ext = treedens@extent, keepres = FALSE)


# Resample so same resolution as tree data
x = resample(x = r, y = treedens)
x1 = resample(x = r1, y = treedens)
plot(x1)
plot(x)

compareRaster(x, treedens)

