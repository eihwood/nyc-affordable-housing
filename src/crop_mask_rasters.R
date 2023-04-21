# CROP AND MASK RASTERS
library(sf)
library(sp)
library(raster)

# read in borogh shapefiles
nyc = st_read("./Borough Boundaries/", layer = "geo_export_da053023-9b69-4e71-bbff-eb976f919a31") %>% st_transform(6347)
nyc = nyc %>% st_buffer(0.5) %>% st_union() %>% st_sf()
nyc = as(nyc, "Spatial")
plot(nyc)

# make it the same as the heat raster
ref_raster = raster('./L1/KDE_subsidized_1ha_scaled_match.tiff')

# read raster 
r = raster("./L1/rentstabilized_pointdens_ha.tiff")
plot(r)


# crop and mask
a = crop(r, extent(nyc))
xcrop = mask(a, nyc)

plot(xcrop)

x_proj = projectRaster(xcrop, ref_raster)
compareRaster(x_proj, ref_raster)

writeRaster(x = x_proj, filename = "./L1/rentstabilized_pointdens_ha_match.tif", overwrite = T)
