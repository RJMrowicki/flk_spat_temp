# flk_spat_temp
# import, tidy and manipulate data

# Import map data ---------------------------------------------------

# specify default projection (UTM zone 21S):
my_proj <-
  "+proj=utm +zone=21 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# (NB -- units in m for rasterisation below)
# WGS84: "+init=epsg:4326"
# Robinson: "+proj=robin"
# Winkel Tripel: "+proj=wintri"

# import Falkland Islands coastline shapefile (polygons):
shp_flk <- readOGR(
  "./data/map/intermediate/flk_coastline_osm_wgs84_poly.shp",
  layer = "flk_coastline_osm_wgs84_poly"
)

# reproject to specified projection:
shp_flk <- spTransform(shp_flk, CRS(my_proj))




# Manipulate map data -----------------------------------------------

# simplify flk shapefile (for plotting):
shp_flk_simple <- ms_simplify(shp_flk, keep = 0.2)




# convert polygons into lines:
flk_coast <- as(shp_flk, "SpatialLinesDataFrame")

# specify desired grid resolution (in m):
grid_res <- 5000

# create grid template for rasterising vector:
grid_template <- raster(
  extent(flk_coast), resolution = grid_res, crs = my_proj
)

# if template x extent is smaller than vector x extent:
if (
  diff(as.matrix(extent(grid_template))["x", ]) <
  diff(as.matrix(extent(flk_coast))["x", ])
) {
  # extend template extent by 1 column (to right):
  grid_template <- modify_raster_margins(grid_template, c(0, 1, 0, 0))
  # # raster::extend() extends by 1 column on *both* sides:
  # grid_template <- extend(grid_template, y = c(0, 1))
}

# and if template y extent is smaller than vector y extent:
if (
  diff(as.matrix(extent(grid_template))["y", ]) <
  diff(as.matrix(extent(flk_coast))["y", ])
) {
  # extend template extent by 1 row:
  grid_template <- modify_raster_margins(grid_template, c(0, 1, 0, 0))
  # grid_template <- extend(grid_template, y = c(1, 0))
}

# extract aspect ratio of template (for plotting):
asp <- ncol(grid_template) / nrow(grid_template)




# use template for rasterization of flk coast:
# (NB -- this takes a **long time!**)
system.time(  # measure execution time
  flk_coast_raster <- rasterize(flk_coast, grid_template)
)




# Import sites and specimens data -----------------------------------

# sites:
dd_sites <- read_csv("./data/flk_sites_DPLUS068.csv")

# convert survey start and end times into date-times:
dd_sites <- dd_sites %>% mutate(
  time_start = ymd_hms(paste(date, time_start)),
  time_end = ymd_hms(paste(date, time_end))
)




# specimens:
dd_specimens_DPLUS068 <- read_csv("./data/flk_specimens_DPLUS068.csv")
dd_specimens_herb <- read_csv("./data/flk_specimens_herb.csv")

# combined specimens:
dd_specimens <- dd_specimens_DPLUS068  # (NB -- temporary)




# create vector of taxa (NB -- based on determined **name**)
# as a basis for analysing 'per taxon' distribution data:
taxa <- unique(dd_specimens$det_name)
taxa <- taxa[!is.na(taxa)]  # remove 'NA' category

# create empty list with one entry per taxon for storing site coordinates:
taxa_coords <- vector("list", length(taxa))
names(taxa_coords) <- taxa  # name list entries according to taxa
taxa <- taxa[order(taxa)]  # sort in alphabetical order


# i <- taxa[1]  ### test

for (i in taxa) {  # for each taxon,
  # filter specimen data for this taxon:
  use_dat <- dd_specimens %>% filter(det_name == i)
  # extract unique site codes associated with specimens:
  taxon_sites <- sort(unique(use_dat$site_code))
  # obtain coordinates associated with these site codes:
  taxon_coords <- SpatialPoints(  # convert to SpatialPoints object
    tibble(  # (instead of data.frame)
      # extract lon and lat (**mean**; matched from dd_sites) for each site:
      x = dd_sites$long_mean[match(taxon_sites, dd_sites$site_code)],
      y = dd_sites$lat_mean[match(taxon_sites, dd_sites$site_code)]
    ), CRS("+init=epsg:4326")  # (NB -- in **WGS84 projection**)
  )
  # assign to corresponding list item (NB -- **reproject**)
  taxa_coords[[i]] <- spTransform(taxon_coords, CRS(my_proj))
}


taxa_rasters <- taxa_coords  # copy list of coordinates

for (i in taxa) {  # for each taxon,
  # rasterise point coordinates based on grid template:
  taxa_rasters[[i]] <- rasterize(
    taxa_coords[[i]], grid_template, field = 1, crs = my_proj)
}  
