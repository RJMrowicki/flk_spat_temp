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
# (NB -- this takes a **long time!**, therefore
# may save time by reloading previously saved object)
if ("flk_coast_raster" %in% list.files("./objects")) {
  # if previously saved object in directory, load object:
  load("./objects/flk_coast_raster")
} else {
  # else create new object and save in directory:
  system.time(  # measure execution time
    flk_coast_raster <- rasterize(flk_coast, grid_template)
  )
  save(flk_coast_raster, file = "./objects/flk_coast_raster")
}




# Import sites and specimens data -----------------------------------

# sites:
dd_sites <- read_csv("./data/flk_sites_DPLUS068.csv", na = nas)

dd_sites <- dd_sites %>%
  # convert survey start and end times into date-times:
  mutate(
    time_start = ymd_hms(paste(date, time_start)),
    time_end = ymd_hms(paste(date, time_end))
  )




# specimens:
# ~ historical (herbarium):
# (NB -- use 'guess_max' to avoid attempted parsing of character
# columns as logical, based on default maximum 1000 records.)
dd_specimens_herb <- read_csv(
  "./data/flk_specimens_herb.csv", na = nas, guess_max = 9999
)

dd_specimens_herb <- dd_specimens_herb %>%
  # convert 'from' and 'to' years into dates:
  mutate_at(vars(fromY_new, toY_new), ~ ymd(., truncated = 2)) %>%
  # exclude rows identified in original datasheet:
  filter(is.na(excl))


# ~ contemporary (DPLUS068):
dd_specimens_DPLUS068 <- read_csv(
  "./data/flk_specimens_DPLUS068.csv", na = nas, guess_max = 9999
)




# Manipulate specimens data -----------------------------------------

# ~ historical:
use_dd_specimens_herb <- dd_specimens_herb %>%
  # create new column for **mean** year:
  mutate(
    year = pmap_dbl(  # row-wise purrr:pmap(), instead of apply()
      list(fromY_new, toY_new),  # list of vectors to pass to function
      ~ year(mean(c(...), na.rm = TRUE))  # c(...) converts elements into vector
    )) %>%
  # rename coordinates columns:
  rename(lat = decLat_new, lon = decLon_new) %>%
  # exclude any rows with NA in either year or coordinate columns:
  filter_at(vars(year, lat, lon), all_vars(!is.na(.))) %>%
  # extract columns for name, year and coordinates:
  dplyr::select(det_name, year, lat, lon)


# ~ contemporary:
use_dd_specimens_DPLUS068 <- dd_specimens_DPLUS068 %>%
  # join site data (all columns) via lookup:
  left_join(dd_sites, by = "site_code") %>%
  # create new column for year:
  mutate(year = year(date)) %>%
  # rename **mean** coordinates columns:
  rename(lat = lat_mean, lon = long_mean) %>%
  # extract columns for group, name, year and coordinates:
  dplyr::select(det_grp, det_name, year, lat, lon)


# combine historical & contemporary data:
dd_specimens <- bind_rows(use_dd_specimens_herb, use_dd_specimens_DPLUS068)




# create vector of taxa (NB -- based on determined **name**)
# as a basis for analysing 'per taxon' distribution data:
taxa <- unique(dd_specimens$det_name)
taxa <- taxa[!is.na(taxa)]  # remove 'NA' category
taxa <- taxa[order(taxa)]  # sort in alphabetical order

# create empty list with one entry per taxon for storing site coordinates:
taxa_coords <- vector("list", length(taxa))
names(taxa_coords) <- taxa  # name list entries according to taxa


# i <- taxa[1]  ### test

for (i in taxa) {  # for each taxon,
  taxon_coords <- dd_specimens %>%
    # filter specimen data for this taxon:
    filter(det_name == i) %>%
    # extract unique locations associated with specimens:
    distinct_at(vars(lon, lat)) %>%
    # rename coordinates columns to x & y:
    rename(x = lon, y = lat) %>%
    # convert to SpatialPoints object (NB -- **WGS84 projection**):
    SpatialPoints(CRS("+init=epsg:4326"))
  # assign to corresponding list item (NB -- **reproject**)
  taxa_coords[[i]] <- spTransform(taxon_coords, CRS(my_proj))
}


taxa_rasters <- taxa_coords  # copy list of coordinates

for (i in taxa) {  # for each taxon,
  # rasterise point coordinates based on grid template:
  taxa_rasters[[i]] <- rasterize(
    taxa_coords[[i]], grid_template, field = 1, crs = my_proj)
}  
