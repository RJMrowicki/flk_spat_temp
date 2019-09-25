# flk_spat_temp
# import, tidy and manipulate data

# Import sites and specimens data ====================================

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




# Manipulate specimens data =========================================

# ~ historical:
use_dd_specimens_herb <- dd_specimens_herb %>%
  # create new column for **mean** year:
  mutate(
    year = pmap_dbl(  # row-wise purrr:pmap(), instead of apply()
      list(fromY_new, toY_new),  # list of vectors to pass to function
      ~ year(mean(c(...), na.rm = TRUE))  # c(...) converts elements into vector
    )) %>%
  # rename coordinates and extent columns:
  rename(lat = decLat_new, lon = decLon_new, extent = extent_m) %>%
  # exclude any rows with NA in either year or coordinate columns:
  filter_at(vars(year, lat, lon), all_vars(!is.na(.))) %>%
  # extract columns for name, year and coordinates:
  dplyr::select(det_name, year, lat, lon, extent)


# ~ contemporary:
use_dd_specimens_DPLUS068 <- dd_specimens_DPLUS068 %>%
  # join site data (all columns) via lookup:
  left_join(dd_sites, by = "site_code") %>%
  # create new column for year:
  mutate(year = year(date)) %>%
  # rename **mean** coordinates columns:
  rename(lat = lat_mean, lon = long_mean) %>%
  # create column for extent (30m for GPS coordinates):
  mutate(extent = 30) %>%
  # extract columns for group, name, year, coordinates and extent:
  dplyr::select(det_grp, det_name, year, lat, lon, extent)


# combine historical & contemporary data:
dd_specimens <- bind_rows(use_dd_specimens_herb, use_dd_specimens_DPLUS068)




# specify year range (for extracting min and max):
year_range <- range(dd_specimens$year, na.rm = TRUE)

# determine year group breaks:
breaks <- c(year_range[1], seq(1850, 2000, 50), year_range[2])

dd_specimens <- dd_specimens %>%
  # create new column for year group:
  mutate(year_grp = cut(year, breaks, include.lowest = TRUE, dig.lab = 4))

# extract year group levels:
year_grps <- levels(dd_specimens$year_grp)




# create vector of taxa (NB -- based on determined **name**)
# as a basis for analysing 'per taxon' distribution data:
taxa <- unique(dd_specimens$det_name)
taxa <- taxa[!is.na(taxa)]  # remove 'NA' category
taxa <- taxa[order(taxa)]  # sort in alphabetical order




# specify coordinates for Stanley (see georeferencing protocol):
stanley_coords <- t(matrix(c(-57.85954, -51.69458)))  # x, y

# create table of all unique coordinates:
all_coords <- dd_specimens %>%
  # extract all unique lat-long combinations:
  distinct_at(vars(lon, lat), .keep_all = TRUE) %>%
  filter(extent <= 500) %>%  # (NB -- **extent <= 500 m**)
  # calculate distance to Stanley (in m):
  mutate(dist_stanley = pmap_dbl(  # pmap() instead of apply()
    list(lon, lat),  # list of vectors to pass to function,
    # using 'c(...)' to convert elements into vectors:
    ~ 1000 * spDists(t(matrix(c(...))), stanley_coords, longlat = TRUE)
  )) %>%
  # select coordinate, extent and distance columns only:
  dplyr::select(lon, lat, extent, dist_stanley)

# calculate maximum nearest neighbour distance (in m)
# separately for coordinates 'near to' and 'far from' Stanley,
# based on arbitrary cutoff distance of **5 km** (in m):
max_dist_near <- all_coords %>%
  filter(dist_stanley <= 5000) %>% nndists %>% max(na.rm = TRUE)
max_dist_far <- all_coords %>%
  filter(dist_stanley > 5000) %>% nndists %>% max(na.rm = TRUE)




# Import map data ===================================================

# specify default projection:
my_proj <-
  "+init=epsg:32721"  # UTM zone 21S
  # "+proj=utm +zone=21 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
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




# Manipulate map data ===============================================

# simplify flk shapefile (for plotting):
shp_flk_simple <- ms_simplify(shp_flk, keep = 0.1)




# convert polygons into lines:
flk_coast <- as(shp_flk_simple, "SpatialLinesDataFrame")  # shp_flk?

# specify desired grid resolution (in m):
# (NB -- max nearest neighbour distance between points 'far from' Stanley,
# rounded to nearest x km [expressed in m], where x = 2^n, where n is an integer;
# i.e. may be reached via doubling of 2km*2km grid for IUCN area of occurence)
grid_res <- (2 ^ round(log2( max_dist_far/1000 ))) * 1000
# round(x, digits = -3)  # simple rounding to nearest 1000




# create grid template for rasterising vector:
grid_template <- raster(
  extent(flk_coast), resolution = grid_res, crs = my_proj
)

# if template xmax is smaller than vector xmax:
if (xmax(grid_template) < xmax(flk_coast)) {
  # extend template extent by n columns (to right):
  ext_xn <- ceiling(  # difference expressed in n columns
    diff(c(xmax(grid_template), xmax(flk_coast))) / grid_res
  )
  ext_x <- extent(grid_template)  # copy template extent
  # increase xmax by n columns * grid resolution:
  xmax(ext_x) <- xmax(grid_template) + (ext_xn * grid_res)
  # update template x extent (and dimensions):
  grid_template <- extend(grid_template, ext_x)
  # raster::extend() extends by n columns on *both* sides;
  # raster::update_raster_margins()
}

# and if template ymin is greater than vector ymin:
if (ymin(grid_template) > ymin(flk_coast)) {
  # extend template extent by n rows (to bottom):
  ext_yn <- ceiling(  # difference expressed in n rows:
    diff(c(ymin(flk_coast), ymin(grid_template))) / grid_res
  )
  ext_y <- extent(grid_template)  # copy template extent
  # decrease ymin by n rows * grid resolution:
  ymin(ext_y) <- ymin(grid_template) - (ext_yn * grid_res)
  # update template y extent (and dimensions):
  grid_template <- extend(grid_template, ext_y)
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




# create empty list for storing site coordinates per taxon/year group:
# ~ create blank list for storing data per year group:
a <- vector("list", length(year_grps))
names(a) <- year_grps  # name list entries according to year group
# ~ create list of year group lists with one entry per taxon:
taxa_coords <- rep(list(a), length(taxa))
names(taxa_coords) <- taxa  # name list entries according to taxa


# i <- taxa[1]  ### test
# j <- year_grps[1]  ### test

for (i in taxa) {  # for each taxon,
  # filter specimen data for this taxon:
  taxon_dat <- dd_specimens %>% filter(det_name == i)
  for (j in year_grps) {  # for each year group,
    # only if year group represented in taxon data:
    if (j %in% taxon_dat$year_grp) {
      taxon_coords <- taxon_dat %>%
        # filter taxon data for this year group:
        filter(year_grp == j) %>%
        # extract unique locations associated with specimens:
        distinct_at(vars(lon, lat)) %>%  # (NB -- **x before y**)
        # convert to SpatialPoints object (NB -- **WGS84 projection**):
        SpatialPoints(CRS("+init=epsg:4326"))
      # assign to corresponding list item (NB -- **reproject**)
      taxa_coords[[i]][[j]] <- spTransform(taxon_coords, CRS(my_proj))
    }
  }
}




taxa_rasters <- taxa_coords %>%
  # rasterise point coordinates based on grid template
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    rasterize(., grid_template, field = 1, crs = my_proj)
  })




taxa_aoo <- taxa_rasters %>%
  # calculate **area** of occupancy (in km^2)
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    # no. of occupied cells * cell area (in km):
    cellStats(., sum) * prod(res(.)/10^3)
  })




taxa_eoo <- taxa_coords %>%
  # calculate **extent** of occupancy (in km^2)
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    if (length(.) == 3) {  # if 3 points,
      # calculate area of **convex hull** polygon:
      area(chull_poly(coordinates(.), my_proj)) / 10^6
    }
    if (length(.) > 3) {  # if >3 points,
      # calculate **alpha hull** (= alpha shape):
      # (NB -- alpha value based on x coordinate range multiplied by
      # factor large enough to avoid collapsed edges on some datasets,
      # determined by trial and error)
      ashp <- ashape(coordinates(.), alpha = 3 * (extent(.)[2] - extent(.)[1]))
      # convert to polygon and calculate area:
      area(ashape_poly(ashp, my_proj)) / 10^6
    }
  })
