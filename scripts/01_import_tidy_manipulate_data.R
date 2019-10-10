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
  ) %>%
  # calculate survey duration in minutes:
  mutate(dur_min = time_end - time_start)




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


# ~ recent (Mystikou 2013):
dd_specimens_Mystikou <- read_csv(
  "./data/flk_specimens_Mystikou.csv", na = nas, guess_max = 9999
)

dd_specimens_Mystikou <- dd_specimens_Mystikou %>%
  # exclude rows identified in original datasheet:
  filter(is.na(excl))


# ~ contemporary (DPLUS068):
dd_specimens_DPLUS068 <- read_csv(
  "./data/flk_specimens_DPLUS068.csv", na = nas, guess_max = 9999
)




# Import map data ===================================================

# import Falkland Islands coastline shapefile (polygons):
shp_flk <- readOGR(
  "./data/map/intermediate/flk_coastline_osm_wgs84_poly.shp",
  layer = "flk_coastline_osm_wgs84_poly"
)

# reproject to specified projection:
shp_flk <- spTransform(shp_flk, CRS(my_proj))




# Manipulate specimens data =========================================

# ~ historical:
use_dd_specimens_herb <- dd_specimens_herb %>%
  # create new column for **mean** year:
  mutate(
    year = pmap_dbl(  # row-wise purrr:pmap(), instead of apply()
      list(fromY_new, toY_new),  # list of vectors to pass to function
      ~ year(mean(c(...), na.rm = TRUE))  # c(...) converts elements into vector
    )) %>%
  # rename collector, coordinates and extent columns:
  rename(
    coll = collector_new,
    lat = decLat_new, lon = decLon_new, extent = extent_m
  ) %>%
  # exclude any rows with NA in either year or coordinate columns:
  filter_at(vars(year, lat, lon), all_vars(!is.na(.))) %>%
  # extract columns for group, name, year, collector,
  # location group, coordinates and extent:
  dplyr::select(
    det_grp, det_name, year, coll, loc_grp, lat, lon, extent
  )


# ~ recent:
use_dd_specimens_Mystikou <- dd_specimens_Mystikou %>%
  # create new column for year:
  mutate(year = year(date)) %>%
  # rename group, name and  coordinates columns:
  rename(
    det_grp = GROUP, det_name = NAME_new,
    lat = decLat_gps, lon = decLon_gps) %>%
  # create column for extent (30 m for GPS coordinates):
  mutate(extent = 30) %>%
  # extract columns for group, name, year, collector,
  # location group, coordinates and extent:
  dplyr::select(
    det_grp, det_name, year, coll, loc_grp, lat, lon, extent
  )


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
  # exclude drift specimens:
  filter(is.na(drift)) %>%
  # extract columns for group, name, year, collector,
  # location group, site code, coordinates and extent:
  dplyr::select(
    det_grp, det_name, year, coll,
    site_code, loc_grp, lat, lon, extent
  )


# combine historical, recent & contemporary data:
dd_specimens <- bind_rows(
  use_dd_specimens_herb,
  use_dd_specimens_Mystikou,
  use_dd_specimens_DPLUS068
)




# specify year range (for extracting min and max):
year_range <- range(dd_specimens$year, na.rm = TRUE)
# determine year group breaks:
year_breaks <- c(year_range[1], seq(1850, 2000, 50), year_range[2])
# create new column in specimens data for year group:
dd_specimens <- dd_specimens %>%
  mutate(year_grp = cut(
    year, year_breaks, include.lowest = TRUE, dig.lab = 4
  ))

# extract year group levels:
year_grps <- levels(dd_specimens$year_grp)




# extract unique location groups:
loc_grps <- dd_specimens %>%
  # drop NAs, arrange alphabetically, convert to vector:
  distinct(loc_grp) %>% drop_na %>% arrange(loc_grp) %>% pull




dd_specimens <- dd_specimens %>%
  # create new factor column for site (combined lon and lat):
  # (NB -- round to 5 decimal places for neatness)
  mutate(site = paste(
    formatC(lon, 5, format = "f"), formatC(lat, 5, format = "f"),
    sep = ", "
  ))

# extract unique sites:
sites <- dd_specimens %>%
  # drop NAs, arrange alphabetically, convert to vector:
  distinct(site) %>% drop_na %>% arrange(site) %>% pull

# extract unique DPLUS068 site codes:
site_codes <- dd_specimens %>%
  # drop NAs, arrange alphabetically, convert to vector:
  distinct(site_code) %>% drop_na %>% arrange(site_code) %>% pull

# specify site codes to use in site-based analyses:
use_site_codes <- dd_sites %>%
  # subset DPLUS068 sites with **survey duration >= 30 min**:
  filter(dur_min >= 30) %>%
  # drop NAs, sort alphabetically, convert to vector:
  distinct(site_code) %>% drop_na %>% arrange(site_code) %>% pull %>%
  # return only values that are found in 'site_codes':
  intersect(site_codes)




# create vectors of all taxa and taxon groups
# (NB -- based on determined **name**), as a basis for analysing
# 'per taxon' and 'per group' distribution data:
# (drop NAs, arrange alphabetically, convert to vectors)
all_taxa <- dd_specimens %>%
  distinct(det_name) %>% drop_na %>% arrange(det_name) %>% pull
all_grps <- dd_specimens %>%
  distinct(det_grp) %>% drop_na %>% arrange(det_grp) %>% pull


all_grps_taxa <- map(all_grps, function (x) {  # for each taxon group,
  # create vector of associated taxa (determined **names**):
  dd_specimens %>%
    # subset specimen data for that taxon group:
    filter(det_grp == x) %>%
    # select taxa and convert to vector:
    dplyr::select(det_name) %>% pull %>%
    # convert to factor and relevel to place "sp. " at the end:
    fct_relevel(
      grep("sp. ", levels(factor(.)), value = TRUE),
      after = Inf  # move to end
    ) %>% levels
}) %>% set_names(all_grps)  # name list elements


# specify taxa and taxon groups to use in species-based analyses:
use_taxa <- c(
  # include all names with "sp. X" (e.g. "sp. 1", "sp. FIH"),
  # i.e. species determined via analysis of molecular data:
  grep("sp. [0-9|A-Z]", all_taxa, value = TRUE),
  grep("cf. ", all_taxa, value = TRUE),
  # Chlorophyta:
  "Bryopsis rosea", "Codium fragile",
  "Spongomorpha aeruginosa", "Ulva australis",
  "Ulva compressa", "Ulva intestinalis",
  "Ulva lactuca",
  # Rhodophyta:
  "Corallina chamberlaineae", "Amphiroa sp. indet.",
  "Hildenbrandia lecannelieri", "Camontagnea oxyclada",
  "Delisea pulchra", "Ptilonia magellanica",
  "Falklandiella harveyi",
  grep("Schizoseris", all_taxa, value = TRUE),
  "Hymenena falklandica", "Hymenena laciniata",
  "Cladodonta lyallii", "Phycodrys quercifolia",
  "Paraglossum epiglossum", "Polysiphonia paniculata",
  "Polysiphonia stricta", "Melanothamnus harveyi",
  "Streblocladia camptoclada", "Griffithsia antarctica",
  "Acanthococcus antarcticus", "Cystoclonium obtusangulum",
  "Mazzaella laminarioides", "Trematocarpus antarcticus",
  "Hymenocladia divaricata", "Semnocarpa corynephora",
  # Ochrophyta:
  "Lithoderma antarcticum", "Syringoderma australe",
  grep("Desmarestia [^'sp. ']", all_taxa, value = TRUE),
  "Geminocarpus geminatus", "Pylaiella littoralis",
  "Caepidium antarcticum", "Chordariopsis capensis",
  "Utriculidium durvelli", "Actinema subtilissimum",
  grep("Chordaria", all_taxa, value = TRUE),
  "Cladothele decaisnei", "Corycus lanceolatus",
  "Myriotrichia clavaeformis", "Tinocladia falklandica",
  "Durvillaea antarctica", "Turbinaria sp. indet.",
  "Macrocystis pyrifera", "Carpomitra costata"
) %>% sort  # sort alphabetically
# use_taxa %in% all_taxa  ### check taxa names are valid


use_grps <- c(  # (NB -- in **'taxonomic' order**)
  # Chlorophyta:
  "Prasiola",
  # Rhodophyta:
  "Ahnfeltia", "Ballia", "Nothogenia", "Heterosiphonia",
  "Bostrychia", "Catenella", "Callophyllis", "Plocamium",
  "Rhodymeniophycidae",
  # Ochrophyta:
  "Dictyotaceae", "Cladostephus", "Halopteris", "Adenocystis",
  "Dictyosiphon", "Punctaria", "Colpomenia", "Petalonia",
  "Scytosiphon", "Lessonia", "Splachnidiaceae"
)
# use_grps %in% all_grps  ### check group names are valid




# specify limit of georeferencing 'extent' (in m),
# below which points can be assigned to a 'location':
ext_lim <- 50000  # 50 km
# (i.e. excludes 'Falkland Islands', 'E Falkland', 'W Falkland')

# specify coordinates for Stanley (see georeferencing protocol):
stanley_coords <- t(matrix(c(-57.85954, -51.69458)))  # x, y


# create table of all unique coordinates with suitable extent:
all_coords <- dd_specimens %>%
  # extract all unique lat-long combinations:
  distinct_at(vars(lon, lat), .keep_all = TRUE) %>%
  # exclude sites with **extent > 50 km**:
  filter(extent <= ext_lim) %>% 
  # calculate distance to Stanley (in m):
  mutate(dist_stanley = pmap_dbl(  # pmap() instead of apply()
    list(lon, lat),  # list of vectors to pass to function,
    # using 'c(...)' to convert elements into vectors:
    ~ 1000 * spDists(t(matrix(c(...))), stanley_coords, longlat = TRUE)
  )) %>%
  # select coordinate, extent and distance columns only:
  dplyr::select(lon, lat, extent, dist_stanley)


# extract all coordinates as spatial points:
plot_coords <- all_coords %>%
  # select coordinates columns only:
  dplyr::select(lon, lat) %>%
  # reproject to current projection:
  SpatialPoints(CRS("+init=epsg:4326")) %>%
  spTransform(CRS(my_proj))




# subset coordinates 'near to' and 'far from' Stanley,
nf_dist <- 5000  # based on cutoff distance of **5 km** (in  m)
near_coords <- all_coords %>%  # near coordinates
  filter(dist_stanley <= nf_dist) %>% dplyr::select(lon, lat)
far_coords <- all_coords %>%  # far coordinates
  filter(dist_stanley > nf_dist) %>% dplyr::select(lon, lat)

# determine x and y limits for Stanley (in current projection),
# based on cutoff distance for 'near to' and 'far from':
stanley_limits <- stanley_coords %>%
  # convert into SpatialPoints and reproject:
  SpatialPoints(CRS("+init=epsg:4326")) %>%
  spTransform(CRS(my_proj)) %>%
  coordinates %>%  # extract coordinates
  # calculate xlim and ylim, as +/- cutoff distance:
  map(~ c(.x - nf_dist, .x + nf_dist))


# separately for coordinates 'near to' and 'far from' Stanley,
# ~ calculate maximum nearest neighbour distance (in m):
max_nndist_near <- near_coords %>% nndists %>% max(na.rm = TRUE)
max_nndist_far <- far_coords %>% nndists %>% max(na.rm = TRUE)

# ~ calculate square root of mean Voronoi polygon area (in m):
mean_vpdist_near <- near_coords %>%
  vparea(my_proj, shp_flk) %>%  mean(na.rm = TRUE) %>% sqrt
mean_vpdist_far <- far_coords %>%
  vparea(my_proj, shp_flk) %>% mean(na.rm = TRUE) %>% sqrt




# Manipulate map data ===============================================

# simplify flk shapefile (for plotting):
shp_flk_simple <- ms_simplify(shp_flk, keep = 0.1)




# convert polygons into lines:
flk_coast <- as(shp_flk_simple, "SpatialLinesDataFrame")  # shp_flk?




# ~ Rasterise coastline vector --------------------------------------

# specify desired grid resolution (in m):
# (NB -- mean Voronoi polygon distance for points 'far from' Stanley,
# rounded up to nearest x 10^3 m, where x = 2^n, where n is an integer;
# i.e. may be reached by doubling 2km*2km grid for IUCN area of occurence)
grid_res <- (2 ^ ceiling(log2( mean_vpdist_far/1000 ))) * 1000
# round(x, digits = -3)  # simple rounding to nearest 1000

# create grid template for rasterising vector:
grid_template <- raster(
  extent(flk_coast), resolution = grid_res, crs = my_proj
)

# extend grid template to cover vector extent:
grid_template <- grid_extent(grid_template, flk_coast)

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
