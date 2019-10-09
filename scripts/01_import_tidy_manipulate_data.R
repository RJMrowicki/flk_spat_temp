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
  # location group, coordinates and extent:
  dplyr::select(
    det_grp, det_name, year, coll, loc_grp, lat, lon, extent
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
  # arrange alphabetically, drop NAs, convert to vector:
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
  distinct(site) %>% arrange(site) %>% pull




# create vectors of all taxa and taxon groups
# (NB -- based on determined **name**), as a basis for analysing
# 'per taxon' and 'per group' distribution data:
# (arrange alphabetically, drop NAs, convert to vectors)
all_taxa <- dd_specimens %>%
  distinct(det_name) %>% drop_na %>% arrange(det_name) %>% pull
all_grps <- dd_specimens %>%
  distinct(det_grp) %>% drop_na %>% arrange(det_grp) %>% pull

# specify taxon groups to use in subsequent analyses:
# (NB -- in 'taxonomic' order)
use_grps <- c(
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




# Species-based analysis ============================================

# ~ Obtain spatial points for taxa by year group --------------------

# create empty list for storing site coordinates per taxon/year group:
# ~ create blank list for storing data per year group:
a <- vector("list", length(year_grps))
names(a) <- year_grps  # name list entries according to year group
# ~ create list of year group lists with one entry per taxon:
taxa_coords <- rep(list(a), length(all_taxa))
names(taxa_coords) <- all_taxa  # name list entries according to taxa


# i <- all_taxa[1]  ### test
# j <- year_grps[1]  ### test

for (i in all_taxa) {  # for each taxon,
  # subset specimen data for this taxon:
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




# ~ Calculate extent of occurrence and area of occupancy ------------

taxa_eoo <- taxa_coords %>%
  # calculate **extent** of occurence (in km^2),
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




taxa_aoo_raw <- taxa_rasters %>%
  # calculate raw **area** of occupancy (in km^2) at current resolution,
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    # no. of occupied cells * cell area (in km):
    cellStats(., sum) * prod(res(.)/10^3)
  })




# extract maximum extent of coastline vector (in m),
max_ext <- max(  # as maximum of x extent vs. y extent:
  xmax(flk_coast) - xmin(flk_coast),
  ymax(flk_coast) - ymin(flk_coast)
)
# grid cell size (in m) corresponding to maximum extent,
# rounded to nearest x 10^3 m, where x = 2^n, where n is an integer
# (i.e. may be reached by doubling 2km*2km grid for IUCN aoo):
max_res <- (2 ^ round(log2( max_ext/10^3 ))) * 10^3

# vector of grid cell sizes (in m),
# from current to maximum, in increments of 2^n:
xres <- 2 ^ (log2(grid_res/10^3):log2(max_res/10^3)) * 10^3


taxa_aoo <- taxa_coords %>%
  # calculate **area** of occupancy (in km^2) at 2km*2km resolution,
  # predicted from log-linear model of aoo vs. grid cell size,
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    
    aoo <- map_dbl(  # result as numerical vector, not list
      xres, function (x) {  # for each grid cell size,
        # create grid template for rasterising vector:
        grid_x <- raster(
          extent(flk_coast), resolution = x, crs = my_proj
        )
        # increase grid extent to match vector extent:
        grid_x <- grid_extent(grid_x, flk_coast)
        # rasterise points vector based on grid template:
        spr <- rasterize(., grid_x, field = 1, crs = my_proj)
        # calculate area of occupancy (no. cells * cell area):
        aoo <- cellStats(spr, sum) * prod(res(spr)/10^3)
        
        return(aoo)  # output area of occupancy
      })
    
    # predict aoo for grid cell size of 2000 m,
    # based on log-linear model of aoo vs. grid cell size:
    pred_aoo <- 10 ^ predict(  # (NB -- reverse log10 transform)
      lm(log10(aoo) ~ log10(xres)),
      data.frame(xres = 2000)
    )
    
    return(pred_aoo)  # output predicted area of occupancy
  })




# ~ Summarise no. of records by year group vs. location -------------

# x <- taxa[172] ### test

taxa_st <- map(all_taxa, function (x) {  # for each taxon,
  # create table of number of records per year group vs. location:
  dd_specimens %>%
    # remove rows for which loc_grp is NA:
    filter(!is.na(loc_grp)) %>%
    # convert loc_grp from character to factor and relevel,
    # so that "Unlocated" levels are placed at the end:
    mutate(loc_grp = fct_relevel(
      loc_grp,  
      grep("Unlocated*", levels(factor(.$loc_grp)), value = TRUE),
      after = Inf  # move to end
    )) %>%
    # convert site from character to factor:
    mutate(site = factor(site)) %>%
    # subset specimen data for this taxon:
    filter(det_name == x) %>%
    # extract unique combinations of collector, **year** and **site**
    # (= collecting 'events' [= 'records']):
    distinct_at(vars(coll, year, site), .keep_all = TRUE) %>%
    # group by **location** and **year group** (NB -- keep factor levels):
    group_by(loc_grp, year_grp, .drop = FALSE) %>%
    # calculate number of rows per group:
    summarise(n = n()) %>%
    # widen into table of year group vs. location group:
    pivot_wider(  # (instead of gather())
      names_from = year_grp, values_from = n,
      values_fill = list(n = 0)
    ) %>%
    # convert location column to rownames:
    column_to_rownames("loc_grp") %>%
    # add column and row sums as new row and column, respectively:
    rbind(TOTAL = colSums(.)) %>% cbind(TOTAL = rowSums(.))
}) %>% set_names(all_taxa)  # name list elements




# Site-based analysis ===============================================

# ~ Obtain taxa list per site by year group -------------------------

# create empty list for storing taxa per site/year group:
# ~ create blank list for storing data per year group:
b <- vector("list", length(year_grps))
names(b) <- year_grps  # name list entries according to year group
# ~ create list of year group lists with one entry per site:
site_taxa <- rep(list(b), length(sites))
names(site_taxa) <- sites  # name list entries according to sites


# i <- sites[1]  ### test
# j <- year_grps[5]  ### test

for (i in sites) {  # for each site,
  # subset specimen data for this site:
  site_dat <- dd_specimens %>% filter(paste(lon, lat) == i)
  for (j in year_grps) {  # for each year group,
    # only if year group represented in site data:
    if (j %in% site_dat$year_grp) {
      # assign to corresponding list item:
      site_taxa[[i]][[j]] <- site_dat %>%
        # filter site data for this year group:
        filter(year_grp == j) %>%
        # extract unique taxa associated with specimens:
        distinct(det_name) %>% pull  # convert to vector
    }
  }
}




# ~ Summarise no. of taxa per site by year group --------------------

# site_rich <- site_taxa %>%
#   # calculate richness (total no. of taxa),
#   # for each year group within each site:
#   map_depth(2, ~ if (!is.null(.)) { length(.) })


site_rich <- dd_specimens %>%
  # convert site from character to factor:
  mutate(site = factor(site)) %>%
  # extract unique combinations of name, site and year group:
  distinct_at(vars(det_name, site, year_grp), .keep_all = TRUE) %>%
  # group by site and year group (NB -- keep factor levels):
  group_by(site, year_grp, .drop = FALSE) %>%
  # calculate number of rows per group:
  summarise(n = n()) %>%
  # replace 0 with NA:
  mutate(n = na_if(n, 0)) %>%
  # widen into table of year group vs. site:
  pivot_wider(  # (instead of gather())
    names_from = year_grp, values_from = n,
    values_fill = list(n = NA)
  ) %>%
  # ungroup and convert site from factor to character:
  ungroup %>% mutate(site = as.character(site)) %>%
  # add columns from original data frame:
  left_join(
    # (NB -- must remove duplicate site rows first)
    distinct_at(dd_specimens, vars(site), .keep_all = TRUE),
    by = "site") %>%
  # select coordinates, location and year group columns:
  dplyr::select(lon, lat, loc_grp, year_grps)




# # specify richness range:
# rich_range <- site_rich %>%
#   # (NB -- **most recent** year group only)
#   dplyr::select(year_grps[length(year_grps)]) %>% range(na.rm = TRUE)
# # determine richness group breaks manually:
# rich_breaks <- c(rich_range[1], seq(15, 70, 15), rich_range[2])

# determine richness group breaks based on quartiles:
rich_breaks <- site_rich %>%
  # (NB -- **most recent** year group only)
  dplyr::select(year_grps[length(year_grps)]) %>%
  as_vector %>% quantile(na.rm = TRUE, type = 3)
  # (NB -- use Type 3 to round to nearest sample value)


# create new table for 'richness group' instead of 'richness':
site_rich_grps <- site_rich %>%
  mutate_at(
    vars(year_grps),
    ~ cut(., rich_breaks, include.lowest = TRUE, na.rm = TRUE)
  )


# extract richness group levels:
rich_grps <- site_rich_grps %>%
  dplyr::select(year_grps) %>% combine %>% levels




# convert coordinats to spatial points for plotting:
plot_coords_rich <- site_rich %>%
  # select coordinates columns only:
  dplyr::select(lon, lat) %>%
  # reproject to current projection:
  SpatialPoints(CRS("+init=epsg:4326")) %>%
  spTransform(CRS(my_proj))
