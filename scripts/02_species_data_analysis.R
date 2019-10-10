# flk_spat_temp
# species-based analysis

# Species-based analysis ============================================

# ~ Obtain coordinates for taxa and taxon groups by year group ------

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




# create empty list for storing site coordinates per taxon/year group:
# ~ create list of year group lists with one entry per taxon:
grps_coords <- rep(list(a), length(all_grps))
names(grps_coords) <- all_grps  # name list entries according to taxa


# i <- all_grps[1]  ### test
# j <- year_grps[1]  ### test

for (i in all_grps) {  # for each taxon group,
  # subset specimen data for this group:
  grp_dat <- dd_specimens %>% filter(det_grp == i)
  for (j in year_grps) {  # for each year group,
    # only if year group represented in group data:
    if (j %in% grp_dat$year_grp) {
      
      grp_coords_dat <- grp_dat %>%
        # filter group data for this year group:
        filter(year_grp == j) %>%
        # extract unique locations associated with specimens:
        # (NB -- **x before y**; keep all columns)
        distinct_at(vars(lon, lat), .keep_all = TRUE) %>%
        # select coordinates and name columns:
        dplyr::select(lon, lat, det_name)
      
      # convert to SpatialPointsDataFrame object,
      # including both coordinates and associated taxon names:
      grp_coords <- SpatialPointsDataFrame(
        coords = SpatialPoints(
          dplyr::select(grp_coords_dat, lon, lat),
          CRS("+init=epsg:4326")),  # (NB -- **WGS84 projection**)
        data = dplyr::select(grp_coords_dat, det_name)
      )
      
      # assign to corresponding list item (NB -- **reproject**)
      grps_coords[[i]][[j]] <- spTransform(grp_coords, CRS(my_proj))
    }
  }
}




grps_rasters <- grps_coords %>%
  # rasterise point coordinates based on grid template
  # for each year group within each taxon group:
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
    } else { NA }
  } else { NA })  # else replace NULL with NA




taxa_aoo_raw <- taxa_rasters %>%
  # calculate raw **area** of occupancy (in km^2) at current resolution,
  # for each year group within each taxon:
  map_depth(2, ~ if (!is.null(.)) {  # only if coordinates not NULL
    # no. of occupied cells * cell area (in km):
    cellStats(., sum) * prod(res(.)/10^3)
  } else { NA })  # else replace NULL with NA




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
  } else { NA })  # else replace NULL with NA




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




# ~ Summarise EOO, AOO and no. of locations -------------------------

# create summary table of EOO, AOO and no. of locations for taxa:
# (NB -- **selected taxa** and **most recent year group** only)
taxa_eoo_aoo_locs <- tibble(
  taxon = use_taxa,  # selected taxa names
  eoo_km2 = map_dbl(  # result as numerical vector
    taxa_eoo[use_taxa],  # selected taxa only
    # extract value for the most recent year group:
    function (x) { x[[last(year_grps)]] }),
  aoo_km2 = map_dbl(
    taxa_aoo[use_taxa],
    function (x) { x[[last(year_grps)]] }),
  n_locs = map_dbl(
    taxa_st[use_taxa], function (x) {
      # calculate no. of locations via `sum(x != 0)`:
      sum(x[loc_grps, last(year_grps)] != 0)
    })
)

# output to .csv file:
write_csv(taxa_eoo_aoo_locs, "./out/taxa_eoo_aoo_locs.csv")
