# flk_spat_temp
# site-based analysis

# Site-based analysis ===============================================

# ~ Obtain taxa list per site by year -------------------------------

# ~~ All sites, by year group:

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
  site_dat <- dd_specimens %>%
    # subset specimen data for this site:
    filter(site == i)
  for (j in year_grps) {  # for each year group,
    # only if year group represented in site data:
    if (j %in% site_dat$year_grp) {
      # assign to corresponding list item:
      site_taxa[[i]][[j]] <- site_dat %>%
        # filter site data for this year group:
        filter(year_grp == j) %>%
        # extract unique taxa associated with specimens,
        # sort alphabetically and convert to vector:
        distinct(det_name) %>% arrange(det_name) %>% pull
    }
  }
}




# ~~ DPLUS068 sites only:

DPLUS068_site_taxa <- map(site_codes, function (x) {
  # for each DPLUS068 site,
  dd_specimens %>%
    # subset specimens data for that site:
    filter(site_code == x) %>%
    # extract unique taxa associated with specimens,
    # sort alphabetically and convert to vector:
    distinct(det_name) %>% arrange(det_name) %>% pull
}) %>% set_names(site_codes)




# ~ Summarise no. of taxa per site by year --------------------------

# ~~ All sites, by year group:

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
  # calculate quartiles (NB -- Type 3 rounds to nearest sample value)
  as_vector %>% quantile(na.rm = TRUE, type = 3)


# create new table for 'richness group' instead of 'richness':
site_rich_grps <- site_rich %>%
  mutate_at(
    vars(year_grps),
    ~ cut(., rich_breaks, include.lowest = TRUE, na.rm = TRUE)
  )




# ~~ DPLUS068 sites only:

# DPLUS068_site_rich <- DPLUS068_site_taxa %>%
#   # calculate richness (total no. of taxa) for each DPLUS068 site:
#   map(~ if (!is.null(.)) { length(.) })


DPLUS068_site_rich <- dd_specimens %>%
  # subset DPLUS068 sites (NB -- selected sites only):
  filter(site_code %in% use_site_codes) %>%
  # convert site code from character to factor:
  mutate(site_code = factor(site_code)) %>%
  # extract unique combinations of name and site code:
  distinct_at(vars(det_name, site_code), .keep_all = TRUE) %>%
  # group by site code (NB -- keep factor levels):
  group_by(site_code, .drop = FALSE) %>%
  # calculate number of rows per group:
  summarise(rich = n()) %>%
  # replace 0 with NA:
  mutate(rich = na_if(rich, 0)) %>%
  # ungroup and convert site code from factor to character:
  ungroup %>% mutate(site_code = as.character(site_code)) %>%
  # add columns from original data frame:
  left_join(
    # (NB -- must remove duplicate site code rows first)
    distinct_at(dd_specimens, vars(site_code), .keep_all = TRUE),
    by = "site_code") %>%
  # select coordinates, location, site code and richness columns:
  dplyr::select(lon, lat, loc_grp, site_code, rich)




# determine richness group breaks based on quartiles:
DPLUS068_rich_breaks <- DPLUS068_site_rich %>%
  # select richness column:
  dplyr::select(rich) %>%
  # calculate quartiles (NB -- Type 3 rounds to nearest sample value)
  as_vector %>% quantile(na.rm = TRUE, type = 3)


# create new table for 'richness group' instead of 'richness':
DPLUS068_site_rich_grps <- DPLUS068_site_rich %>%
  mutate(rich = cut(
    rich, DPLUS068_rich_breaks, include.lowest = TRUE, na.rm = TRUE
  ))




# # extract richness group levels:
# rich_grps <- site_rich_grps %>%
#   dplyr::select(year_grps) %>% combine %>% levels

# extract richness group levels:
rich_grps <- levels(DPLUS068_site_rich_grps$rich)




# convert coordinates to spatial points for plotting:
plot_coords_rich <- DPLUS068_site_rich %>%
  # select coordinates columns only:
  dplyr::select(lon, lat) %>%
  # reproject to current projection:
  SpatialPoints(CRS("+init=epsg:4326")) %>%
  spTransform(CRS(my_proj))
