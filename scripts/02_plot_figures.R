# flk_spat_temp
# plot figures

mar_map <- c(0, 0, 0, 0)  # specify map margins




# Summary histogram =================================================

# open .pdf plotting device:
pdf(
  "./figures/histogram.pdf",
  width = 10/2.54, height = 7.5/2.54
)


# plot histogram of no. of specimens vs. year:
ggplot(dd_specimens, aes(x = year)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  scale_x_continuous(  # x axis
    limits = c(1820, 2020), expand = c(0, 0),
    breaks = seq(1800, 2050, 50), name = "Year") +
  scale_y_continuous(  # y axis
    limits = c(0, 1750), expand = c(0, 0),
    breaks = seq(0, 2000, 500), name = "No. of specimens") +
  theme_rob()  # apply custom theme


# close .pdf plotting device
dev.off()




# Coordinates map ===================================================

# open .pdf plotting device:
pdf(
  "./figures/coords_map.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = mar_map) # outer margins

# ~ all coordinates:

# plot simplified flk polygon shapefile:
plot(shp_flk_simple, border = grey(0.5), lwd = 1)

# add circles representing extent for all coordinates:
symbols(
  coordinates(plot_coords),
  circles = all_coords$extent, inches = FALSE,
  add = TRUE, fg = NULL,
  bg = adjustcolor("royalblue", alpha = 0.1)
)

# add points for all coordinates:
points(
  plot_coords, pch = 21,
  col = "white", bg = adjustcolor("royalblue", alpha = 0.8)
)

# ~ coordinates 'near to' Stanley:

# plot (unsimplified) flk polygon shapefile:
plot(
  shp_flk,
  xlim = stanley_limits[[1]], ylim = stanley_limits[[2]],
  col = grey(0.75, alpha = 0.75), border = grey(0.5), lwd = 1
)

# add circles representing extent for all coordinates:
symbols(
  coordinates(plot_coords),
  circles = all_coords$extent, inches = FALSE,
  add = TRUE, fg = NULL,
  bg = adjustcolor("royalblue", alpha = 0.1)
)

# add points for all coordinates:
points(
  plot_coords, pch = 21,
  col = "white", bg = adjustcolor("royalblue", alpha = 0.8)
)


dev.off()  # close .pdf plotting device




# Base map ==========================================================

# open .pdf plotting device:
pdf(
  "./figures/base_map.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = mar_map) # outer margins

plot(  # plot raster cell underlay
  rasterToPolygons(flk_coast_raster),  # as polygons
  border = grey(0.75), lwd = 1.5
)

plot(  # add simplified flk shapefile (polygons)
  shp_flk_simple,  # add flk coast
  lwd = 0.5, add = TRUE
)

points(  # add points for all coordinates
  plot_coords, pch = 21,
  col = "white", bg = adjustcolor("royalblue", alpha = 0.8)
)


# close .pdf plotting device:
dev.off()




# Taxon maps ========================================================

# open .pdf plotting device:
pdf(
  "./figures/taxon_maps.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = mar_map)  # outer margins

# create plots:
for (i in use_taxa) {  # for each taxon,
  
  # extract coordinates and raster for this taxon:
  # (NB -- **most recent** year group only)
  taxon_coords <- taxa_coords[[i]][[year_grps[length(year_grps)]]]
  taxon_raster <- taxa_rasters[[i]][[year_grps[length(year_grps)]]]
  
  # only if coordinates/raster are not NULL :
  if (!is.null(taxon_coords)) {
    
    plot(  # plot raster cell underlay (as polygons)
      rasterToPolygons(flk_coast_raster),
      border = grey(0.75), lwd = 1
    )
    
    plot(  # add simplified flk polygon shapefile
      shp_flk_simple, add = TRUE,
      lwd = 0.5
    )
    
    plot(  # add taxon raster squares
      rasterToPolygons(taxon_raster), add = TRUE,
      border = NA, col = adjustcolor("royalblue", alpha = 0.8)
    )
    
    points(  # add taxon coordinates points
      taxon_coords, pch = 21,
      col = "white", bg = adjustcolor("royalblue", alpha = 0.8)
    )
    
    legend(  # add taxon name in top right corner
      "topright", legend = i, bty = "n", cex = 1
    )
    
  }
}


# close .pdf plotting device:
dev.off()




# Taxon group maps ==================================================

# create custom colour palette for taxon groups:
grps_taxa_colours <- colorRampPalette(
  c("royalblue", "red"),
  alpha = TRUE, bias = 1) # alter spread of colours




# open .pdf plotting device:
pdf(
  "./figures/taxon_group_maps.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = mar_map)  # outer margins


# i <- all_grps[2]  ### test

# create plots:
for (i in all_grps) {  # for each taxon group,
  
  # extract coordinates/raster for this group:
  # (NB -- **most recent** year group only)
  grp_coords <- grps_coords[[i]][[year_grps[length(year_grps)]]]
  grp_raster <- grps_rasters[[i]][[year_grps[length(year_grps)]]]
  
  # only if coordinates/raster are not NULL :
  if (!is.null(grp_raster)) {
    
    # create vector of taxa within this group:
    grp_taxa <- grp_coords$det_name %>%
      # convert to factor and relevel to place "sp. " at the end:
      fct_relevel(
        grep("sp. ", levels(factor(.)), value = TRUE),
        after = Inf  # move to end
      ) %>% levels
    
    # create point style table for taxon lookup:
    pt_sty_taxa <- tibble(
      taxon = grp_taxa,
      col = grps_taxa_colours(length(grp_taxa))
    )
    
    plot(  # plot raster cell underlay (as polygons)
      rasterToPolygons(flk_coast_raster),
      border = grey(0.75), lwd = 1
    )
    
    plot(  # add simplified flk polygon shapefile
      shp_flk_simple, add = TRUE,
      lwd = 0.5
    )
    
    plot(  # add taxon group raster squares
      rasterToPolygons(grp_raster), add = TRUE,
      border = NA, col = grey(0.5, alpha = 0.5)
    )
    
    # # ~ plot standard points (overlapping) for taxa:
    # for (j in grp_taxa) {  # for each taxon,
    # 
    #   # extract taxon coordinates:
    #   # (NB -- **most recent** year group only)
    #   taxon_coords <- taxa_coords[[j]][[year_grps[length(year_grps)]]]
    # 
    #   # only if coordinates/rasters are not NULL :
    #   if (!is.null(taxon_coords)) {
    # 
    #     points(  # add taxon coordinates points
    #       taxon_coords,
    #       pch = 21,
    #       col = "white",
    #       bg = pt_sty_taxa$col[pt_sty_taxa$taxon == j],
    #       cex = 1.5
    #     )
    # 
    #   }
    # }

    # ~ plot non-overlapping 'points' for group as a whole:
    pt_radius <- 2000  # specify 'point' radius (in m)
    # determine non-overlapping layout, based on 'point' radius:
    lyt <- circleRepelLayout(
      cbind(coordinates(grp_coords), pt_radius),
      sizetype = "radius")$layout
    
    # add taxa points as non-overlapping circles:
    symbols(
      lyt$x, lyt$y, lyt$radius, inches = FALSE, add = TRUE,
      fg = "white",
      # colour according to taxon:
      bg = pt_sty_taxa$col[match(
        grp_coords$det_name, pt_sty_taxa$taxon
      )]
    )

    legend(  # add legend for taxon
      "bottomright", bty = "n", legend = pt_sty_taxa$taxon,
      pch = 21, col = "white", pt.bg = pt_sty_taxa$col
    )
    
    legend(  # add taxon group name in top right corner
      "topright", legend = i, bty = "n", cex = 1
    )
    
  }
}


# close .pdf plotting device:
dev.off()




# Richness map ======================================================

# create custom colour palette for richness groups:
rich_grp_colours <- colorRampPalette(
  c("royalblue", "red"),
  alpha = TRUE, bias = 1) # alter spread of colours

# create point style lookup table for richness group:
pt_sty_rich <- tibble(
  rich_grp = rich_grps,
  pch = 15:18,
  # col = c("blue", "green", "yellow", "red")
  col = rich_grp_colours(length(rich_grps))
)




# open .pdf plotting device:
pdf(
  "./figures/rich_map.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = mar_map)  # outer margins



plot(  # plot simplified flk shapefile (polygons)
  shp_flk_simple, lwd = 0.5
)

# specify richness variable (/year group) to plot:
plot_var <- "rich"

pt_radius <- 1500  # specify 'point' radius (in m)
# determine non-overlapping layout, based on 'point' radius:
lyt <- circleRepelLayout(
  cbind(coordinates(plot_coords_rich), pt_radius),
  sizetype = "radius")$layout

points(  # add points
  # plot_coords_rich,  # standard points (overlapping)
  lyt$x, lyt$y,  # non-overlapping points
  # symbol and colour according to richness group:
  pch = pt_sty_rich$pch[
    match(
      # (NB -- convert tibble column to vector):
      pull(DPLUS068_site_rich_grps[, plot_var]),
      pt_sty_rich$rich_grp
    )],
  col = pt_sty_rich$col[
    match(
      pull(DPLUS068_site_rich_grps[, plot_var]),
      pt_sty_rich$rich_grp
    )],
  cex = 1.5
)

legend(  # add legend for richness group
  "topright", bty = "n", title = "Richness",
  legend = pt_sty_rich$rich_grp,
  pch = pt_sty_rich$pch,
  col = pt_sty_rich$col
)


# close .pdf plotting device:
dev.off()
