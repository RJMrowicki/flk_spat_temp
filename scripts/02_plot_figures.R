# flk_spat_temp
# plot figures

# Summary histogram =================================================

# open .pdf plotting device:
pdf(
  "./figures/histogram.pdf",
  width = 10/2.54, height = 7.5/2.54
)


# plot histogram of no. of specimens vs. year:
ggplot(dd_specimens, aes(x = year)) +
  # 
  geom_histogram(binwidth = 10, boundary = 0) +
  scale_x_continuous(
    limits = c(1820, 2020), expand = c(0, 0),
    breaks = seq(1800, 2050, 50), name = "Year") +
  scale_y_continuous(
    limits = c(0, 1750), expand = c(0, 0),
    breaks = seq(0, 2000, 500), name = "No. of specimens") +
  theme_rob()  # apply custom theme


# close .pdf plotting device
dev.off()




# Base map ==========================================================

# open .pdf plotting device:
pdf(
  "./figures/base_map.pdf",
  # adjust height according to raster aspect ratio:
  width = 18/2.54, height = (18/asp)/2.54
)


# set plotting parameters:
par(mar = rep(0.1, 4))  # narrow outer margins

plot(  # plot raster cell underlay
  rasterToPolygons(flk_coast_raster),  # as polygons
  border = grey(0.75), lwd = 1.5
)

plot(  # add simplified flk shapefile (polygons)
  shp_flk_simple,  # add flk coast
  lwd = 0.5, add = TRUE
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
par(mar = rep(0.1, 4))  # narrow outer margins

# create plots:
for (i in taxa) {  # for each taxon,
  # only if coordinates are not NULL (NB -- **most recent** year group):
  if (!is.null(taxa_rasters[[i]][[year_grps[length(year_grps)]]])) {
    
    # plot raster cell underlay (as polygons):
    plot(
      rasterToPolygons(flk_coast_raster),
      border = grey(0.75), lwd = 1
    )
    
    # add simplified flk polygon shapefile:
    plot(
      shp_flk_simple, add = TRUE,
      lwd = 0.5
    )
    
    # add taxon raster squares:
    plot(
      # (NB -- **most recent** year group only)
      rasterToPolygons(taxa_rasters[[i]][[year_grps[length(year_grps)]]]),
      add = TRUE, border = NA, col = adjustcolor("royalblue", alpha = 0.8)
    )
    
    # add taxon name in top right corner:
    legend(
      "topright", legend = i, bty = "n", cex = 1
    )
    
  }
}


# close .pdf plotting device:
dev.off()
