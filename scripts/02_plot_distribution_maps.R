# flk_spat_temp
# plot distribution maps

# Base map ----------------------------------------------------------

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




# Taxon maps --------------------------------------------------------

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
    rasterToPolygons(taxa_rasters[[i]]), add = TRUE,
    border = NA, col = adjustcolor("royalblue", alpha = 0.8)
  )
  
  # add taxon name in top right corner:
  legend(
    "topright", legend = i, bty = "n", cex = 1
  )
  
}


# close .pdf plotting device:
dev.off()
