# flk_spat_temp
# functions

# calculate nearest neighbour distances for set of coordinates:
# (requires sp::spDists() and tibble::as_tibble())
nndists <- function (coords)
{
  # calculate all pairwise distances (in m):
  dists <- 1000 * spDists(as.matrix(coords), longlat = TRUE)
  dists[dists == 0] <- NA  # replace 0s with NAs
  # assign numerical row/column names (avoid as_tibble warning):
  rownames(dists) <- colnames(dists) <- 1:nrow(dists)
  dists <- as_tibble(dists)  # convert to tibble
  
  # calculate 'nearest neighbour' distance for each point
  # (i.e. minimum value in each column of distance matrix):
  nndists <- dists %>% map_dbl(~ min(., na.rm = TRUE))
  
  return(nndists)  # output result
}




# make custom ggplot2 theme:
# (requires ggplot2::theme())
theme_rob <- function (axis_col = "black")
{ 
  theme_bw() %+replace% 
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 1, 1, 1),
      axis.line = element_line(colour = axis_col, size = 0.25),
      axis.ticks = element_line(colour = axis_col, size = 0.25),
      axis.text = element_text(colour = axis_col, size = 8),
      axis.title = element_text(colour = axis_col, size = 8),
      strip.background = element_blank(),
      strip.text = element_text(size = 11),
      strip.placement = "outside",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0, "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)
    )
}
