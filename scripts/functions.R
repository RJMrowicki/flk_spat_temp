# flk_spat_temp
# functions

# convert alphahull::ashape into sp::SpatialPolygon object:
# (see <https://babichmorrowc.github.io/post/2019-03-18-alpha-hull/>)
# (requires igraph package)
ashape_poly <- function(ashp, use_proj)
{
  # create ashape edge graph:
  ashp_graph <- ashp$edges %>% as_tibble %>%
    # select ashape edge index columns and convert to character:
    dplyr::select(ind1, ind2) %>% mutate_all(as.character) %>%
    # convert to matrix and create graph (not plotted):
    as.matrix %>% graph_from_edgelist(directed = FALSE)
  
  # cut graph (remove single edge) to create chain:
  cut_graph <- ashp_graph - E(ashp_graph)[1]
  
  # extract chain ends (corresponding nodes for which degree = 1):
  ends <- names(which(degree(cut_graph) == 1))
  # extract ordered vector of nodes in path:
  path_ind <- shortest_paths(cut_graph, ends[1], ends[2])$vpath[[1]]  # index
  path <- as.numeric(V(ashp_graph)[path_ind]$name)  # corresponding nodes
  path_full <- c(path, path[1])  # join ends
  
  # extract path coordinates and convert to polgyon:
  ashp_poly <- Polygon(ashp$x[path_full, ])
  # convert polygon into SpatialPolygons object:
  ashp_spoly <- SpatialPolygons(
    list(Polygons(list(ashp_poly), 1)),  # via Polygons object
    proj4string = CRS(use_proj)  # specify projection
  )
  
  # output SpatialPolygons object:
  return(ashp_spoly)
}




# create convex hull from points as SpatialPolygons object:
# (requires sp::Polygon, Polygons, SpatialPolygons)
chull_poly <- function(coords, use_proj)
{
  ch <- chull(coords)  # calculate convex hull nodes
  # extract path coordinates and convert to polygon:
  ch_poly <- Polygon(coords[c(ch, ch[1]), ])
  # convert polygon into SpatialPolygons object:
  ch_spoly <- SpatialPolygons(
    list(Polygons(list(ch_poly), 1)),  # via Polygons object
    proj4string = CRS(use_proj)  # specify projection
  )
  
  # output SpatialPolygons object:
  return(ch_spoly)
}




# calculate nearest neighbour distances for set of coordinates:
# (requires sp::spDists() and tibble::as_tibble())
nndists <- function (coords_dat)
{
  # extract lon and lat columns from input data:
  coords <- coords_dat %>% dplyr::select(lon, lat)
  
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
