# flk_spat_temp
# functions

# convert alphahull::ashape into sp::SpatialPolygon object:
# (see <https://babichmorrowc.github.io/post/2019-03-18-alpha-hull/>)
# (requires igraph package)
ashape_poly <- function (ashp, use_proj)
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
  
  return(ashp_spoly)  # output polygons
}




# create convex hull from points as SpatialPolygons object:
# (requires sp::Polygon(), Polygons(), SpatialPolygons())
chull_poly <- function (coords, use_proj)
{
  ch <- chull(coords)  # calculate convex hull nodes
  # extract path coordinates and convert to polygon:
  ch_poly <- Polygon(coords[c(ch, ch[1]), ])
  # convert polygon into SpatialPolygons object:
  ch_spoly <- SpatialPolygons(
    list(Polygons(list(ch_poly), 1)),  # via Polygons object
    proj4string = CRS(use_proj)  # specify projection
  )
  
  return(ch_spoly)  # output polygons
}




# match rasterisation grid template extent to vector extent:
# (requires raster package)
grid_extent <- function(grid_use, vector_use) {
  
  grid_out <- grid_use  # copy grid template
  
  # if template xmax is smaller than vector xmax:
  if (xmax(grid_out) < xmax(vector_use)) {
    # extend template extent by n columns (to right):
    ext_xn <- ceiling(  # difference expressed in n columns
      diff(c(xmax(grid_out), xmax(vector_use))) / xres(grid_out)
    )
    ext_x <- extent(grid_out)  # copy template extent
    # increase xmax by n columns * grid resolution:
    xmax(ext_x) <- xmax(grid_out) + (ext_xn * xres(grid_out))
    # update template x extent (and dimensions):
    grid_out <- extend(grid_out, ext_x)
    # raster::extend() extends by n columns on *both* sides;
    # raster::modify_raster_margins() does not update extent
  }
  
  # and if template ymin is greater than vector ymin:
  if (ymin(grid_out) > ymin(vector_use)) {
    # extend template extent by n rows (to bottom):
    ext_yn <- ceiling(  # difference expressed in n rows:
      diff(c(ymin(vector_use), ymin(grid_out))) / xres(grid_out)
    )
    ext_y <- extent(grid_out)  # copy template extent
    # decrease ymin by n rows * grid resolution:
    ymin(ext_y) <- ymin(grid_out) - (ext_yn * xres(grid_out))
    # update template y extent (and dimensions):
    grid_out <- extend(grid_out, ext_y)
  }
  
  return(grid_out)  # return grid template
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
  
  return(nndists)  # output nearest neighbour distances
}




# calculate Voronoi polygon areas for set of coordinates:
# (requires SDraw::voronoi.polygons())
vparea <- function (coords_dat, use_proj, bbox_shp)
{
  coords <- coords_dat %>%
    dplyr::select(lon, lat) %>%  # extract coordinates columns
    # convert into SpatialPoints and reproject:
    SpatialPoints(CRS("+init=epsg:4326")) %>%  # (NB -- **WGS84**)
    spTransform(CRS(use_proj))
  
  # specify bounding box as SpatialPolygon, using bbox
  # extracted from specified shapefile, with same projection:
  bbpoly <- bbox_to_SpatialPolygons(bbox(bbox_shp), CRS(my_proj))
  
  # create Voronoi polygons clipped to bbox, as SpatialPolygons:
  vorpolys <- SDraw::voronoi.polygons(coords, bbpoly)
  
  return(area(vorpolys))  # output polygon areas
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
