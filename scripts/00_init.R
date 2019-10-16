# flk_spat_temp
# initialisation

rm(list = ls())  # clear R's memory
orig_wd <- getwd()  # store working directory

# set environmental variables and constants:
Sys.setenv(TZ = "UTC")  # set timezone (avoid `as.POSIX*` warnings)
set.seed(123456)  # set seed for random number generation
nas <- c("", "NA", "#N/A")  # missing value strings for read_csv

# specify default projection:
my_proj <-
  "+init=epsg:32721"  # UTM zone 21S
# "+proj=utm +zone=21 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# (NB -- units in m for rasterisation below)
# WGS84: "+init=epsg:4326"
# Robinson: "+proj=robin"
# Winkel Tripel: "+proj=wintri"




# Packages ==========================================================

# load required packages:
suppressPackageStartupMessages(library(tidyverse))
library(lubridate)  # ymd_hms(), year(), etc.

library(alphahull)  # ashape(), areahull()
library(igraph)  # graph.edgelist()
library(packcircles)  # circleRepelLayout()
library(raster)  # raster(), plot.raster()
library(rgdal)  # readOGS(), spTransform()
library(rmapshaper)  # ms_simplify()
library(SDraw)  # voronoi.polygons()
library(spatial.tools)  # modify_raster_margins()

library(knitr)  # kable()
library(openxlsx)  # output to .xlsx

library(devtools)  # session_info()




# Functions =========================================================

# load functions:
source('./scripts/functions.R')
