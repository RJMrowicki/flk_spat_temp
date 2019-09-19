# flk_spat_temp
# initialisation

rm(list = ls())  # clear R's memory
orig_wd <- getwd()  # store working directory

# set environmental variables and constants:

Sys.setenv(TZ = "UTC")  # set timezone (avoid `as.POSIX*` warnings)
set.seed(123456)  # set seed for random number generation

nas <- c("", "NA", "#N/A")  # missing value strings for read_csv




# Packages ==========================================================

# load required packages:
suppressPackageStartupMessages(library(tidyverse))
library(lubridate)  # ymd_hms(), year(), etc.

library(raster)  # raster(), plot.raster()
library(rgdal)  # readOGS(), spTransform()

library(spatial.tools)  # modify_raster_margins()
library(rmapshaper)  # ms_simplify()




# Functions =========================================================

# load functions:
source('./scripts/functions.R')
