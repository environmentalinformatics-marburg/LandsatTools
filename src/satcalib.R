# Crop, calibrate and illumination correct landsat or hyperion data sets.
#
#  Copyright (C) 2014 Thomas Nauss
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de
rm(list = ls(all = T))


#### General setttings #########################################################
sensor <- "landsat"
input.path <- "active/bis-fogo/data/remote-sensing/landsat/LC82100502014328LGN00"
output.path <- "active/bis-fogo/analysis/vulcano_2014"

src.filepath <- "active/bis-fogo/software/vulcano_2014/src"

crop.filepath <- "active/bis-fogo/analysis/vulcano_2014/crop_template.tif"

rad.unit <- "rad"

dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")


#### Set working directory and load libraries ##################################
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(rgdal)
library(raster)
for (i in c("getSceneCoef.R", "getInfoFromLevel1Name.R", 
            "allignDataGeometry.R", "lmIlluminationCorrection.R",
            "cCorrection.R"))
  source(paste0(dsn, src.filepath, "/", i))


#### Crop auxiliary data #######################################################
# dem.data <- raster(paste0(dsn, dem.filepath, "original/HessenDGM_10m_proj.tif"),
#                    native = TRUE)
crop.template <- raster(paste0(dsn, crop.filepath), native = TRUE)
# hillshade.data <- raster(paste0(dsn, hillshade.filepath), native = TRUE)
# slope.data <- raster(paste0(dsn, slope.filepath), native = TRUE)
# aspect.data <- raster(paste0(dsn, aspect.filepath), native = TRUE)
# print("Croping auxiliary data...")
# dem.data.crop <- allignDataGeometry(dem.data, crop.template)
# hillshade.data.crop <- allignDataGeometry(hillshade.data, crop.template)
# slope.data.crop <- allignDataGeometry(slope.data, crop.template)
# aspect.data.crop <- allignDataGeometry(aspect.data, crop.template)
# writeRaster(dem.data.crop, filename = paste0(dsn, dem.filepath, 
#                                              "/dgm10_mr.tif"),
#             format = "GTiff", overwrite = TRUE)

#### Crop, calibrate and illumination correct satellite bands ##################
# Crop, callibrate and illumination correct each satellite band file and write
# results to level 2 folder. Auxiliary data is only croped during the first loop.
crop.template <- raster(paste0(dsn, crop.filepath), native = TRUE)

datasets <- list.files(path = ".", 
                       pattern =  glob2rx("*.TIF"),
                       full.names = TRUE, recursive = TRUE)
sapply(datasets,function(x){
  act.filepath <- x
  print(paste0("Computing file ", act.filepath))
  act.info <- getInfoFromLevel1Name(act.filepath, sensor)
  coef <- getSceneCoef(act.info[2], act.info[1], rad.unit, sensor)
  act.data <-  raster(act.filepath, native = TRUE)
  act.data.crop <- allignDataGeometry(act.data, crop.template)
  act.data.calib <- coef[1] * act.data.crop + coef[2]
  #act.data.calib.ic <- cCorrection(act.data.calib, coef[4], coef[5],
  #                                 slope.data.crop, aspect.data.crop)
  writeRaster(act.data.calib, filename = paste0(dsn, output.path, "/",
                                                   basename(act.filepath)),
              format = "GTiff", overwrite = TRUE)
  if[(act.info[1] == "10" | act.info[1] == "11") & rad.unit == "rad"]{
    coef <- getSceneCoef(act.info[2], act.info[1], "tir", sensor)
    act.data.calib.ir <- coef[2] / ((log(coef[1]/act.data.calib)+1))
    act.filename <- paste0(substr(basename(act.filepath), 1, 
                                  nchar(basename(act.filepath))-4), "_K", ".tif")
    writeRaster(act.data.calib, filename = paste0(dsn, output.path, "/",
                                                  act.filename),
                format = "GTiff", overwrite = TRUE)
  }
})

#### Crop RapidEye #############################################################
datasets <- list.files(path = paste0(dsn, rapideye.path), 
                       pattern =  glob2rx("*0.tif$"),
                       full.names = TRUE, recursive = TRUE)
sapply(datasets,function(x){
  act.filepath <- x
  act.data <- stack(act.filepath)
  print(paste0("Computing file ", act.filepath))
  act.data.crop <- allignDataGeometry(act.data, crop.template)
  writeRaster(act.data.crop, filename = paste0(dsn, rapideye.path, 
                                                "_processed/",
                                                basename(act.filepath)),
              format = "GTiff", overwrite = TRUE)
})

