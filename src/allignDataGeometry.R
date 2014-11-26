allignDataGeometry <- function(data, template, method = "bilinear"){
  # Allign raster data by bringing it in the same geometry and extend.
  # If the data is not in the same projection as the template, the allignment
  # will be computed by reprojection only. If the data has already the same
  # projection, the data set will be croped and aggregated prior to resampling
  # in order to reduce computation time.
  #
  # Args:
  #   data: raster layer to be resampled
  #   template: raster or spatial data set from which geometry can be extracted
  #   method: method for resampling ("ngb" or "bilinear")
  #
  # Returns: raster layer containing geometrically alligned data
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
  
  lib <- c("raster")
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  
  if(projection(data) == projection(template)){
    data <- crop(data, template, snap = "out")
    if(class(template) == "RasterLayer"){
      if(data@ncols / template@ncols >= 2){
        factor <- floor(data@ncols/template@ncols)
          data <- aggregate(data, fact = factor, fun = mean, 
                            expand=TRUE)
      }
      data <- resample(data, template, method = method)
    }
  } else {
    data <- projectRaster(data, template, method = method)
  }
  return(data)
}

