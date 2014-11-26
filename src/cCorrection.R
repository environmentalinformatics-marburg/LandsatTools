cCorrection <- function(data, szen, sazm, tslp, taspt){
  # Correct illumination using a c correction.
  #
  # Args:
  #   data: raster layer to be illumination corrected
  #   szen: sun zenith angle
  #   sazm: sun azimuth angle
  #   tslp: terrain slope
  #   taspt: terrain azimuth angle
  #
  # Returns: raster layer containing illumination corrected band
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
  
  szen <- szen * pi/180.0
  sazm <- sazm * pi/180.0
  tslp <- tslp * pi/180.0
  taspt <- taspt * pi/180.0
  print(szen)
  cos.i <- cos(getValues(tslp)) * cos(szen) + 
    sin(getValues(tslp)) * sin(szen) * cos(sazm - getValues(taspt))
  ck.lm <- lm(getValues(data) ~ cos.i)
  ck <- ck.lm$coefficients[1] / ck.lm$coefficients[2]
  data.ic <- data * (cos(szen) + ck) / (cos.i + ck)
  return(data.ic)
}