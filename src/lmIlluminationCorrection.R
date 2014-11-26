lmIlluminationCorrection <- function(data, hsm){
  # Correct illumination using a simple linear model between a hillshade
  # model and the band information.
  #
  # Args:
  #   data: raster layer to be illumination corrected
  #   hsm: hillshade model
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
  
  hsm.lm <- lm(getValues(data) ~ getValues(hsm))
  hsm.pred <- hsm.lm$coefficients[1] + hsm * hsm.lm$coefficients[2]
  hsm.pred <- hsm.pred - cellStats(data, 'mean')
  data.ic <- data + hsm.pred
  return(data.ic)
}