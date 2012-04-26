###   Miscellaneous methods that must be defined last in the library
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

## Note that  require( stats )  has already happened ...

comparePred.lme <- comparePred.lmList <- comparePred.gls

getData.nlme <- getData.gnls

getData.lme <- getData.gls <- getData.nls

qqnorm.gls <- qqnorm.lm <- qqnorm.nls

plot.lme <- plot.nls

fitted.gnls <- fitted.gls

residuals.gnls <- residuals.gls

vcov.gls <- function (object, ...) object$varBeta

vcov.lme <- function (object, ...) object$varFix

.onUnload <- function(libpath)
    library.dynam.unload("nlme", libpath)

