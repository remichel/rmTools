# Copyright (c) 2020 René Michel

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' summaryRMwithin with extras
##' @description \code{summaryRMwithin}
##' Adds option "doNorm" to summarySEwithin, to have control over normalization of means. Note: Correction is still applied using normalized data (to get adequate estimates for SE), but means stay unaffected
##' @param doNorm (default = T)
##' @author René Michel
##' @export summaryRMwithin
##' @name summaryRMwithin
##' @import Rmisc

summaryRMwithin = function (data = NULL,
                            measurevar,
                            betweenvars = NULL,
                            withinvars = NULL,
                            idvar = NULL,
                            doNorm = T,
                            na.rm = FALSE,
                            conf.interval = 0.95,
                            .drop = TRUE)
{

  # Adds option "doNorm" (default = T) to summarySEwithin, to have control over normalization of means.
  # Note: Correction is still applied using normalized data (to get adequate estimates for SE), but means stay unaffected




  factorvars <- sapply(data[, c(betweenvars, withinvars),
                            drop = FALSE], FUN = is.factor)

  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]

    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))

    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }

  if(doNorm){
    # Just follow the summarySEwithin routine
    data <- normDataWithin(data, idvar, measurevar, betweenvars,
                           na.rm, .drop = .drop)

    measureNormedVar <- paste(measurevar, "Normed", sep = "")

    data[, measurevar] <- data[, measureNormedVar]

    datac <- summarySE(data, measurevar, groupvars = c(betweenvars,
                                                       withinvars), na.rm = na.rm, conf.interval = conf.interval,
                       .drop = .drop)

    nWithinGroups <- prod(sapply(datac[, withinvars, drop = FALSE],
                                 FUN = nlevels))

    correctionFactor <- sqrt(nWithinGroups/(nWithinGroups -
                                              1))
    datac$sd <- datac$sd * correctionFactor
    datac$se <- datac$se * correctionFactor
    datac$ci <- datac$ci * correctionFactor

    return(datac)
  }else{
    # follow the summarySEwithin routine to receive the corrected sd/se/ci
    # but use the unnormalized mean

    raw <- data

    data <- normDataWithin(data, idvar, measurevar, betweenvars,
                           na.rm, .drop = .drop)

    measureNormedVar <- paste(measurevar, "Normed", sep = "")

    data[, measurevar] <- data[, measureNormedVar]

    datac <- summarySE(data, measurevar, groupvars = c(betweenvars,
                                                       withinvars), na.rm = na.rm, conf.interval = conf.interval,
                       .drop = .drop)

    nWithinGroups <- prod(sapply(datac[, withinvars, drop = FALSE],
                                 FUN = nlevels))

    correctionFactor <- sqrt(nWithinGroups/(nWithinGroups -
                                              1))
    datac$sd <- datac$sd * correctionFactor
    datac$se <- datac$se * correctionFactor
    datac$ci <- datac$ci * correctionFactor

    dataRaw <- summarySE(raw, measurevar, groupvars = c(betweenvars,
                                                        withinvars), na.rm = na.rm, conf.interval = conf.interval,
                         .drop = .drop)
    dataRaw$sd <- datac$sd
    dataRaw$se <- datac$se
    dataRaw$ci <- datac$ci

    return(dataRaw)
  }


}
