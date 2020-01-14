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

##' Creates a number of directories
##' @description \code{rep_dataframe}
##' Repeats a dataframe n times by rbind or cbind.
##' @param data requires the dataframe to be copied.
##' @param times requires the number of times the dataframe has to be repeated. Defaults to 1.
##' @param by requires a character string specifying the function to bind the repeated dataframes("rbind", "cbind"). Defaults to "rbind".
##' @examples
##' repeated_data = rep_dataframe(data, 2, "rbind")
##' @author René Michel
##' @export rep_dataframe
##' @name rep_dataframe

rep_dataframe <- function(data, times = 1, by = "rbind"){

  if(!is.character(by)) stop('by argument needs to be a character')
  if(mod(times,1) != 0) stop('times argument must be an integer')

  if(by == "rbind"){
    repMat = do.call("rbind", replicate(times, data, simplify = FALSE))
  }else if(by == "cbind"){
    repMat = do.call("cbind", replicate(times, data, simplify = FALSE))
  }else{
    stop('Invalid by argument. Use rbind or cbind instead.')
  }

  return(repMat)
}
