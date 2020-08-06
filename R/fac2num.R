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

##' convert factor to numeric
##' @description \code{fac2num}
##' converts factor levels to numeric values
##' @param c the factor to be converted
##' @author René Michel
##' @export fac2num
##' @name fac2num

fac2num <- function(c){

  if(!is.factor(c)){
    stop('input is not a factor')
  }else{
    f = as.numeric(as.character(c))
  }
  return(f)
}


