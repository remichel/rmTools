# Copyright (c) 2021 René Michel

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

##' check_div
##' @description \code{check_div}
##' returns all elements of a vector that produce a modulo of 0 for a given number. 
##' @param vec vector of numbers to be tested
##' @param num number to be divided
##' @examples
##' check_div(seq(2,256,2), 456)
##' @author René Michel
##' @export check_div
##' @name check_div

check_div <- function(vec, num){
  
  # sanity checks
  if(!is.numeric(vec) | !is.numeric(num)) stop('vec needs to be numeric')
  
  # check divisibility
  divisible = vec[which(num %% vec == 0)]
    
  return(divisible)
    
}

