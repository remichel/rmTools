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

##' closest_value
##' @description \code{closest_value}
##' returns the numerically closest value in vector 2 for each element of vector 1
##' @param vec1 vector of numbers to find neighbours for
##' @param vec2 vector of numbers to find neighbours from (e.g. the new bins)
##' @examples
##' a = seq(1,100,2)
##' b = seq(10,100,10)
##' closest_value(a,b)
##' @author René Michel
##' @export closest_value
##' @name closest_value

closest_value <- function(vec1, vec2){
  
  # sanity checks
  if(!is.numeric(vec1) | !is.numeric(vec2)) stop('vecs need to be numeric')
  if(!is.vector(vec1)) vec1 = as.vector(vec1)
  if(!is.vector(vec2)) vec1 = as.vector(vec2)
  
  # transpose vector 2 to column vector
  vec2 = t(vec2)
  
  #  determine closest values
  cv = vec2[which(abs(vec1-repmat(vec2,length(vec1),1)) == apply(abs(vec1-repmat(vec2,length(vec1),1)), 1, min), arr.ind=TRUE)[,2]]
  
  return(cv)
  
}

