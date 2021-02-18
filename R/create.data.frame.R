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

##' create dataframe
##' @description \code{create.data.frame}
##' creates a data.frame and allows to add colnames 
##' @param default_value which value to add in all cells? defaults to NA
##' @param nrow number of rows
##' @param ncol number of cols 
##' @param col.names character vector with colnames
##' @examples
##' d = create.data.frame(NA, 2, 2, c("Col1", "Col2"))
##' @author René Michel
##' @export create.data.frame
##' @name create.data.frame

create.data.frame <- function(default_value = NA, nrow = 2, ncol = 2, col.names = c("V1", "V2")){
  
    df = as.data.frame.matrix(matrix(default_value,nrow, ncol, dimnames = list(NULL, col.names)))
    
    return(df)
}

