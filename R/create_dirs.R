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
##' @description \code{create_dirs}
##' Creates specified folders in subsequent order.
##' Is able to create nested folders - for this, the outer folder must be parsed before the inner ones.
##' @param ... requires a vector of character strings.
##' @examples
##' # create 2 new folders
##' dirs = c("Testfolder", "Testfolder2")
##' create_dirs(dirs)
##' @author René Michel
##' @export create_dirs
##' @name create_dirs


create_dirs <- function(...){

  inputs = unlist(list(...))

  # sanity checks
  if(length(inputs) < 1) stop('Parse at least 1 argument to create_dirs.')
  if(!is.character(inputs)) stop('Please parse characters to create_dirs.')

  # create folders
  for (iInput in 1:length(inputs)){
    if (dir.exists(inputs[iInput]) == F){
      dir.create(inputs[iInput], showWarnings = T)
    }
  }
}
