# Copyright (c) 2020 Rene Michel

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

##' Permutes a DSA dataset.
##' @description \code{permute_by}
##' In case of unequal SOA values in different datasets, this function merges the slightly different SOA values.
##' @param data DSA dataset. Needs to be in longformat.
##' @examples
##' # Group all different SOA values in data$soa to 20 SOAs with a desired spacing of 42ms
##' data = permute_by()
##' @author Rene Michel
##' @export permute_by
##' @name permute_by
##'
permute_by <- function(data, nperm = 1000, seednum = 1){

  # set seed
  set.seed(seednum)

  # get all values to permute by
  subjects = unique(data$subj)
  validity = unique(data$validity)

  # save original data
  original = data

  # shuffle SOAs
  for (iPerm in 1:nperm){
    tmp = original
    tmp$permNum = iPerm
    for(iSub in subjects){
      for(iVal in validity){
        tmp$soa[tmp$subj == iSub & tmp$validity == iVal] = sample(original$soa[original$subj == iSub & original$validity == iVal])
      }
    }
    data = rbind(data,tmp)
  }

  return(data)



}

