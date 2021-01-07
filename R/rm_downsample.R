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

##' rm_downsample
##' @description \code{rm_downsample}
##' downsample a data vector and transform its corresponding time vector
##' @param vec  data vector
##' @param t time vector
##' @param f factor of downsampling (integer)
##' @param method "filter" or "average" (uses time windows of f to average the time course) or "rollmean" (from zoo package)
##' @examples
##' rm_downsample(c(1,-1,3, 4), c(1,2,3,4), 2)
##' @author René Michel
##' @export rm_downsample
##' @name rm_downsample

rm_downsample <- function(vec, t, f, method){

  # sanity checks
  if((f %% 1)) stop('f needs to be an integer')
  if(!is.character(method)) stop('method needs to be a character')
  if(!is.vector(t)) t = as.vector(t)
  if(!is.vector(vec)) vec = as.vector(vec)

  ds_time = rep(t[seq(1,length(t), f)], f)

  ds_centered_time = rep(rollmean(t,f)[seq(1,length(t),f)], f)

  if(any(is.na(ds_centered_time))) ifelse(ds_centered_time == NA, max(t), ds_centered_time)

  if(method == "filter"){
    ds_data = rep(decimate(vec,f),f)
  }else if(method == "average"){
    ds_data = as.vector(by(vec, sort(ds_time)[1:length(vec)], mean))
  }else if(method == "rollmean"){
    ds_data = rollmean(vec, 3)[seq(1, length(vec), f)]
  }

  ds = as.data.frame(cbind(ds_time, ds_centered_time, ds_data))
  colnames(ds) = c("ds_time", "ds_centered_time", "ds_data")
  ds = arrange(ds, ds_time)

  ds = ds[1:length(vec),]

  return(ds)

}

