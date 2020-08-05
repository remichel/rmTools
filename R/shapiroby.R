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

##' Perform shapiro test by group
##' @description \code{shapiroby}
##' Adopted from Marco Sandri.
##' @param data vector or dataframe with multiple columns which contain the data
##' @param group group variable which is used to split the data vector(s)
##' @examples
##' res = shapiro.by(data$reactiontime, data$group)
##' @author René Michel
##' @export shapiroby
##' @name shapiroby
##' @import dplyr

shapiroby <- function(data, group) {
  data.frame(data, group) %>%
    group_by(group) %>%
    summarise(statistic = ifelse(sd(data)!=0,shapiro.test(data)$statistic,NA),
              p.value = ifelse(sd(data)!=0,shapiro.test(data)$p.value,NA)
    )
}
