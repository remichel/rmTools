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

##' enables RM_theme for ggplot2
##' @description \code{set_theme_RM}
##' Once called, it simply enables us to add + theme_RM to ggplots
##' @author René Michel
##' @examples
##'
##' set_theme_RM()
##' ggplot(...)+theme_RM
##'
##' @export set_theme_RM
##' @name set_theme_RM

set_theme_RM = function(){

# Once called, it simply enables us to add + theme_RM to ggplots
theme_RM = theme_classic()+theme(legend.key.width = unit(3, 'cm'),
                                 strip.background = element_rect(colour="black", fill="grey70"),
                                 strip.text = element_text(colour = "black", size = 10, face = 'bold'),
                                 axis.line = element_line(colour = 'black', size = 2.2),
                                 axis.ticks = element_line(colour = "black", size = 2.2),
                                 panel.border = element_rect(color = "black", fill = NA, size = 2.2),
                                 axis.ticks.length = unit(.4, "cm"),
                                 axis.title.y = element_text(size = 20, face = 'bold',margin = margin(t = 0, r = 30, b = 0, l = 0)),
                                 axis.title.x = element_text(size = 20,face = 'bold', margin = margin(t = 40, r = 0, b = 0, l = 0)),
                                 axis.text.y = element_text(size = 15,face = 'bold', margin = margin(t = , r = 12, b = 0, l = 0), color = 'black'),
                                 axis.text.x = element_text(size = 14,face = 'bold', margin = margin(t = 12, r = 0, b = 0, l = 0), color = 'black'),
                                 strip.text.x = element_text(size = 15),
                                 title = element_text(size = 15, face = "bold"),
                                 legend.text = element_text(size = 14,face = 'bold'),
                                 legend.title = element_blank(),
                                 legend.key.size = unit(9,"point"),
                                 legend.position = "bottom",
                                 legend.box = "vertical",
                                 legend.spacing.x = unit(.25,"cm"))

return(theme_RM)
}
