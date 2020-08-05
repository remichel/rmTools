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

##' creates a predefined histogram
##' @description \code{plot_gg_hist}
##' Once called, it simply enables us to add + theme_RM to ggplots
##' @author René Michel
##' @param data data
##' @param xvar xvar
##' @param plot_title title
##' @param facets facet var
##' @param fill fill?
##' @param binwidth bin width
##' @param col line col
##' @param fillcol fill col
##' @param breaks breaks
##' @export plot_gg_hist
##' @name plot_gg_hist


plot_gg_hist <- function(data,xvar = 'DummyVar',plot_title = 'Dummy Title', facets = NA, fill = NA, binwidth = 10, col = "black", fillcol = "grey", breaks = seq(-180,180,20)){

  # sanity checks

  # xvar, plot title, all facets and fill need to be a characters
  if(!is.character(xvar)) stop('xvar needs to be a character.')
  if(!is.character(plot_title)) stop('plot title needs to be a character.')
  if(!is.character(fill) & !is.na(fill)) stop('fill needs to be a character.')
  if(!any(is.character(facets)) & any(!is.na(facets))) stop('facets need to be a characters.')
  if(any(which(xvar == colnames(data)))){colnum = which(xvar == colnames(data))}else{stop('Assigned xvar doesnt seem to be part of parsed data.frame.')}
  if(!is.na(facets)){
    if(length(facets)>3) stop('Number of facets exceeds maximum facet number')
    facetcol = NA
    for(i in 1:length(facets)){
      if(any(which(facets[i] == colnames(data)))){
        facetcol[i] = which(facets[i] == colnames(data))
      }else{
        stop('At least one of the assigned facet variables doesnt seem to be part of parsed data.frame.')
      }
    }
    for(i in 1:length(facets)){
      colnames(data)[facetcol[i]] = paste0('facet',i)
    }
  }
  if(!is.na(fill)){
    if(any(which(fill == colnames(data)))){
      fillcolnum = which(fill == colnames(data))
    }else{
      stop('Assigned xvar doesnt seem to be part of parsed data.frame.')
    }
    colnames(data)[fillcolnum] = 'fillvar'
  }


  # rename target column
  colnames(data)[colnum] = 'xvar'

  if(is.na(fill)){
    # do plot
    if(is.na(facets)){
      plot = ggplot(data, aes(x = xvar))+
        geom_histogram(binwidth = binwidth, color=col, fill=fillcol)+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        ggtitle(plot_title)+
        labs(x = xvar)
    }else if(length(facets) == 1){
      plot = ggplot(data, aes(x = xvar))+
        geom_histogram(binwidth = binwidth, color=col, fill=fillcol)+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        ggtitle(plot_title)+
        facet_wrap(~facet1)+
        labs(x = xvar)
    }else if(length(facets) == 2){
      plot = ggplot(data, aes(x = xvar))+
        geom_histogram(binwidth = binwidth, color=col, fill=fillcol)+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        ggtitle(plot_title)+
        facet_wrap(~facet1+facet2)+
        labs(x = xvar)
    }else if(length(facets) == 3){
      plot = ggplot(data, aes(x = xvar))+
        geom_histogram(binwidth = binwidth, color=col, fill=fillcol)+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        ggtitle(plot_title)+
        facet_wrap(~facet1+facet2+facet3)+
        labs(x = xvar)
    }

  }else{

    if(is.na(facets)){
      plot = ggplot(data, aes(x = xvar, fill = fillvar))+
        geom_histogram(alpha = 0.3, binwidth = binwidth, position = "identity")+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=c(col, fillcol)) +
        ggtitle(plot_title)+
        labs(x = xvar,fill = fill)
    }else if(length(facets) == 1){
      plot = ggplot(data, aes(x = xvar, fill = fillvar))+
        geom_histogram(alpha = 0.3, binwidth = binwidth, position = "identity")+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=c(col, fillcol)) +
        ggtitle(plot_title)+
        facet_wrap(~facet1)+
        labs(x = xvar, fill = fill)
    }else if(length(facets) == 2){
      plot = ggplot(data, aes(x = xvar, fill = fillvar))+
        geom_histogram(alpha = 0.3, binwidth = binwidth, position = "identity")+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=c(col, fillcol)) +
        ggtitle(plot_title)+
        facet_wrap(~facet1+facet2)+
        labs(x = xvar,fill = fill)
    }else if(length(facets) == 3){
      plot = ggplot(data, aes(x = xvar, fill = fillvar))+
        geom_histogram(alpha = 0.3, binwidth = binwidth, position = "identity")+
        scale_x_continuous(breaks=breaks,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=c(col, fillcol)) +
        ggtitle(plot_title)+
        facet_wrap(~facet1+facet2+facet3)+
        labs(x = xvar,fill = fill)
    }
  }




  return(plot)
}


