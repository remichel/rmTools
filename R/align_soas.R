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

##' aligns SOAs
##' @description \code{align_soas}
##' evaluates a vector of SOAs and tries to align slightly different SOA values by rounding them in a way that we end up in n_soa different SOAs.
##' If you want, align_soas can plot a histogram of difference between original input vector and the output vector provided by this function.
##' @param soa_vec vector of SOAs
##' @param n_soa number of SOAs in the paradigm
##' @param round_factor defines the second argument for the round() command decimal value with which to start the rounding procedure.
##' @param soa_spacing needs ms value of spacing between SOAs. Will use this to group all SOAs with a difference < soa_spacing. By default, it will try to achieve n_soa SOAs with round_factor =2 and will give out warnings.
##' @param verbose def = T
##' @param difference_plot def = F
##' @examples
##' repeated_data = rep_dataframe(data, 2, "rbind")
##' @author René Michel
##' @export align_soas
##' @name align_soas

align_soas <- function(soa_vec, n_soa = 20, round_factor = 2, soa_spacing = 42,  verbose = TRUE, difference_plot = FALSE){

    # sanity checks
  if(n_soa < 1)                     stop('n_soa needs to be greater than 0')
  if(soa_spacing < 1)               stop('soa_spacing needs to be greater than 0')
  if(mod(n_soa,1))                  stop('n_soa needs to be an integer and no decimal number.')
  if(is.logical(verbose) == F)      stop('Verbose needs to be logical.')
  if(is.logical(difference_plot) == F)      stop('Difference_plot needs to be logical.')
  if(size(soa_vec)[1] > 1)          stop('Dimension mismatch: soa_vec must be a vector.')
  if(round_factor < 1)              stop('Round factor needs to be greater than 0')
  if(mod(round_factor,1))           stop('Round factor needs to be an integer and no decimal number.')

  if(difference_plot)       original_vec = soa_vec

  if(verbose)               disp(paste0('Used n_soa =', n_soa, ', round factor = ', round_factor, ' decimals and soa spacing = ', soa_spacing,'ms.'))
  if(verbose)               disp(paste(length(xtabs(~soa_vec)), 'different SOAs found. Will cut number of SOAs down to', n_soa,'.'))

  # round SOAs to merge them more easily
  while(length(unique(soa_vec))>n_soa && round_factor > 0){
    soa_vec = round(soa_vec,round_factor)
    round_factor = round_factor-1
  }

  # group all SOAs with a smaller spacing than 0.5*soa_spacing
  if (length(unique(soa_vec))>n_soa){
    if(verbose)             disp(paste(length(xtabs(~soa_vec)), 'different SOAs found after round procedure. Will continue to cut number of SOAs down to', n_soa,'.'))

    soas = sort(unique(soa_vec))
    diff = soas[-1]-soas[-length(soas)]
    delete_soa = which(diff < soa_spacing*0.5)
    for (iDelete in 1:length(delete_soa)){
      soa_vec = ifelse(soa_vec == soas[delete_soa[iDelete]],soas[delete_soa[iDelete]+1], soa_vec)
    }

    # Overview over new SOAs
    if(verbose){
      disp(paste(length(xtabs(~soa_vec)), 'different SOAs found after difference vector calculation. If it diverges from', n_soa,'you should dig deeper into this problem manually.'))
      print(xtabs(~soa_vec))
    }

    if(length(xtabs(~soa_vec)) != n_soa) stop('Unable to find a solution to align all SOAs. Recommended  to adjust rounding_factor.')

    # difference plot
    if(difference_plot) hist(original_vec-soa_vec,
                             main = "Difference Original SOA values - Aligned SOA values",
                             xlab = "Difference in ms",
                             ylab = "Frequency")

    return(soa_vec)


  }else{

    # Overview over new SOAs
    if(verbose) disp(xtabs(~soa_vec))
    if(length(xtabs(~soa_vec)) != n_soa) stop('Unable to find a solution to align all SOAs. Recommended  to adjust rounding_factor.')

    # difference plot
    if(difference_plot) hist(original_vec-soa_vec,
                             main = "Difference Original SOA values - Aligned SOA values",
                             xlab = "Difference in ms",
                             ylab = "Frequency")


    return(soa_vec)

  }
}

