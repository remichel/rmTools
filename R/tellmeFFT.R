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

##' tellmeFFT
##' @description \code{tellmeFFT}
##' informs you about the FFT characteristics for your proposed dense sampling study.
##' @param spaceBetweenIntervals Spacing between SOAs in ms
##' @param numSoa number of SOAs in the paradigm
##' @param startingSoa first SOA in ms after time locking event (e.g. cue)
##' @param freqOfInterest which frequency are you interested? in Hz
##' @examples
##' tellmeFFT(42,20,192,4)
##' @author René Michel
##' @export tellmeFFT
##' @name tellmeFFT

tellmeFFT <- function(spaceBetweenIntervals = NA, numSoa = NA, startingSoa = 0, freqOfInterest = 4){
  # Calculates the Frequency
  end = startingSoa+spaceBetweenIntervals*(numSoa-1)
  fs = 1000/spaceBetweenIntervals
  d= 1000*numSoa/fs
  fres = fs/numSoa
  fmax = fs/2
  fnum = floor((numSoa/2))
  fullcycles = end/(1000/freqOfInterest)
  message(sprintf(paste0('Your %.0f SOAs range from %.0f ms to %0.f ms with steps of %.0f ms, providing a sampling frequency of %.2f Hz.\n',
                         'An FFT would deliver %.f frequency bins (excluding DC offset) with a Nquist frequency of %.2f Hz and a resolution of %.2f Hz per bin.\n',
                         'Frequency of Interest (%.2fHz) can be covered for %.2f cycles.\n'),
                  numSoa, startingSoa, end, spaceBetweenIntervals, fs,
                  fnum,fmax,fres,
                  freqOfInterest, fullcycles))
}
