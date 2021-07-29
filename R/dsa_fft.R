# Copyright (c) 2020 Rene Michel

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' performs an FFT on behavioral data
##' @description \code{dsa_fft}
##' A function to compute the fft results for a time series (especially dense sampled behavioral data).
##' By default, dsa_fft will treat the data so that each column is considered one timepoint and computes all steps by row.
##' If your input data is organized with time = rows, change time argument to 'row'.
##' Both sampling frequency and desired frequency resolution (Bin width per frequency bin of FFT) needs to be in Hz.
##'
##' @param data requires the dataframe
##' @param time are timepoints across columns or rows? default = "col"
##' @param detrend Detrending the data (default = 'linear', also available: 'dc', '2ndorder' or 'none')
##' @param window Windowing whole time series (default = 'tukey', also available: 'hanning', 'none')
##' @param scaling Rescaling the data (default: 'z' for z-transform, also available: 'none' )
##' @param padding Padding the data (default: 'zeros' for zero-padding, also available: 'mean', 'none')
##' @param desired_res Padding the data up to desired_res (automatically evaluates necessary number of pads to achieve the resolution, default = 1)
##' @param sfreq sampling frequency of inserted datapoints
##' @param desired_out Extract amplitude values (default: 'amp', also available 'power' or 'logPower')
##' @param single_sided Single-sided frequency spectum (default = T)
##' @param out By default, output is Mod (Amplitude) of the signal. Change to 'arg' to get phase or 'complex' to get the complex output.
##' @param alpha only for tukey window
##' @param verbose T/F
##' @examples
##' @author Rene Michel
##' @export dsa_fft
##' @name dsa_fft


dsa_fft <- function(data, time = 'col', detrend = 'linear', window = 'tukey', scaling = 'z' , padding = 'zeros', desired_res = 1, sfreq = 25, desired_out = 'amp', single_sided = T, out = 'mod', alpha = .1, verbose = T){
  # A function to compute the fft results for a time series (especially dense sampled behavioral data)
  # By default, it computes the following steps:
  # 1) Rearranging data if necessary (to long)
  # 2) Detrending the data (default = 'linear', also available: 'dc', '2ndorder' or 'none')
  # 3) Windowing whole time series (default = 'tukey', also available: 'hanning', 'none')
  # 4) Rescaling the data (default: 'z' for z-transform, also available: 'none' )
  # 5) Padding the data (default: 'zeros' for zero-padding, also available: 'mean', 'none')
  # 6) Padding the data up to desired_res (automatically evaluates necessary number of pads to achieve the resolution, default = 1)
  # 7) Extract amplitude values (default: 'amp', also available 'power' or 'logPower')
  # 8) Extract phase values (default = F)
  # 9) Single-sided frequency spectum (default = T)

  # By default, dsa_fft will treat the data so that each column is considered one timepoint and computes all steps by row.
  # If your input data is organized with time = rows, change time argument to 'row'.

  # Both sampling frequency and desired frequency resolution (Bin width per frequency bin of FFT) needs to be in Hz.

  # By default, output is Mod (Amplitude) of the signal. Change to 'arg' to get phase.

  # Check n dims of input data
  if(length(dim(data))>2) stop('Input data must be 2D.')

  # Check length of time domain
  if((time == 'row' & dim(data)[1] < 5) | (time == 'col' & dim(data)[2] < 5)) {
    stop('length of data seems to be smaller than 5 datapoints.\nby this, nyquist would be 2 Hz. please revise your input to time argument.\n')
  }else if((time == 'row' & dim(data)[1] < 11) | (time == 'col' & dim(data)[2] < 11)){
    if(verbose) message('WARNING:\nlength of data seems to be smaller than 11 datapoints.\nby this, nyquist would be 5 Hz. will perform fft anyways but highly recommend to revise your input to dsa_fft.\n')
  }else{
    if(verbose) message(paste('Will compute FFT assuming',time,'as timepoints.\n'))
  }

  # Check input data
  if(is.data.frame(data)) data = as.matrix(data)

  # Reshape long to wide
  if(time == 'row') data = t(data)

  # Detrend
  if(detrend == 'linear'){
    data = t(detrend(t(data)))
    if(verbose) message(paste('Detrended (linear)...\n'))
  }else if(detrend == 'dc'){
    data = t(detrend(t(data), tt = 'constant'))
    if(verbose) message(paste('Detrended (dc only)...\n'))
  }else if(detrend == '2ndorder'){
    data = t(apply(data, 1, function(x.col) lm(x.col ~ poly(c(1:length(x.col)), 2))$residuals))
    if(verbose) message(paste('Detrended (2ndorder)...\n'))
  }else if(detrend == 'none'){
    if(verbose) message(paste('No detrending applied...\n'))
  }

  # Windowing
  winLength = dim(data)[2]

  if(window == 'hanning'){
    library('e1071')
    data = t(t(data)*hanning.window(winLength))
    if(verbose) message('Hanning window...\n')
  }else if(window == 'tukey'){
    library('bspec')
    data = t(t(data)*tukeywindow(winLength,alpha))
    if(verbose) message(paste('Tukey window with alpha = ', alpha,'applied...\n'))
  }else{
    if(verbose) message('No window applied...\n')
  }

  # Scaling
  if(scaling == 'z'){
    data = data.frame(t(apply(data,1,scale)))
    if(verbose) message('Z-transformation applied...\n')
  }else if(scaling == 'none'){
    if(verbose) message('No scaling applied...\n')
  }

  # Padding
  if(padding != 'none'){

    n_padding = ceil(sfreq/desired_res-(dim(data)[2]-1)) # find number of pads to achieve required resolution
    if (n_padding < 2) n_padding = 2
    new_res = sfreq/(dim(data)[2]+n_padding) # compute new resolution
    if(verbose) message(paste('Desired resolution was', desired_res,'Hz.\nBy padding with', n_padding, 'additional points, a frequency resolution of', new_res,'Hz per bin will be achieved.\n'))


    if(padding == 'zeros'){
      if(mod(n_padding,2) == 0){
        data = cbind(zeros(dim(data)[1],n_padding/2), data, zeros(dim(data)[1],n_padding/2))
      }else{
        data = cbind(zeros(dim(data)[1],ceil(n_padding/2)), data, zeros(dim(data)[1],floor(n_padding/2)))
      }
      if(verbose) message('Zero-padding applied...\n')
    }else if(padding == 'mean'){
      if(mod(n_padding,2) == 0){
        data = cbind(t(repmat(apply(data,1,mean),n_padding/2,1)), data, t(repmat(apply(data,1,mean),n_padding/2,1)))
      }else{
        data = cbind(t(repmat(apply(data,1,mean),ceil(n_padding/2),1)), data, t(repmat(apply(data,1,mean),floor(n_padding/2),1)))
      }
      if(verbose) message('Mean-padding applied...\n')
    }
  }else{
    new_res = sfreq/(dim(data)[2])
    if(verbose) message('No padding applied...\n')
  }

  # Calculate frequencies
  frequencies = seq(new_res,sfreq/2,new_res)
  n_freq = length(frequencies)
  if(verbose) message(paste('Will compute FFT for', n_freq, 'frequency bins...\n'))

  # Apply FFT
  if(out == 'mod'){
    spectrum = as.data.frame(t(Mod(apply(as.matrix(data),1,fft))))
    if(verbose) message('Extract amplitude  ...\n')
  }else if(out == 'arg'){
    phase = as.data.frame(t(Arg(apply(as.matrix(data),1,fft))))
    if(verbose) message('Extract phase ...\n')
  }else if(out == 'complex'){
    fftOut = as.data.frame(t(apply(as.matrix(data),1, fft)))
    if(verbose) message('Extract complex FFT output ...\n')
  }

  # Extract single-sided frequency spectrum
  if(single_sided){
    if(out == 'mod'){
      spectrum = spectrum[,2:(n_freq+1)]
      colnames(spectrum) = frequencies
      # Compute desired output
      if(desired_out == 'power'){
        spectrum = spectrum^2
        if(verbose) message('Power was computed...\n')
      }else if(desired_out == 'logPower'){
        spectrum = log(spectrum^2)
        if(verbose) message('logPower was computed...\n')
      }else if(desired_out == 'amp'){
        if(verbose) message('Simple amplitude was computed...\n')
      }
    }else if(out == 'arg'){
      phase = phase[,2:(n_freq+1)]
      colnames(phase) = frequencies
    }else if(out == 'complex'){
      fftOut = fftOut[, 2:(n_freq + 1)]
      colnames(fftOut) = frequencies
    }
    if(verbose) message('Single-sided spectrum was computed...\n')
  }

  # Return desired outputs
  if(out == 'mod'){
    return(spectrum)
  }else if(out == 'arg'){
    return(phase)
  }else if(out == 'complex'){
    return(fftOut)
  }
}
