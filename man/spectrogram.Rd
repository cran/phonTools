\name{spectrogram}
\alias{spectrogram}
\alias{plot.spectrogram}
\alias{print.spectrogram}
\title{
Create Spectrograms
}
\description{
Create and display spectrograms.
}
\usage{
spectrogram (sound, fs = 22050, windowlength = 5, freqres, timestep = -400, 
    preemphasisf = 50, maxfreq = 5000, colors = TRUE, dynamicrange = 50, 
	nlevels = dynamicrange, maintitle = "", show = TRUE, output = FALSE, 
	window = 'kaiser', windowparameter = 3, pause = FALSE)
}
\arguments{
  \item{sound}{Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.}
  \item{fs}{The sampling frequency in Hz. If a sound object is passed this does not need to be specificed.}
  \item{windowlength}{The desired analysis windowlength in milliseconds.}
  \item{freqres}{The desired frequency resolution. By default this is set to 1/4 the analysis bandwidth as determined by the actual (not effective) analysis window length.}
  \item{timestep}{If a negative value is given, this many equally-spaced time steps are calculated. If a positive number is given, this determines how far the window will be moved for each adjacent analysis, in milliseconds.}
  \item{preemphasisf}{Preemphasis of 6 dB per octave is added to frequencies above the specified frequency. For no preemphasis, set to a frequency higher than the sampling frequency.}
  \item{maxfreq}{the maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default.}
  \item{colors}{If TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.}
  \item{dynamicrange}{Values greater than this many dB below the maximum will be displayed in the same color.}
  \item{nlevels}{The number of divisions to be used for the z-axis of the spectrogram. By default it is set equal to the dynamic range, meaning that a single color represents 1 dB on the z-axis.}
  \item{maintitle}{A string indicating the spectrogram title if one is desired.}
  \item{show}{If FALSE, no spectrogram is plotted. This is useful if the user would like to perform an action on an existing spectrogram plot without having to redraw it.}
  \item{output}{If this is TRUE, a spectrogram object is returned containing information regarding the created spectrogram. This can then be used to re-plot the spectrogram with plot().}
  \item{window}{the window to be applied to the signal, applied by the windowfunc function in this package.}
  \item{windowparameter}{the parameter for the window to be applied to the signal, if appropriate.}
  \item{pause}{If TRUE, the console waits until enter is hit before plotting the spectrogram so that the dimensions can be determined. Resizing spectograms that have already been plotted can be slow, especially with R-Studio.}
}
\details{
This function is used to create and plot spectrograms. The user may specify all analysis parameters, in addition to the colors used to display the spectrogram. Resizing spectrograms after plotting may be slow (especially with RStudio, though I'm not sure why). If you find this is the case, take advantage of the PAUSE parameter which lets the user set their plotting window size before displaying the spectrogram. 

The function optionally returns a spectrogram object, for which the plot() function is defined. 
}
\value{
If output is set to TRUE, an object of class 'spectrogram', a list containing the elements:

\item{spectrogram}{a matrix containing the magnitude at each bin center. Frequencies differ across columns, while time varies between rows.}
\item{fs}{the sampling frequency of the sound from which the spectrogram was made.}
\item{windowlength}{the length of the analysis window used to create the spectrogram.}
\item{timestep}{the timestep (in milliseconds) used to create the spectrogram.}
\item{dynamicrange}{the dynamic range (in dB) of the spectrogram.}
\item{color}{the colors used to create the spectrogram. This value corresponds to the 'color' parameter set when calling spectrogram().}

}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
## uncoment and run. 
## data (sound)                 ## use the example 'sound' object provided
# sound = loadsound()        ## or run this line to use you own sound
# spectrogram (sound, pause = FALSE)
}

