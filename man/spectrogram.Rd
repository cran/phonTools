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
spectrogram (sound, fs = 22050, windowlength = 5, freqres, timestep = 3, 
preemphasis = 50, maxfreq = 5000, gridlines = FALSE, colors = TRUE, 
dynamicrange = 40, nlevels = dynamicrange, maintitle = "", show = TRUE,
output = FALSE, chooseslices = 0, indicateslices = TRUE, zoom = FALSE, 
indicatezoom = TRUE, window = 'kaiser', windowparameter = 4, pause = TRUE) 
}
\arguments{
  \item{sound}{either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() function.}
  \item{fs}{the sampling frequency is Hz. This is set to 22050 Hz by default. If a sound object is passed this does not need to be specificed.}
  \item{windowlength}{the desired analysis windowlength in milliseconds.}
  \item{freqres}{the desired frequency resolution. By default this is set to 1/4 the analysis bandwidth as determined by the analysis window length.}
  \item{timestep}{how far a window will be moved for each adjacent analysis, in milliseconds. By default this is set to 3 ms.}
  \item{preemphasis}{preemphasis of 6 dB per octave is added to frequencies above the specified frequency. For no preemphasis, set to a frequency higher than the samplling frequency.}
  \item{maxfreq}{the maximum frequency to be displayed for the spectrogram with a maximum of fs/2. This is set to 5000 Hz by default.}
  \item{gridlines}{boolean. If TRUE gridlines are drawn where horizontal lines are drawn every 500 Hz and vertical lines are drawn every 25 ms.}
  \item{colors}{if TRUE a color spectrogram will be displayed, if FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.}
  \item{dynamicrange}{numeric. values greater than this many dB below the maximum will be displayed in the same color.}
  \item{nlevels}{the number of divisions to be used for the z-axis of the spectrogram. By default it is set equal to the dynamic range, meaning that a single color represents 1 dB on the z-axis.}
  \item{maintitle}{a string indicating the spectrogram title if one is desired.}
  \item{show}{if FALSE, no spectrogram is plotted. This is useful if the user would like to perform an action on an existing spectrogram plot without having to redraw it.}
  \item{output}{if this is TRUE, a spectrogram object is returned containing information regarding the created spectrogram. This can then be used to re-plot the spectrogram with plot().}
  \item{chooseslices}{numeric variable. If this is greater than 0, the user may indicate locations on the plotted spectrogram by clicking on it. This variable will indicate the number of selections to be selected. The time value and sample point corresponding to each selection is then returned in a dataframe. This information can be used with the spectralslice() function in this package to view spectra at those locations.}
  \item{indicateslices}{if TRUE, lines on the spectrogram will indicate where the user selected slices.}
  \item{zoom}{if TRUE, the user may indicate a subsection of the spectrogram by clicking on two points on a spectrogram plot. The spectrogram between these two points is then presented in a new plot.}
  \item{indicatezoom}{if TRUE, the points between the zoom was selected are displayed with vertical lines.}
  \item{window}{the window to be applied to the signal, applied by the windowfunc function in this package.}
  \item{windowparameter}{the parameter for the window to be applied to the signal, if appropriate.}
  \item{pause}{boolean, If TRUE, the console waits until enter is hit before plotting the spectrogram so that the dimensions can be determined. Resizing spectograms that have already been plotted can be slow, especially with R-Studio.}
}
\details{This function is used to create and plot spectrograms. The user may specify all analysis parameters, in addition to the colors used to display the spectrogram. Resizing spectrograms after plotting may be slow (especially with RStudio). If you find this is the case, take advantage of the PAUSE parameter which lets the user set their plotting window size before displaying the spectrogram.

The function optionally returns a spectrogram object, for which the plot() function is defined. 
}
\value{
If output is set to TRUE, an object of class 'Hotelling.test', a list containing the elements:

\item{spectrogram}{a matrix containing the magnitude at each bin center. Frequencies differ across columns, while time varies between rows.}
\item{fs}{the sampling frequency of the sound from which the spectrogram was made.}
\item{windowlength}{the length of the analysis window used to create the spectrogram.}
\item{timestep}{the timestep (in milliseconds) used to create the spectrogram.}
\item{dynamicrange}{the dynamic range (in dB) of the spectrogram.}
\item{color}{the colors used to create the spectrogram. This value corresponds to the 'color' parameter set when calling spectrogram().}

If chooseslices is greater than 0, a dataframe with the following columns is returnd instead:

\item{slice}{the number of each selected point. Points are sorted in terms of increasing time.}
\item{time}{the time, in milliseconds, corresponding to each selected point.}
\item{point}{the exact point in the vector representing the sound corresponding to each selected point.}
}
\author{
Santiago Barreda <sbarreda@ualberta.ca>
}
\examples{
data (sound)
spectrogram (sound, pause = FALSE)
}

