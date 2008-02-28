\name{qvalLCS}
\alias{qvalLCS}
\alias{print.qvalLCS}
\alias{plot.qvalLCS}
\alias{summary.qvalLCS}
\title{Qualitative Validation by Means of Interval Sequences and LCS}

\description{Dividing time series into interval sequences of qualitative
  features and determining the similarity of the qualitative behavior by
  means of the length of \code{LCS}.}

\usage{
qvalLCS(o, p,
        o.t     = seq(0, 1, length.out = length(o)),
        p.t     = seq(0, 1, length.out = length(p)),
        smooth  = c("none", "both", "obs", "sim"),
        feature = c("f.slope", "f.curve", "f.steep", "f.level"))
\method{print}{qvalLCS}(x, ...)
\method{plot}{qvalLCS}(x, y = NULL, ..., xlim = range(c(x$obs$x, x$sim$x)),
ylim = range(c(x$obs$y, x$sim$y)), xlab = "time", ylab = " ",
col.obs = "black", col.pred = "red",
plot.title = paste("LLCS =", x$lcs$LLCS, ", QSI =", x$lcs$QSI),
legend = TRUE)
\method{summary}{qvalLCS}(object, ...)
}

\arguments{
  \item{o}{vector of observed values}
  \item{p}{vector of predicted values}
  \item{o.t}{vector of observation times}
  \item{p.t}{vector of times for predicted values}
  \item{smooth}{character string to decide if values should be smoothed
before validation, default no smoothing \code{"none"} is set,
\code{"both"} observed and predicted values will be smoothed,
\code{"obs"} only observed, and \code{"sim"} only simulated values will
be smoothed.}
   \item{feature}{one of \code{"f.slope", "f.curve", "f.steep",
       "f.level"} as defined in \code{\link{features}} to divide the
     time series into interval sequences of these feature. As default
     the first derivative \code{"f.slope"} is used.}
   \item{x}{a result from a call of \code{qvalLCS}}
   \item{y}{y unused}
   \item{\dots}{further parameters to be past to
      \code{\link[base]{plot}}}
   \item{xlim}{the size of the plot in x-direction}
   \item{ylim}{the size of the plot in y-direction}
   \item{xlab}{the label of the x-axis of the plot}
   \item{ylab}{the label of the y-axis of the plot}
   \item{col.obs}{color to plot the observations}
   \item{col.pred}{color to plot the predictions}
   \item{plot.title}{title for the plot}
   \item{legend}{tegend for the plot}
   \item{object}{a result from a call of \code{qvalLCS}}
}

\value{
The result is an object of type \code{qvalLCS} with the following entries:
\item{smooth}{smoothing parameter}
\item{feature}{feature parameter}
\item{o}{xy-table of observed values}
\item{p}{xy-table of predicted values}
\item{obs}{xy-table of (smoothed) observed values}
\item{sim}{xy-table of (smoothed) simulated values}
\item{obsf}{interval sequence of observation according to selected \code{\link{features}}}
\item{simf}{interval sequence of simulation according to selected \code{\link{features}}}
\item{lcs}{output of \code{\link{LCS}} function} 
\item{obs.lcs}{one \code{LCS} of observation}
\item{sim.lcs}{one \code{LCS} of simulation}
}

\details{Common quantitative deviance measures underestimate the
  similarity of patterns if there are shifts in time between measurement
  and simulation. These methods also assume compareable values in each
  time series of the whole time sequence. To compare values independent
  of time the qualitative behavior of the time series could be
  analyzed. Here the time series are divided into interval sequences
  of their local shape. The comparison occurs on the basis of these
  segments and not with the original time series. Here shifts in time
  are possible, i.e. missing or additional segments are acceptable
  without losing similarity. The dynamic programming algorithm of
  the longest common subsequence \code{\link{LCS}} is used to determine
  \code{QSI} as index of similarity of the patterns.
  \cr
  If selected the data are smoothed using a weighted average and a
  Gaussian curve as kernel. The bandwidth is automatically selected
  based on the plug-in methodology (\code{dpill}, see package
  \pkg{KernSmooth} for more details).

\item{print.qvalLCS}{prints only the requested value, without
  additional information.}
\item{summary.qvalLCS}{prints all the additional information.}
\item{plot.qvalLCS}{shows a picture visualizing a \code{LCS}.}
}

\references{Agrawal R., K. Lin., H. Sawhney and K. Shim (1995).
  Fast similarity search in the presence of noise, scaling, and
  translation in time-series databases. In VLDB '95: Proceedings
  of the 21. International Conference on Very Large Data Bases,
  pp. 490-501. Morgan Kaufmann Publishers Inc. ISBN 1-55860-379-4.
  
  Cuberos F., J. Ortega, R. Gasca, M. Toro and J. Torres (2002).
  Qualitative comparison of temporal series - QSI. Topics in
  Artificial Intelligence. Lecture Notes in Artificial
  Intelligence, 2504, 75-87.

  Jachner, S., K.G. v.d. Boogaart, T. Petzoldt (2007) Statistical methods
  for the qualitative assessment of dynamic models with time delay (R
  package qualV), in preparation  
}

\seealso{
  \code{\link{LCS}}, \code{\link{features}}
}

\examples{
# a constructed example
x <- seq(0, 2*pi, 0.1)
y <- 5 + sin(x)           # a process
o <- y + rnorm(x, sd=0.2) # observation with random error
p <- y + 0.1              # simulation with systematic bias

qvalLCS(o, p)
qvalLCS(o, p, smooth="both", feature="f.curve")

qv <- qvalLCS(o, p, smooth = "obs")
print(qv)
plot(qv, ylim=c(3, 8))

# observed and measured data with non-matching time steps
data(phyto)
qvlcs <- qvalLCS(obs$y, sim$y, obs$t, sim$t, smooth = "obs")

basedate <- as.Date("1960/1/1")
qvlcs$o$x   <- qvlcs$o$x + basedate
qvlcs$obs$x <- qvlcs$obs$x + basedate
qvlcs$sim$x <- qvlcs$sim$x + basedate
qvlcs$obs.lcs$x <- qvlcs$obs.lcs$x + basedate
qvlcs$sim.lcs$x <- qvlcs$sim.lcs$x + basedate

plot.qvalLCS(qvlcs)
summary(qvlcs)
}

\keyword{misc}
