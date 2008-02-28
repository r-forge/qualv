\name{features}
\alias{features}
\alias{f.slope}
\alias{f.curve}
\alias{f.steep}
\alias{f.level}
\title{Qualitative Features of Time Series}

\description{A time series is characterised by a sequence of characters,
  indicating features of the time series itself, of its first or second
  derivative, steepness or level of values.}

\usage{
f.slope(x, y, f = 0.1)
f.curve(x, y, f = 0.1)
f.steep(x, y, f1 = 1, f2 = 0.1)
f.level(y, high = 0.8, low = 0.2)
}

\arguments{
  \item{x}{vector of time}
  \item{y}{input y values}
  \item{f}{factor defining the limit for constant (\code{f.slope}) or linear (\code{f.curve}) sequences}
  \item{f1}{factor for the upper bound of steepness}
  \item{f2}{factor for the lower bound of steepness}
  \item{high}{lower limit of high values}
  \item{low}{upper limit of low values}
}
\value{\item{v}{interval sequence}}

\details{For the first derivative the segment between two values is
  characterised by increasing ('A'), decreasing ('B') or constant ('C') and for
  the second by convex ('K'), concave ('I') or linear ('J'). For the property of
  the first derivative the segment between two values is characterised
  by very steep ('S'), steep ('T') or not steep ('U') or the values are divided
  into high ('H'), low ('L') or values in between ('M'). For
  the last two cases the values will be standardised to ignore quantitative
  deviations.}

\seealso{\code{\link{LCS}}, \code{\link{qvalLCS}}}

\examples{
data(phyto)
bbobs    <- dpill(obs$t, obs$y)
n        <- tail(obs$t, n = 1) - obs$t[1] + 1
obsdpill <- ksmooth(obs$t, obs$y, kernel = "normal", bandwidth = bbobs,
            n.points = n)
obss     <- data.frame(t = obsdpill$x, y = obsdpill$y)
obss     <- obss[match(sim$t, obss$t), ]
f.slope(obss$t, obss$y)
f.curve(obss$t, obss$y)
f.steep(obss$t, obss$y, f1 = 30, f2 = 10)
f.level(obss$y)
}

\keyword{misc}
