\name{compareME}
\alias{compareME}
\alias{print.compareME}
\alias{summary.compareME}

\title{Compute Several Deviance Measures for Comparison}

\description{
  Various deviance measures are computed allowing the user to find the
  aspects in which two time series differ.
}\usage{
compareME(o, p,
          o.t      = seq(0, 1, length.out = length(o)),
          p.t      = seq(0, 1, length.out = length(p)),
          ignore   = c("raw", "centered", "scaled", "ordered"),
          geometry = c("real", "logarithmic", "geometric", "ordinal"),
          measure  = c("mad", "var", "sd"),
          type     = "normalized",
          time     = "fixed", ..., col.vars=c("time", "ignore")
         )
\method{print}{compareME}(x, ..., digits = 3)
\method{summary}{compareME}(object, ...)
}
\arguments{
  \item{o}{vector of observed values,}
  \item{p}{vector of predicted values,}
  \item{o.t}{vector of observation times,}
  \item{p.t}{vector of times for predicted values,}
  \item{ignore}{a subset of \code{c("raw", "centered", "scaled",
      "ordered")} as defined in \code{\link{generalME}} to specify
    the aspects of the data to be ignored,}
  \item{geometry}{a subset of \code{c("real", "logarithmic", "geometric",
      "ordinal")} as defined in \code{\link{generalME}} to specify the
    geometry of the observed data,}
  \item{measure}{a subset of \code{c("mad", "var", "sd")} to specify the
    type of error to be measured,}
  \item{type}{a subset of \code{c("dissimilarity", "normalized",
      "similarity", "reference")} as defined in \code{\link{generalME}}
    to specify the type of deviance measure to be used,}
  \item{time}{a subset of \code{c("fixed", "transform")}, indicates wether
    the time should actually be transformed. If this argument and the
    time arguments are missing the comparison is based on values only
    without time matching.}
  \item{...}{further arguments passed to \code{\link{timeTransME}},}
  \item{col.vars}{a subset of \code{c("ignore", "geometry", "measure",
      "time")} to be displayed in the columns of the resulting \code{ftable},}
  \item{digits}{number of significant digits displayed,}
  \item{x, object}{objects of class \code{compareME}.}
  
}

\value{
  The result is a list of \code{ftable}s containing the deviance
  measures of all requested combinations of parameters. The list is done
  over the different types of measures requested. 
}
\details{
  The function provides a simple standard interface to get a first idea
  on the similarities and dissimilarities of two time series spanning the
  same time interval. The \code{print} and \code{summary} methods extract
  the relevant information, rounded to an optional number of significant 
  digits.
}
\seealso{
  \code{\link{timeTransME}},
  \code{\link{generalME}}
}
\examples{
# a constructed example
x <- seq(0, 2*pi, 0.1)
y <- 5 + sin(x)             # a process
o <- y + rnorm(x, sd = 0.2) # observation with random error
p <- y + 0.1                # simulation with systematic bias

os <- ksmooth(x, o, kernel = "normal",
  bandwidth = dpill(x, o), x.points = x)$y
plot(x, o); lines(x, p); lines(x, os, col = "red")

compareME(o, p)
compareME(os, p)

# observed and measured data with non-matching time intervals
data(phyto)
compareME(obs$y, sim$y, obs$t, sim$t, time = "fixed")
tt <- timeTransME(obs$y, sim$y, obs$t, sim$t, ME = SMSLE, trials = 5)
compareME(tt$yo, tt$yp)

# show names of deviance measures
compareME(type = "name")
}
\keyword{misc}

