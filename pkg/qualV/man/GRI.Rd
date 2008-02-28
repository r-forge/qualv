\name{GRI}
\alias{GRI}
\title{A Geometric Reliability Index as Suggested by Leggett \& Williams}

\description{Given a set of predictions and a corresponding set of
  observations, the geometric validation index is a reliability index for
  the predictions.}

\usage{GRI(o, p)}

\arguments{
  \item{o}{vector of observed values}
  \item{p}{vector of corresponding predicted values}
}

\value{\item{GRI}{geometric reliability index}}

\details{One possible interpretation of 'GRI' is that the
  simulation is accurate within a multiplicative factor 'GRI', i.e. the
  observed values fall between 1/GRI and GRI times of the corresponding
  predicted values. Values close to one indicate a good match.}

\references{Leggett, L. R. and Williams, L. R. (1981) A reliability index for
  models. Ecological Modelling, 13, 303-312.}

\seealso{\code{\link{MAE}}, \code{\link{MSE}}, \code{\link{MAPE}}, \code{\link{EF}}}

\examples{
# a constructed example
x <- seq(0, 2*pi, 0.1)
y <- 5 + sin(x)           # a process
o <- y + rnorm(x, sd=0.2) # observation with random error
p <- y + 0.1              # simulation with systematic bias

plot(x, o); lines(x, p)
GRI(o, p)

# observed and measured data with non-matching time intervals
data(phyto)
obsb <- na.omit(obs[match(sim$t, obs$t), ])
simb <- sim[na.omit(match(obs$t, sim$t)), ]
GRI(obsb$y, simb$y)
}

\keyword{misc}

