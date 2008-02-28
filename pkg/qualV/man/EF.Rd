\name{EF}
\alias{EF}
\title{Efficiency Factor as Suggested by Nash \& Sutcliffe}

\description{The efficiency factor is a dimensionless statistic which
  directly relates predictions to observed data.}

\usage{EF(o, p)}

\arguments{
  \item{o}{vector of observed values}
  \item{p}{vector of corresponding predicted values}
}

\value{\item{EF}{efficiency factor}}

\details{Two time series are compared. \code{'EF'} is an overall measure of
  similarity between fitted and observed values. Any model giving a negative value
  cannot be recommended, whereas values close to one indicate a
  'near-perfect' fit.}

\references{Nash, J. E. and Sutcliffe, J. V. (1970) River flow forecasting through conceptual models part I - A discussion of principles. Journal of Hydrology, 10, 282-290.

Mayer, D. G. and Butler, D. G. (1993) Statistical Validation. Ecological Modelling, 68, 21-32.}

\seealso{\code{\link{MAE}}, \code{\link{MSE}}, \code{\link{MAPE}}, \code{\link{GRI}}}

\examples{
# a constructed example
x <- seq(0, 2*pi, 0.1)
y <- 5 + sin(x)           # a process
o <- y + rnorm(x, sd=0.2) # observation with random error
p <- y + 0.1              # simulation with systematic bias

plot(x, o); lines(x, p)
EF(o, p)

# observed and measured data with non-matching time intervals
data(phyto)
obsb <- na.omit(obs[match(sim$t, obs$t), ])
simb <- sim[na.omit(match(obs$t, sim$t)), ]
EF(obsb$y, simb$y)
}

\keyword{misc}

