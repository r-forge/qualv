\name{timetrans}
\alias{transBeta}
\alias{transSimplex}
\alias{transBezier}
\title{Bijective Transformations of Time}

\description{Various function models for isoton bijective transformation of
  a time interval to itself.}

\usage{
transBeta(x, p, interval = c(0, 1), inv = FALSE,
  pmin = -3, pmax = 3, p0 = c(0, 0))
transSimplex(x, p, interval = c(0, 1), inv = FALSE,
  pmin = -2, pmax = 2, p0 = c(0, 0, 0, 0, 0))
transBezier(x, p, interval = c(0, 1), inv = FALSE,
  pmin = 0, pmax = 1, p0 = c(0.25, 0.25, 0.75, 0.75))
}
\arguments{
  \item{x}{a vector of values to be transformed,}
  \item{p}{the vector of parameters for the transformation,}
  \item{interval}{a vector of length 2 giving the minimum and maximum
    value in the transformation interval.}
  \item{inv}{a boolean, if true the inverse transform is computed.}
  \item{pmin}{a number or a vector giving the minimal useful value for
    the parameters. This information is not used by the function itself,
    but rather provides a meta information about the function used in
    \code{\link{timeTransME}}. The chosen values are quite restrictive
    to avoid stupid extreme transformation.}
  \item{pmax}{provides similar to \code{pmin} the upper useful bounds
    for the parameters.}
  \item{p0}{provides similar to \code{pmin} and \code{pmax} the
    parameterization for the identify transform.}
}
\value{
  The value is a vector of the same length as \code{x} providing the
  transformed values. 
}

\details{
  \describe{
    \item{\code{transBeta}}{
      The transformation provided is the distribution function of the
      Beta-Distribution with parameters \code{exp(p[1])} and
      \code{exp(p[2])} scaled to the given interval. This function is
      guaranteed to be strictly isotonic for every choice of p. p has
      length 2. The strength of the Beta transformation is the reasonable
      behavior for strong time deformations.
    }
    \item{\code{transSimplex}}{
      The transformation provided a simple linear interpolation. The
      interval is separated into equidistant time spans, which are
      transformed to non-equidistant length. The length of the new
      time spans is the proportional to exp(c(p, 0)). This function is
      guaranteed to be strictly isotonic for every choice of p. p can have
      any length. The strength of the Simplex transformation is the
      possibility to have totally different speeds at different times.
    }
    \item{\code{transBezier}}{
      The transformation is provided by a Bezier-Curve of order
      \code{length(p) / 2 + 1}. The first and last control point are given by
      \code{c(0, 0)} and \code{c(1, 1)} and the intermediate control points
      are given by \code{p[c(1, 2) + 2 * i - 2]}. This function is not guaranteed
      to be isotonic for \code{length(p) > 4}. However it seams useful. A
      major theoretical advantage is that this model is symmetric between
      image and coimage. The strength of the Bezier transformation is fine
      tuning of transformation.
    }
  }
}
\seealso{
  \code{\link{timeTransME}}
}
\examples{
t <- seq(0, 1, length.out = 101)
par(mfrow = c(3, 3))
plot(t, transBeta(t, c(0, 0)), type = "l")
plot(t, transBeta(t, c(0, 1)), type = "l")
plot(t, transBeta(t, c(-1,1)), type = "l")
plot(t, transSimplex(t, c(0)), type = "l")
plot(t, transSimplex(t, c(3, 2, 1)), type = "l")
plot(t, transSimplex(t, c(0, 2)), type = "l")
plot(t, transBezier(t, c(0, 1)), type = "l")
plot(t, transBezier(t, c(0, 1, 1, 0)), type = "l")
plot(t, transBezier(t, c(0.4, 0.6)), type = "l")
}
\keyword{misc}

