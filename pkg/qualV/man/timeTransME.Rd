\name{timeTransME}
\alias{timeTransME}
\alias{timetransme}
\alias{print.timeTransME}
\alias{plot.timeTransME}
\alias{summary.timeTransME}
\title{Transformation of Time to Match Two Time Series}

\description{Transforming the time of predicted values by means of a monotonic mapping.}

\usage{
timeTransME(o, p,
            o.t      = seq(0, 1, length.out = length(o)),
            p.t      = seq(0, 1, length.out = length(p)),
            ignore   = "scaled",
            geometry = "real",
            measure  = "mad",
            type     = c("dissimilarity", "normalized",
                         "similarity", "reference"),
            interval = range(c(o.t, p.t)), 
            time     = c("transformed", "fixed"),
            trans    = transBeta,
            p0       = eval(formals(trans)$p0),
            pmin     = eval(formals(trans)$pmin, list(p = p0)),
            pmax     = eval(formals(trans)$pmax, list(p = p0)),
            timeMEFactor = 0,
            timeME       = MAE,
            timeMEtype   = "normalized",
            timeScale    = 1,
            ME     = generalME(o, p, ignore, geometry, measure,
                               type = "function"),
            MEtype = c("dissimilarity", "normalized"),
            trials = 100,
            debug  = FALSE)
\method{print}{timeTransME}(x, ..., digits = 3)
\method{summary}{timeTransME}(object, ...)
\method{plot}{timeTransME}(x, y = NULL, ..., col.obs = "black", col.pred = "green",
     col.map = "red", sub = x$call, xlab = "t",
     xlim = range(x$x), ylim = range(c(0, x$yo, x$yp)))
}
\arguments{
  \item{x}{a result from a call to \code{timeTransME}}
  \item{object}{a result from a call to \code{timeTransME}}
  \item{o}{vector of observed values}
  \item{p}{vector of predicted values}
  \item{o.t}{vector of observation times}
  \item{p.t}{vector of times for predicted values}
  \item{ignore}{one of \code{"raw", "centered", "scaled", "ordered"} as defined
    in
    \cr
    \code{\link{generalME}} to specify the aspects of the data to be
ignored.}
  \item{geometry}{one of \code{"real", "logarithmic", "geometric", "ordinal"}
    as defined in \code{\link{generalME}} to specify the geometry of
    the observed data.}
  \item{measure}{one of \code{"mad", "sd", "var"} to specify the type of
    error to be measured.}
  \item{type}{one of
    \code{"dissimilarity", "normalized", "similarity", "reference"} as
    defined in \code{\link{generalME}} to
    specify the type of deviance measure to be used.}
  \item{interval}{a vector with two entries giving start and end time of
        the experiment.}
  \item{time}{indicates wether the time should actually be
              transformed. LCS is currently not implemented. Use the LCS method
             directly.}
  \item{trans}{the model function for the time transformation. See
              \code{\link{transBezier}} for possible alternatives.}
  \item{p0}{the identity parameters for the time-transformation. A non
    identity value can be given to force specific parameters for the
    transformation with \code{time = "fixed"}.}
  \item{pmin}{number or vector providing the minimal allowed values for
    the parameters of the transformation}
  \item{pmax}{number or vector providing the minimal allowed values for
    the parameters of the transformation}
  
  \item{timeME}{The \code{timeTransME} minimizes a weighted sum of the
    deformation of the time scale and of the data values
    according to \cr
    \code{totalME =} min \code{ME(o(x), p(trans(x, timep)), MEtype) +
      timeMEFactor * timeME(x * timeScale,
      trans(x, timep) * timeScale, timeMEtype)} over p
    \cr
    for \code{x = c(ot, trans(pt, timep, inv = TRUE))}.
    \cr
    timeME specifies the function to be used to quantify the temporal deformation.}
  \item{timeMEtype}{the type of deviance measure (``dissimilarity'' or
                    ``normalized'') to be used for \code{timeME}.}
  \item{timeMEFactor}{a real value specifying the weighting of the time
     deformation against the value deformation. A value of 0 avoids penalty
     for time deformation.}
  \item{timeScale}{a scaling applied to the time values before
    \code{timeME} is applied. This can be used to change the units of
    measurement for the time.}
  \item{ME}{the deviance function to be used for the data. See \code{\link{MSE}}
           for alternatives.
         }
  \item{MEtype}{the type of Mean Error to be used in the
calculations. This is not the type of Measure to be reported.}
  \item{trials}{The number of random starting values that should be used
during the optimization of the time transformation. The optimization of
the time transformation is a very critical task of this procedure and it
had been shown by practical tests that a single local optimization
typically fails to find the globally best fit. Depending on the number
of parameters a value between 100 and 10000 seems reasonable for this
parameter.}
  \item{debug}{a logical. If true some diagnostic information for the
         optimization step is printed.
      }
   \item{\dots}{further parameters to be passed to
      \code{\link[base]{plot}}}
   \item{col.obs}{color to plot the observations}
   \item{col.pred}{color to plot the predictions}
   \item{col.map}{color to plot the mapped predictions}
   \item{sub}{the sub-headline of the plot}
   \item{xlab}{the label of the x-axis of the plot}
   \item{xlim}{the size of the plot in x-direction}
   \item{ylim}{the size of the plot in y-direction}
   \item{y}{y unused}
   \item{digits}{number of significant digits displayed}
}

\value{
The result is an object of type \code{timeTransME} with the following entries:
\item{totalME}{the requested measure with specified type}
\item{criterium}{the "dissimilarity" measure, which was calculated as a
  minimum of
  \cr
  \code{ME(o(x), p(trans(x, timep)), MEtype) + timeMEFactor *\
    timeME(x * timeScale, trans(x, timep) * timeScale,\
    timeMEtype)}.}
\item{reference}{the reference value of this criterium achieved without
  time deformation and full dissimilarity.}
\item{call}{the call used to generate this deviance.}
\item{x}{the times at which the series were compared from the
  perspective of the observations.}
\item{xp}{the transformed times at which the series were compared from
  the perspective of the prediction.}
\item{yo}{the interpolated values of the observations at times \code{x}.}
\item{yp}{the interpolated values of the time transformed predictions
  at times \code{x}.}
\item{timeME}{the deviance of the time transformation:
  \cr
  \code{timeME(x, trans(x, ME), timeMEtype))}.}
\item{timeMEref}{the reference value of timeME}
\item{timeMEFactor}{the factor to be used for timeME in the weighting
  with respect to \code{ME}.}
\item{timeScale}{the scaling to time to account for an other unit.}
\item{p}{the parameter of trans minimizing the criterium.}
\item{interval}{the interval of time under consideration}
\item{trans}{the transformation function used for the time.}
If there is actually a minimisation performed there is the additional
list element:
\item{optim}{contains informations about the convergence of the
  optimization procedure and a list of secondary minima found.}
}

\details{Common quantitative deviance measures underestimate the
  similarity of patterns if there are shifts in time between measurement
  and simulation. An alternative to measure model performance
  independent of shifts in time is to transform the time of the
  simulation, i.e. to run the time faster or slower, and to compare the
  performance before and after the transformation. The applied
  transformation function must be monotonic. \code{timeTransME}
  minimizes the joint criterium
\code{ME(o(x), p(trans(x, timep)), MEtype) +
timeMEFactor * timeME(x * timeScale, trans(x, timep) * timeScale, timeMEtype)} to
find a best fitting time transformation.

\item{\code{print.timeTransME}}{prints only the requested value, without
  additional information.}
\item{\code{summary.timeTransME}}{prints all the additional information.}
\item{\code{plot.timeTransME}}{shows a picture visualising the fit of the
  transformed dataset. This can be used as a diagnostic.}
}

\seealso{
  \code{\link{transBeta}},
  \code{\link{transBezier}}
}

\note{
The deviance calculated by \code{timeTransME(..., time = "fixed")} and the
corresponding deviance measure are different because the timeTransME
does an interpolation and compares time sequences at different spacing,
while a simple deviance measure compares values only.
\cr
The CPU usage of the calculation of the
minimum, when \code{trans = "transform"} is very high, because the
optimization is done a hundred times with random starting values for
the parameters. This is necessary since with the given objective the
general purpose optimizers often run into local minima and/or do not
convergence. The number of iterations can be controlled with the
parameter \code{trials}. Setting \code{debug = TRUE} gives an impression
how long it takes to find an improved optimum.
}

\examples{
set.seed(123)
# a constructed example
x <- seq(0, 2*pi, length=10)
o <- 5 + sin(x) + rnorm(x, sd=0.2) # observation with random error
p <- 5 + sin(x-1)                  # simulation with time shift

# timeTransME(o, p) # reasonably accurate but takes very long!
# timeTransME(o, p, trials=5, debug=TRUE)

ttbeta <- timeTransME(o, p, trials=5)
plot(ttbeta)
\dontrun{
ttsimplex <- timeTransME(o, p, trans = transSimplex, trials=5)
plot(ttsimplex)

ttbezier <- timeTransME(o, p, trans = transBezier, trials=5)
plot(ttbezier)
}

# observed and measured data with non-matching time intervals
data(phyto)
bbobs    <- dpill(obs$t, obs$y)
n        <- tail(obs$t, n = 1) - obs$t[1] + 1
obsdpill <- ksmooth(obs$t, obs$y, kernel = "normal", bandwidth = bbobs,
            n.points = n)
obss     <- data.frame(t = obsdpill$x, y = obsdpill$y)
obss     <- obss[match(sim$t, obss$t), ]
obss     <- na.omit(obss)
tt       <- timeTransME(obss$y, sim$y, obss$t, sim$t, ME = SMSE,
            timeMEFactor = 0, time = "transform", type = "n", trials = 5)
round(tt$totalME, digits = 3)

basedate <- as.Date("1960/1/1")
plot(basedate + sim$t, sim$y, type="l", ylim = c(min(obs$y, sim$y),
  max(obs$y, sim$y)), xlab = "time", ylab = "Phytoplankton (mg/L)",
  col = 2, font = 2, lwd = 2, cex.lab = 1.2, las = 1)
lines(basedate + obss$t, obss$y, lwd = 2)
points(basedate + obs$t, obs$y, lwd = 2)
lines(basedate + tt$x, tt$yp, lwd = 2, col = 2, lty = 2)
legend(basedate + 12600, 50, c("measurement", "smoothed measurement",
"simulation", "transformed simulation"), lty = c(0, 1, 1, 2),
pch = c(1, NA, NA, NA), lwd = 2, col = c(1, 1, 2, 2))

tt1 <- timeTransME(obs$y, sim$y, obs$t, sim$t, ME = SMSLE, type = "n",
  time = "fixed")
tt1
plot(tt1)
summary(tt1)

\dontrun{
tt2 <- timeTransME(obs$y, sim$y, obs$t, sim$t, ME = SMSLE, type = "n",
  time = "trans", debug = TRUE)
tt2
plot(tt2)
summary(tt2)
tt3 <- timeTransME(obs$y, sim$y, obs$t, sim$t, ME = SMSLE, type = "n",
  time = "trans", trans = transBezier)
tt3
plot(tt3)
summary(tt3)
tt4 <- timeTransME(obs$y, sim$y, obs$t, sim$t, ME = MSOE, type = "n",
  time = "trans", trans = transBezier, debug = TRUE)
tt4
plot(tt4)
summary(tt4)
}

}
\keyword{misc}

