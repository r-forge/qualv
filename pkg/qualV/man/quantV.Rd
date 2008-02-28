\name{quantV}
\alias{generalME}
\alias{MAE}
\alias{MAPE}
\alias{MSE}
\alias{RMSE}
\alias{CMAE}
\alias{CMSE}
\alias{RCMSE}
\alias{SMAE}
\alias{SMSE}
\alias{RSMSE}
\alias{MALE}
\alias{MSLE}
\alias{MAGE}
\alias{RMSLE}
\alias{RMSGE}
\alias{SMALE}
\alias{SMAGE}
\alias{SMSLE}
\alias{RSMSLE}
\alias{RSMSGE}
\alias{MAOE}
\alias{MSOE}
\alias{RMSOE}
\title{Quantitative Validation Methods}
\description{Different methods for calculating the difference between two vectors.}
\usage{
generalME(o, p,
          ignore   = c("raw", "centered", "scaled", "ordered"),
          geometry = c("real", "logarithmic", "geometric", "ordinal"),
          measure  = c("mad", "var", "sd"),
          type     = c("dissimilarity", "normalized", "similarity",
                       "reference", "formula", "name", "function"),
                       method = NULL)
   MAE(o, p, type = "dissimilarity")
  MAPE(o, p, type = "dissimilarity")
   MSE(o, p, type = "dissimilarity")

  RMSE(o, p, type = "dissimilarity")
  CMAE(o, p, type = "dissimilarity")
  CMSE(o, p, type = "dissimilarity")
 RCMSE(o, p, type = "dissimilarity")
  SMAE(o, p, type = "dissimilarity")
  SMSE(o, p, type = "dissimilarity")
 RSMSE(o, p, type = "dissimilarity")
  MALE(o, p, type = "dissimilarity")
  MAGE(o, p, type = "dissimilarity")
 RMSLE(o, p, type = "dissimilarity")
 RMSGE(o, p, type = "dissimilarity")

 SMALE(o, p, type = "dissimilarity")
 SMAGE(o, p, type = "dissimilarity")
 SMSLE(o, p, type = "dissimilarity")

RSMSLE(o, p, type = "dissimilarity")
RSMSGE(o, p, type = "dissimilarity")

  MAOE(o, p, type = "dissimilarity")
  MSOE(o, p, type = "dissimilarity")
 RMSOE(o, p, type = "dissimilarity")
}
\arguments{
  \item{o}{vector of observed values}
  \item{p}{vector of corresponding predicted values}
  \item{type}{one of \code{c("dissimilarity", "normalized",
      "similarity",\ "reference", "formula")}, for the dissimilarity
    measure, the normalized dissimilarity measure, the similarity
    measure, or the formula for the normalized measure. For
    \code{generalME} it is additionally possible to specify
    \code{"function"} for getting the corresponding function and
    \code{"name"} for getting the name of the function.
  }
  \item{ignore}{specifies which aspects should be ignored: "raw"
    compares original values, "centered" removes differences in mean,
    "scaled" ignores scaling, "ordered" indicates the use of the ordinal
    geometry only.}
  \item{geometry}{indicating the geometry to be used for the data and
    the output, "real" corresponds to arithmetic differences and means,
    "logarithmic" to handling relative data on a logarithmic scale,
    "geometric" to geometric means and differences and "ordinal" to a
    pure ordinal treatment.}
  \item{measure}{indicates how distances should be measured: as mean
    absolute distances like in MAD, as squared distances like in a
    variance, or as the root of mean squared distances like in sd.}
   \item{method}{optionally the function to be used can specified
     directly as a function or as a string.}
}
\value{
\item{generalME}{selects the best deviance measure according to the
description given in the parameters. It has the two additional
possibilities of name and function in the type parameter.}
\item{MAE}{mean absolute error \eqn{\frac1n}{1/n}}
\item{MAPE}{mean absolute percentage error}
\item{MSE}{mean squared error}
\item{RMSE}{root mean squared error}
\item{CMAE}{centered mean absolute error}
\item{CMSE}{centered mean squared error}
\item{RCMSE}{root centered mean squared error}
\item{SMAE}{scaled mean absolute error}
\item{SMSE}{scaled mean squared error}
\item{RSMSE}{root scaled mean squared error}
\item{MALE}{mean absolute logarithmic error}
\item{MAGE}{mean absolute geometric error}
\item{MSLE}{mean squared logarithmic error}
\item{MSGE}{mean squared geometric error}
\item{RMSLE}{root mean squared logarithmic error}
\item{SMALE}{scaled mean absolute logarithmic error}
\item{SMAGE}{scaled mean absolute relative error}
\item{SMSLE}{scaled mean squared logarithmic error}
\item{RSMSLE}{root scaled mean squared logarithmic error}
\item{RSMSGE}{root scaled mean squared geometric error}
\item{MAOE}{mean absolute ordinal error }
\item{MSOE}{mean squared ordinal error}
\item{RMSOE}{root mean squared ordinal error}
}
\details{
  These comparison criteria are designed for a semiquantitative
  comparison of observed values \code{o} with predicted values
  \code{p} to validate the performance of the prediction.
\cr
The general naming convention follows the grammar scheme
\cr
\code{[R][C|S]M[S|A][L|G|O]E}
\cr
corresponding to
\code{[Root] [Centered | Scaled] Mean [Squared | Absolute]}
\cr
\code{[Logarithmic, Geometric, Ordinal] Error}
\item{Root}{is used together with squared errors to indicate, that a
  root is applied to the mean.}
\item{Centered}{indicates that an additive constant is allowed.}
\item{Scaled}{indicates that a scaling of the predictive sequence is
  allowed. Scaled implies centered for real scale.}
\item{Squared}{indicates that squared error is used.} 
\item{Absolute}{indicates that absolute error is used.}
\item{Logarithmic}{indicates that the error is calculated based on the
  logarithms of the values. This is useful for data on a relative
  scale.}
\item{Geometric}{indicates that the result is to be understood as a
  factor, similar to a geometric mean.}
\item{Ordinal}{indicates that only the order of the observations is
  taken into account by analyzing the data by ranks scaled to the
  interval [0, 1].}

The mean errors for squared error measures are
based on the number of degrees of freedom of the residuals.

}

\references{
  Mayer, D. G. and Butler, D. G. (1993) Statistical
  Validation. Ecological Modelling, 68, 21-32.
  
  Jachner, S., K.G. v.d. Boogaart, T. Petzoldt (2007) Statistical methods
  for the qualitative assessment of dynamic models with time delay (R
  package qualV), in preparation
}

\seealso{\code{\link{EF}}, \code{\link{GRI}}, \code{\link{compareME}}}
\examples{
data(phyto)
obsb <- na.omit(obs[match(sim$t, obs$t), ])
simb <- sim[na.omit(match(obs$t, sim$t)), ]
o <- obsb$y
p <- simb$y

generalME(o, p, ignore = "raw", geometry = "real")

   MAE(o, p)
  MAPE(o, p)
   MSE(o, p)
  RMSE(o, p)
  CMAE(o, p)
  CMSE(o, p)
 RCMSE(o, p)
  SMAE(o, p)
  SMSE(o, p)
 RSMSE(o, p)
  MALE(o, p)
  MAGE(o, p)
 RMSLE(o, p)
 RMSGE(o, p)

 SMALE(o, p)
 SMAGE(o, p)
 SMSLE(o, p)

RSMSLE(o, p)
RSMSGE(o, p)

  MAOE(o, p)
  MSOE(o, p)
 RMSOE(o, p)
   MAE(o, p)
  MAPE(o, p)


   MSE(o, p, type = "s")
  RMSE(o, p, type = "s")
  CMAE(o, p, type = "s")
  CMSE(o, p, type = "s")
 RCMSE(o, p, type = "s")
  SMAE(o, p, type = "s")
  SMSE(o, p, type = "s")
 RSMSE(o, p, type = "s")
  MALE(o, p, type = "s")
  MAGE(o, p, type = "s")
 RMSLE(o, p, type = "s")
 RMSGE(o, p, type = "s")

 SMALE(o, p, type = "s")
 SMAGE(o, p, type = "s")
 SMSLE(o, p, type = "s")

RSMSLE(o, p, type = "s")
RSMSGE(o, p, type = "s")

  MAOE(o, p, type = "s")
  MSOE(o, p, type = "s")
 RMSOE(o, p, type = "s")
}
\keyword{misc}
