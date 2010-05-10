\name{phyto}
\docType{data}
\alias{phyto}
\alias{obs}
\alias{sim}
\title{Observed and Predicted Data of Phytoplankton}
\description{
  The data contain the day since 1.1.1994 and observed/predicted biovolumes
  of phytoplankton.
}
\usage{
obs
sim
}
\format{Two data frames of two variables with the following components:

\describe{
  \item{\code{obs:}}{A data frame of observed phytoplankton concentration in
    Bautzen reservoir 1994 (TU Dresden, Institute of Hydrobiology,
    workgroup limnology) with the elements:
       \describe{
          \item{\code{t:}}{time code}
          \item{\code{y:}}{observed biovolume (mg/L)}
       }}  
  \item{\code{sim:}}{A data frame of predicted phytoplankton concentration
    in Bautzen reservoir 1994 (TU Dresden, Institute of Hydrobiology,
    workgroup Limnology) with the elements:
       \describe{
          \item{\code{t:}}{time code}
          \item{\code{y:}}{predicted biovolume (mg/L)}
       }}
} 
}

\keyword{datasets}
