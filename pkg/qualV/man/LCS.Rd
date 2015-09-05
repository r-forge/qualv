\name{LCS}
\alias{LCS}
\title{Algorithm for the Longest Common Subsequence Problem}

\description{Determines the longest common subsequence of two strings.}
\usage{
LCS(a, b)
}

\arguments{
  \item{a}{vector (numeric or character), missing values are not accepted}
  \item{b}{vector (numeric or character), missing values are not accepted}
}

\value{
  \item{a}{vector \code{'a'}}
  \item{b}{vector \code{'b'}}
  \item{LLCS}{length of \code{LCS}}
  \item{LCS}{longest common subsequence}
  \item{QSI}{quality similarity index}
  \item{va}{one possible \code{LCS} of vector \code{'a'}}
  \item{vb}{one possible \code{LCS} of vector \code{'b'}}
}

\details{A longest common subsequence (\code{LCS}) is a common subsequence
  of two strings of maximum length. The \code{LCS} Problem consists of
  finding a \code{LCS} of two given strings and its length
  (\code{LLCS}). A qualitative similarity index \code{QSI} is
  computed by division of the \code{LLCS} over maximum length of
  \code{'a'} and \code{'b'}.
}

\references{
  Wagner, R. A. and Fischer, M. J. (1974) The String-to-String
    Correction Problem. Journal of the ACM, 21, 168-173.

  Paterson, M. and Dancik, V. (1994) Longest Common
    Subsequences. Mathematical Foundations of Computer Science, 841,
    127-142.

  Gusfield, D. (1997) Algorithms on Strings, Trees, and Sequences:
    Computer Science and Computational Biology. Cambridge University
    Press, England, ISBN 0-521-58519-8.
}

\note{\code{LCS} is now using a C version of the algorithm provided by
  Dominik Reusser.}

\examples{
# direct use
a <- c("b", "c", "a", "b", "c", "b")
b <- c("a", "b", "c", "c", "b")
print(LCS(a, b))

# a constructed example
x <- seq(0, 2 * pi, 0.1)  # time
y <- 5 + sin(x)           # a process
o <- y + rnorm(x, sd=0.2) # observation with random error
p <- y + 0.1              # simulation with systematic bias
plot(x, o); lines(x, p)

lcs <- LCS(f.slope(x, o), f.slope(x, p))  # too much noise
lcs$LLCS
lcs$QSI

os <- ksmooth(x, o, kernel = "normal", bandwidth = dpill(x, o), x.points = x)$y
lcs <- LCS(f.slope(x, os), f.slope(x, p))
lcs$LLCS
lcs$QSI


# observed and measured data with non-matching time intervals
data(phyto)
bbobs    <- dpill(obs$t, obs$y)
n        <- tail(obs$t, n = 1) - obs$t[1] + 1
obsdpill <- ksmooth(obs$t, obs$y, kernel = "normal", bandwidth = bbobs,
                    n.points = n)
obss     <- data.frame(t = obsdpill$x, y = obsdpill$y)
obss     <- obss[match(sim$t, obss$t),]
obs_f1   <- f.slope(obss$t, obss$y)
sim_f1   <- f.slope(sim$t, sim$y)
lcs      <- LCS(obs_f1, sim_f1)
lcs$QSI
}

\keyword{misc}

