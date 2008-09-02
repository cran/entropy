\name{entropy.empirical}
\alias{entropy.empirical}
\alias{freqs.empirical}
\title{Empirical entropy estimator}

\usage{
entropy.empirical(y, unit=c("log", "log2", "log10"))
freqs.empirical(y)
}

\description{
  \code{entropy.empirical} estimates the Shannon entropy H 
   of the random variable Y from the corresponding observed counts \code{y}
   by plug-in of the empirical frequencies.

  \code{freqs.empirical} computes the empirical frequencies from counts \code{y}.
}

\arguments{
  \item{y}{vector of counts.}
  \item{unit}{the unit in which entropy is measured.}
}
\details{
   The empirical entropy estimator is a plug-in estimator: 
   in the definition of the Shannon entropy the
   bin probabilities are replaced by the respective empirical frequencies.

   The empirical entropy estimator is the maximum likelihood estimator.
   If there are many zero counts and the sample size is small
   it is very inefficient and also strongly biased.
}
\value{
  \code{entropy.empirical} returns an estimate of the Shannon entropy. 

  \code{freqs.empirical} returns the underlying frequencies.
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{entropy.MillerMadow}}, \code{\link{entropy.plugin}}.}


\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# empirical estimate of entropy
entropy.empirical(y)
}
\keyword{univar}
