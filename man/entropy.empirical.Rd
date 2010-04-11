\name{entropy.empirical}
\alias{entropy.empirical}
\alias{freqs.empirical}
\alias{mi.empirical}

\title{Empirical Entropy and Mutual Information Estimator}

\usage{
entropy.empirical(y, unit=c("log", "log2", "log10"))
mi.empirical = function(y, unit=c("log", "log2", "log10"))
freqs.empirical(y)
}

\description{
  \code{entropy.empirical} estimates the Shannon entropy H 
   of the random variable Y from the corresponding observed counts \code{y}
   by plug-in of the empirical frequencies.

  \code{mi.empirical} computes the empirical mutual information from counts \code{y}.

  \code{freqs.empirical} computes the empirical frequencies from counts \code{y}.
}

\arguments{
  \item{y}{vector or matrix of counts.}
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

  \code{mi.empirical} returns an estimate of the mutual information. 

  \code{freqs.empirical} returns the underlying frequencies.
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{entropy}}, \code{\link{entropy.MillerMadow}}, \code{\link{entropy.plugin}},
 \code{\link{mi.plugin}}.}


\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# empirical estimate of entropy
entropy.empirical(y)

# contigency table with counts for two discrete variables
y = rbind( c(1,2,3), c(6,5,4) )

# empirical estimate of mutual information
mi.empirical(y)
}
\keyword{univar}
