\name{entropy.MillerMadow}
\alias{entropy.MillerMadow}
\title{Miller-Madow Entropy Estimator}

\usage{
entropy.MillerMadow(y, unit=c("log", "log2", "log10"))
}

\description{
  \code{entropy.MillerMadow} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} using the Miller-Madow correction
   to the empirical entropy).
}

\arguments{
  \item{y}{vector of counts.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
  The Miller-Madow entropy estimator (1955) is the bias-corrected empirical
  entropy estimate. 

  Note that the Miller-Madow estimator is not a plug-in estimator, hence there
  are no explicit underlying bin frequencies.
}
\value{
  \code{entropy.MillerMadow} returns an estimate of the Shannon entropy. 
}

\references{
  Miller, G.  1955. Note on the bias of information estimates. 
  Info. Theory Psychol. Prob. Methods  \bold{II-B}:95-100.
}
\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy.empirical}}}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# estimate entropy using Miller-Madow method
entropy.MillerMadow(y)

# compare to empirical estimate
entropy.empirical(y)
}
\keyword{univar}
