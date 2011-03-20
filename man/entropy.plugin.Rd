\name{entropy.plugin}
\alias{entropy.plugin}
\title{Plug-In Entropy Estimator}

\usage{
entropy.plugin(freqs, unit=c("log", "log2", "log10"))
}

\description{
  \code{entropy.plugin} computes the Shannon entropy H 
   of a discrete random variable from the specified bin frequencies.
}

\arguments{
  \item{freqs}{bin frequencies.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   The Shannon entropy of a discrete random variable is 
   defined as \eqn{H = -\sum p(x_i) \log( p(x_i) )}, where \eqn{p(x_i)} are the bin
   probabilities.
}
\value{
  \code{entropy.plugin} returns the Shannon entropy. 
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{entropy}}, \code{\link{entropy.empirical}}, \code{\link{mi.shrink}}.}


\examples{
# load entropy library 
library("entropy")

# some frequencies
freqs = c(0.2, 0.1, 0.15, 0.05, 0, 0.3, 0.2)  

# and corresponding entropy
entropy.plugin(freqs)
}
\keyword{univar}
