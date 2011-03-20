\name{mi.plugin}
\alias{mi.plugin}
\title{Plug-In Mutual Information Estimator}

\usage{
mi.plugin(freqs2d, unit=c("log", "log2", "log10"))
}

\description{
  \code{mi.plugin} computes the mutual information of 
   of two discrete random variables from the specified bin frequencies.
}

\arguments{
  \item{freqs2d}{matrix of bin frequencies.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   The mutual information of two discrete random variables \eqn{X} and \eqn{Y}
   is defined as \eqn{MI = H(X) + H(Y) - H(X, Y)}.
}
\value{
  \code{mi.plugin} returns the mutual information. 
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{entropy.plugin}}.}


\examples{
# load entropy library 
library("entropy")

# joint distribution of two discrete variables
freqs2d = rbind( c(0.2, 0.1, 0.15), c(0, 0.3, 0.25) )  

# and corresponding mutual information
mi.plugin(freqs2d)
}
\keyword{univar}
