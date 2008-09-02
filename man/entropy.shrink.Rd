\name{entropy.shrink}
\alias{entropy.shrink}
\alias{freqs.shrink}
\title{Shrinkage entropy estimator}

\usage{
entropy.shrink(y, unit=c("log", "log2", "log10"), target=1/length(y), verbose=TRUE)
freqs.shrink(y, target=1/length(y), verbose=TRUE)
}

\description{
  \code{entropy.shrink} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} by plug-in of shrinkage estimate
   of the bin frequencies.
  
  \code{freq.shrink} estimates the bin frequencies from the counts \code{y}
  using a James-Stein-type shrinkage estimator.  The default shrinkage target is the uniform,
  unless otherwise specified.
}

\arguments{
  \item{y}{vector of counts.}
  \item{unit}{the unit in which entropy is measured.}
  \item{target}{the shrinkage target for the frequencies (default: uniform distribution).}
  \item{verbose}{report shrinkage intensity and equivalent pseudocount.}
}
\details{
   The shrinkage estimator is a James-Stein-type estimator.  It is essentially
   a  \code{\link{entropy.Dirichlet}} estimator, where the pseudocount is
   estimated from the data.
 
   For details see Hausser and Strimmer (2008).
}
\value{
  \code{entropy.shrink} returns an estimate of the Shannon entropy. 

  \code{freqs.shrink} returns the underlying frequencies. 
}

\references{
  Hausser, J., and K. Strimmer. 2008.  Entropy inference and the James-Stein
  estimator. Preprint (see \url{http://strimmerlab.org/publications/entropy2008.pdf}).
}
\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{entropy.Dirichlet}}, 
          \code{\link{entropy.NSB}}, 
         \code{\link{entropy.ChaoShen}}, 
         \code{\link{entropy.plugin}}.}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# shrinkage estimate
entropy.shrink(y)
}
\keyword{univar}
