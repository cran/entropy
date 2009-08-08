\name{entropy.shrink}
\alias{entropy.shrink}
\alias{freqs.shrink}
\alias{mi.shrink}
\title{Shrinkage Entropy and Mutual Information Estimator}

\usage{
entropy.shrink(y, unit=c("log", "log2", "log10"), target=1/length(y), verbose=TRUE)
mi.shrink(y, unit=c("log", "log2", "log10"), target=1/length(y), verbose=TRUE)
freqs.shrink(y, target=1/length(y), verbose=TRUE)
}

\description{
  \code{entropy.shrink} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} by plug-in of shrinkage estimate
   of the bin frequencies.
 
  \code{mi.shrink} estimates the corresponding mutual information of two random variables.
 
  \code{freq.shrink} estimates the bin frequencies from the counts \code{y}
  using a James-Stein-type shrinkage estimator.  The default shrinkage target is the uniform,
  unless otherwise specified.
}

\arguments{
  \item{y}{vector or matrix of counts.}
  \item{unit}{the unit in which entropy is measured.}
  \item{target}{the shrinkage target for the frequencies (default: uniform distribution).}
  \item{verbose}{report shrinkage intensity.}
}
\details{
   The shrinkage estimator is a James-Stein-type estimator.  It is essentially
   a  \code{\link{entropy.Dirichlet}} estimator, where the pseudocount is
   estimated from the data.
 
   For details see Hausser and Strimmer (2009).
}
\value{
  \code{entropy.shrink} returns an estimate of the Shannon entropy. 

   \code{mi.shrink} returns an estimate of mutual information. 

  \code{freqs.shrink} returns the underlying frequencies. 

   In all instances the estimated shrinkage intensity is attached to the returned
   value in the attribute \code{lambda.freqs}.
}

\references{
  Hausser, J., and K. Strimmer. 2009.  Entropy inference and the James-Stein
  estimator, with application to nonlinear gene association networks. 
   J. Mach. Learn. Res. \bold{10}: 1469-1484.  Available online from
   \url{http://jmlr.csail.mit.edu/papers/v10/hausser09a.html}.
}
\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{entropy}}, \code{\link{entropy.Dirichlet}}, 
          \code{\link{entropy.NSB}}, 
         \code{\link{entropy.ChaoShen}}, 
         \code{\link{entropy.plugin}},  \code{\link{mi.plugin}}.}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# shrinkage estimate
entropy.shrink(y)

# contigency table with counts for two discrete variables
y = rbind( c(1,2,3), c(6,5,4) )

# shrinkage estimate of mutual information
mi.shrink(y)
}
\keyword{univar}
