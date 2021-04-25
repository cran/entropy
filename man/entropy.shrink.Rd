\name{entropy.shrink}
\alias{freqs.shrink}
\alias{entropy.shrink}
\alias{KL.shrink}
\alias{chi2.shrink}
\alias{mi.shrink}
\alias{chi2indep.shrink}


\title{Shrinkage Estimators of Entropy, Mutual Information and Related Quantities}

\usage{
freqs.shrink(y, lambda.freqs, verbose=TRUE)
entropy.shrink(y, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
KL.shrink(y1, y2, lambda.freqs1, lambda.freqs2, unit=c("log", "log2", "log10"),
            verbose=TRUE)
chi2.shrink(y1, y2, lambda.freqs1, lambda.freqs2, unit=c("log", "log2", "log10"),
            verbose=TRUE)
mi.shrink(y2d, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
chi2indep.shrink(y2d, lambda.freqs, unit=c("log", "log2", "log10"), verbose=TRUE)
}

\description{
  \code{freq.shrink} estimates the bin frequencies from the counts \code{y}
  using a James-Stein-type shrinkage estimator, where the shrinkage target is the uniform distribution.

  \code{entropy.shrink} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} by plug-in of shrinkage estimate
   of the bin frequencies.

  \code{KL.shrink} computes a shrinkage estimate of the Kullback-Leibler (KL) divergence 
   from counts \code{y1} and \code{y2}.

  \code{chi2.shrink} computes a shrinkage version of the chi-squared divergence
   from counts \code{y1} and \code{y2}.

  \code{mi.shrink} estimates a shrinkage estimate of mutual information of two random variables.
 
   \code{chi2indep.shrink} computes a shrinkage version of the chi-squared divergence of independence
   from a table of counts \code{y2d}.

}

\arguments{
  \item{y}{vector of counts.}
  \item{y1}{vector of counts.}
  \item{y2}{vector of counts.}
  \item{y2d}{matrix of counts.}

  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
  \item{lambda.freqs}{shrinkage intensity.  If not specified (default) it is estimated in a James-Stein-type fashion.}
  \item{lambda.freqs1}{shrinkage intensity for first random variable.  If not specified (default) it is estimated in a James-Stein-type fashion.}
  \item{lambda.freqs2}{shrinkage intensity for second random variable.  If not specified (default) it is estimated in a James-Stein-type fashion.}

  \item{verbose}{report shrinkage intensity.}
}
\details{
   The shrinkage estimator is a James-Stein-type estimator.  It is essentially
   a  \code{\link{entropy.Dirichlet}} estimator, where the pseudocount is
   estimated from the data.
 
   For details see Hausser and Strimmer (2009).
}
\value{
    \code{freqs.shrink} returns a shrinkage estimate of the frequencies.

    \code{entropy.shrink} returns a shrinkage estimate of the Shannon entropy. 

    \code{KL.shrink} returns a shrinkage estimate of the KL divergence. 

    \code{chi2.shrink} returns a shrinkage version of the chi-squared divergence. 

    \code{mi.shrink} returns a shrinkage estimate of the mutual information. 

    \code{chi2indep.shrink} returns a shrinkage version of the chi-squared divergence of independence. 

   In all instances the estimated shrinkage intensity is attached to the returned
   value as attribute \code{lambda.freqs}.
}

\references{
  Hausser, J., and K. Strimmer. 2009.  Entropy inference and the James-Stein
  estimator, with application to nonlinear gene association networks. 
   J. Mach. Learn. Res. \bold{10}: 1469-1484.  Available online from
   \url{https://jmlr.csail.mit.edu/papers/v10/hausser09a.html}.
}
\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy}}, \code{\link{entropy.Dirichlet}}, 
         \code{\link{entropy.plugin}},  
          \code{\link{KL.plugin}}, \code{\link{mi.plugin}}, \code{\link{discretize}}.}

\examples{
# load entropy library 
library("entropy")

# a single variable

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# shrinkage estimate of frequencies
freqs.shrink(y)

# shrinkage estimate of entropy
entropy.shrink(y)


# example with two variables

# observed counts for two random variables
y1 = c(4, 2, 3, 1, 10, 4)
y2 = c(2, 3, 7, 1, 4, 3)

# shrinkage estimate of Kullback-Leibler divergence
KL.shrink(y1, y2)

# half of the shrinkage chi-squared divergence
0.5*chi2.shrink(y1, y2)


## joint distribution example

# contingency table with counts for two discrete variables
y2d = rbind( c(1,2,3), c(6,5,4) )

# shrinkage estimate of mutual information
mi.shrink(y2d)

# half of the shrinkage chi-squared divergence of independence
0.5*chi2indep.shrink(y2d)


}
\keyword{univar}
