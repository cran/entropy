\name{entropy.ChaoShen}
\alias{entropy.ChaoShen}
\title{Chao-Shen Entropy Estimator}

\usage{
entropy.ChaoShen(y, unit=c("log", "log2", "log10"))
}

\description{
  \code{entropy.ChaoShen} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} using the method of Chao and Shen (2003).
}

\arguments{
  \item{y}{vector of counts.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
  The Chao-Shen entropy estimator (2003) is a  Horvitz-Thompson (1952) 
  estimator applied to the problem of entropy estimation, 
  with additional coverage correction as proposed by Good (1953). 

  Note that the Chao-Shen estimator is not a plug-in estimator, hence there
  are no explicit underlying bin frequencies.
}
\value{
  \code{entropy.ChaoShen} returns an estimate of the Shannon entropy. 
}

\references{
  Chao, A., and T.-J. Shen. 2003.  Nonparametric estimation of Shannon's
  index of diversity when there are unseen species in sample.
  Environ. Ecol. Stat. \bold{10}:429-443.

  Good, I. J. 1953. The population frequencies of species and the estimation of
  population parameters. Biometrika \bold{40}:237-264.

  Horvitz, D.G., and D. J. Thompson. 1952. A generalization of sampling without
  replacement from a finite universe. J. Am. Stat. Assoc. \bold{47}:663-685.
}
\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy}}, 
         \code{\link{entropy.shrink}}, 
         \code{\link{entropy.Dirichlet}}, 
         \code{\link{entropy.NSB}}.}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# estimate entropy using Chao-Shen method
entropy.ChaoShen(y)

# compare to empirical estimate
entropy.empirical(y)
}
\keyword{univar}
