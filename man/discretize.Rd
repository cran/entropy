\name{discretize}
\alias{discretize}

\title{Discretize a Continuous Random Variable}

\usage{
discretize( x, numBins, r=range(x) )
}

\description{
  \code{discretize} puts observations from a continuous random variable 
    into bins and returns the corresponding table of counts.
}

\arguments{
  \item{x}{vector of observations.}
  \item{numBins}{number of bins.}
  \item{r}{range of the random variable (default: observed range).}
}
\details{
  All bins have the same width. It is determined by the length of the range divided by the number of bins.
}
\value{
  \code{discretize} returns a vector containing the counts for each bin. 

}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{entropy}}.}

\examples{
# load entropy library 
library("entropy")

# sample from continuous uniform distribution
x1 = runif(10000)
hist(x1, xlim=c(0,1), freq=FALSE)

# discretize into 10 categories
y1 = discretize(x1, numBins=10, r=c(0,1))
y1

# compute entropy from counts
entropy(y1) # empirical estimate near theoretical maximum
log(10) # theoretical value for discrete uniform distribution with 10 bins 

# sample from a non-uniform distribution 
x2 = rbeta(10000, 750, 250)
hist(x2, xlim=c(0,1), freq=FALSE)

# discretize into 10 categories and estimate entropy
y2 = discretize(x2, numBins=10, r=c(0,1))
y2
entropy(y2) # almost zero

}
\keyword{univar}


