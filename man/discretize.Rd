\name{discretize}
\alias{discretize}
\alias{discretize2d}

\title{Discretize Continuous Random Variables}

\usage{
discretize( x, numBins, r=range(x) )
discretize2d( x1, x2, numBins1, numBins2, r1=range(x1), r2=range(x2) )
}

\description{
  \code{discretize} puts observations from a continuous random variable 
    into bins and returns the corresponding vector of counts.
 
\code{discretize2d} puts observations from a pair of continuous random variables 
    into bins and returns the corresponding table of counts.
}

\arguments{
  \item{x}{vector of observations.}
  \item{x1}{vector of observations for the first random variable.}
  \item{x2}{vector of observations for the second random variable.}
  \item{numBins}{number of bins.}
  \item{numBins1}{number of bins for the first random variable.}
  \item{numBins2}{number of bins for the second random variable.}
  \item{r}{range of the random variable (default: observed range).}
  \item{r1}{range of the first random variable (default: observed range).}
  \item{r2}{range of the second random variable (default: observed range).}
}
\details{
  The bins for a random variable all have the same width. It is determined by the length of the range divided by the number of bins.
}
\value{
  \code{discretize} returns a vector containing the counts for each bin.

  \code{discretize2d} returns a matrix containing the counts for each bin. 
}


\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy}}.}

\examples{
# load entropy library 
library("entropy")

### 1D example ####

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

### 2D example ####

# two independent random variables
x1 = runif(10000)
x2 = runif(10000)

y2d = discretize2d(x1, x2, numBins1=10, numBins2=10)
sum(y2d)

# joint entropy
H12 = entropy(y2d )
H12
log(100) # theoretical maximum for 10x10 table

# mutual information
mi.empirical(y2d) # approximately zero


# another way to compute mutual information

# compute marginal entropies
H1 = entropy(rowSums(y2d))
H2 = entropy(colSums(y2d))

H1+H2-H12 # mutual entropy

}
\keyword{univar}


