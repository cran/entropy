\name{entropy.NSB}
\alias{entropy.NSB}
\title{R Interface to NSB Entropy Estimator}

\usage{
entropy.NSB(y, unit=c("log", "log2", "log10"), CMD="nsb-entropy")
}

\description{
  \code{entropy.NSB} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} using the method 
   of Nemenman, Shafee and Bialek (2002).

   Note that this function is an R interface to the "nsb-entropy" program. 
   Hence, this needs to be installed separately from \url{http://nsb-entropy.sourceforge.net/}.}

\arguments{
  \item{y}{vector of counts.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
  \item{CMD}{path to the "nsb-entropy" executable.}
}
\details{
  The NSB estimator is due to Nemenman, Shafee and Bialek (2002).
  It is a Dirichlet-multinomial entropy estimator, with a hierarchical prior
  over the Dirichlet pseudocount parameters.

  Note that the NSB estimator is not a plug-in estimator, hence there
  are no explicit underlying bin frequencies.
}
\value{
  \code{entropy.NSB} returns an estimate of the Shannon entropy. 
}

\references{
  Nemenman, I., F. Shafee, and W. Bialek. 2002. Entropy and inference, revisited.
  In: Dietterich, T., S. Becker, Z. Gharamani, eds. Advances in Neural
  Information Processing Systems 14: 471-478. Cambridge (Massachusetts):
   MIT Press.
}
\author{
  Jean Hausser.
}
\seealso{\code{\link{entropy}}, \code{\link{entropy.shrink}},
         \code{\link{entropy.Dirichlet}}, 
         \code{\link{entropy.ChaoShen}}.
}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

\dontrun{
# estimate entropy using the NSB method
entropy.NSB(y) # 2.187774
}

# compare to empirical estimate
entropy.empirical(y)
}
\keyword{univar}
