\encoding{latin1}
\name{entropy-package}
\alias{entropy-package}
\docType{package}
\title{The entropy package}

\description{This package implements various estimators of the Shannon entropy.
Most estimators in this package can be applied in "small n, large p" situations, 
i.e. when there are many more bins than counts.

The main function of this package is \code{\link{entropy}}, which provides
a unified interface to the implemented entropy estimators.

If you use this package please cite:
Jean Hausser and Korbinian Strimmer. 2008.  Entropy inference and the James-Stein estimator. 
Preprint (see \url{http://strimmerlab.org/publications/entropy2008.pdf}).

This paper contains a detailed statistical comparison of the estimators available 
in this package. It also describes the shrinkage entropy estimator \code{\link{entropy.shrink}}.
}

\author{Jean Hausser (\url{http://jean.hausser.org/}) and Korbinian Strimmer (\url{http://strimmerlab.org/})}
\references{
  See website: \url{http://strimmerlab.org/software/entropy/}
}
\keyword{univar}
\seealso{
\code{\link{entropy}}
}
