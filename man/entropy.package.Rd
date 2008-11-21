\encoding{latin1}
\name{entropy-package}
\alias{entropy-package}
\docType{package}
\title{The entropy package}

\description{This package implements various estimators of the Shannon entropy.
Most estimators in this package can be applied in "small n, large p" situations, 
i.e. when there are many more bins than counts.

The main function of this package is \code{\link{entropy}}, which provides
a unified interface to various entropy estimators.

If you use this package please cite:
Jean Hausser and Korbinian Strimmer. 2008.  Entropy inference and the James-Stein estimator. 
See \url{http://arxiv.org/abs/0811.3579} for publication details.

This paper contains a detailed statistical comparison of the estimators available 
in this package. It also describes the shrinkage entropy estimator \code{\link{entropy.shrink}}.
}

\author{Jean Hausser and Korbinian Strimmer (\url{http://strimmerlab.org/})}
\references{
  See website: \url{http://strimmerlab.org/software/entropy/}
}
\keyword{univar}
\seealso{
\code{\link{entropy}}
}
