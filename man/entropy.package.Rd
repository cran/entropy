\encoding{latin1}
\name{entropy-package}
\alias{entropy-package}
\docType{package}
\title{The entropy Package}

\description{This package implements various estimators of the Shannon entropy.
Most estimators in this package can be applied in ``small n, large p'' situations, 
i.e. when there are many more bins than counts.

The main function of this package is \code{\link{entropy}}, which provides
a unified interface to various entropy estimators.  Other functions included in this package are
estimators of Kullback-Leibler divergence (\code{\link{KL.plugin}}), mutual information (\code{\link{mi.plugin}}) and of the chi-squared divergence (\code{\link{chi2.plugin}}).
Furthermore, there are functions to compute the G statistic (\code{\link{Gstat}})
and the chi-squared statistic (\code{\link{chi2stat}}).

If you use this package please cite:
Jean Hausser and Korbinian Strimmer. 2009.  Entropy inference and the James-Stein estimator, with application to nonlinear gene association networks.  J. Mach. Learn. Res. \bold{10}: 1469-1484.  Available online from
   \url{https://jmlr.csail.mit.edu/papers/v10/hausser09a.html}.

This paper contains a detailed statistical comparison of the estimators available 
in this package. It also describes the shrinkage entropy estimator \code{\link{entropy.shrink}}.
}

\author{Jean Hausser and Korbinian Strimmer (\url{http://www.strimmerlab.org/})}
\references{
  See website: \url{http://www.strimmerlab.org/software/entropy/}
}
\keyword{univar}
\seealso{
\code{\link{entropy}}
}
