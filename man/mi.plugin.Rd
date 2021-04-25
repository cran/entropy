\name{mi.plugin}
\alias{mi.plugin}
\alias{chi2indep.plugin}

\title{Plug-In Estimator of Mutual Information and of the Chi-Squared Statistic of Independence}

\usage{
mi.plugin(freqs2d, unit=c("log", "log2", "log10"))
chi2indep.plugin(freqs2d, unit=c("log", "log2", "log10"))
}

\description{
  \code{mi.plugin} computes the mutual information 
   of two discrete random variables from the specified joint probability mass function.

  \code{chi2indep.plugin} computes the chi-squared divergence of independence.
}

\arguments{
  \item{freqs2d}{matrix of joint bin frequencies (joint probability mass function).}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   The mutual information of two random variables \eqn{X} and \eqn{Y}
   is the Kullback-Leibler divergence between the joint density/probability
   mass function and the product independence density of the marginals.

   It can also defined using entropy as \eqn{MI = H(X) + H(Y) - H(X, Y)}. 

   Similarly, the chi-squared divergence of independence is the chi-squared divergence
   between the joint density and the product density. It is a second-order 
   approximation of twice the mutual information.

}
\value{
  \code{mi.plugin} returns the mutual information.

  \code{chi2indep.plugin} returns the chi-squared divergence of independence. 
}


\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}

\seealso{\code{\link{mi.Dirichlet}}, \code{\link{mi.shrink}}, \code{\link{mi.empirical}}, \code{\link{KL.plugin}}, \code{\link{discretize2d}}. }


\examples{
# load entropy library 
library("entropy")

# joint distribution of two discrete variables
freqs2d = rbind( c(0.2, 0.1, 0.15), c(0.1, 0.2, 0.25) )  

# corresponding mutual information
mi.plugin(freqs2d)

# MI computed via entropy
H1 = entropy.plugin(rowSums(freqs2d))
H2 = entropy.plugin(colSums(freqs2d))
H12 = entropy.plugin(freqs2d)
H1+H2-H12

# and corresponding (half) chi-squared divergence of independence
0.5*chi2indep.plugin(freqs2d)

}
\keyword{univar}
