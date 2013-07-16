\name{KL.plugin}
\alias{KL.plugin}
\alias{chi2.plugin}

\title{Plug-In Estimator of the Kullback-Leibler divergence and of the Chi-Squared Statistic}

\usage{
KL.plugin(freqs1, freqs2, unit=c("log", "log2", "log10"))
chi2.plugin(freqs1, freqs2, unit=c("log", "log2", "log10"))
}

\description{
  \code{KL.plugin} computes the Kullback-Leiber (KL) divergence from random variable \eqn{X_1}
   to  \eqn{X_2}.  The corresponding probability mass functions are given by \code{freqs1} and \code{freqs2}, and the expectation is computed over \code{freqs1}.
   
  \code{chi2.plugin} computes the chi-squared statistic between an observed \eqn{X_1} and an expected \eqn{X_2}, where \code{freqs1} and \code{freqs2} are the corresponding probability mass functions.
}

\arguments{
  \item{freqs1}{bin frequencies for variable \eqn{X_1}.}
  \item{freqs2}{bin frequencies for variable \eqn{X_2}.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   Kullback-Leibler divergence between the from random variable \eqn{X_1}
   to  \eqn{X_2} is given as \eqn{ E_{X_1} \log (f(x_1)/f(x_2)) }.

   
   The chi-squared statistic is given \eqn{ \sum (f(x_1)-f(x_2))^2/f(x_2) }.
   It can also be seen as a second-order accurate
   approximation of twice the KL divergence.

   Note that both the KL divergence and the chi-squared statistic are not symmetric
   in  \eqn{X_1} and \eqn{X_2}.

}
\value{
  \code{KL.plugin} returns the KL divergence.

  \code{chi2.plugin} returns the chi-squared statistic. 
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{KL.Dirichlet}}, \code{\link{KL.shrink}}, \code{\link{KL.empirical}}, \code{\link{mi.plugin}}, \code{\link{discretize2d}}. }


\examples{
# load entropy library 
library("entropy")

# probabilities for two random variables
freqs1 = c(1/5, 1/5, 3/5)
freqs2 = c(1/10, 4/10, 1/2) 

# KL divergence from X1 to X2
KL.plugin(freqs1, freqs2)

# and corresponding (half) chi-squared statistic
0.5*chi2.plugin(freqs1, freqs2)

}
\keyword{univar}
