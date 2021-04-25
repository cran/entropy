\name{KL.plugin}
\alias{KL.plugin}
\alias{chi2.plugin}

\title{Plug-In Estimator of the Kullback-Leibler divergence and of the Chi-Squared Divergence}

\usage{
KL.plugin(freqs1, freqs2, unit=c("log", "log2", "log10"))
chi2.plugin(freqs1, freqs2, unit=c("log", "log2", "log10"))
}

\description{
  \code{KL.plugin} computes the Kullback-Leiber (KL) divergence between two discrete random variables \eqn{x_1} and \eqn{x_2}.  The corresponding probability mass functions are given by \code{freqs1} and \code{freqs2}. Note that the expectation is taken with regard to \eqn{x_1} using \code{freqs1}.
   
  \code{chi2.plugin} computes the chi-squared divergence between two discrete random variables \eqn{x_1} and \eqn{x_2} with \code{freqs1} and \code{freqs2} as corresponding probability mass functions.  Note that the denominator contains \code{freqs2}.
}

\arguments{
  \item{freqs1}{frequencies (probability mass function) for variable \eqn{x_1}.}
  \item{freqs2}{frequencies (probability mass function) for variable \eqn{x_2}.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   Kullback-Leibler divergence between the two discrete variables \eqn{x_1}
   to  \eqn{x_2} is \eqn{ \sum_k p_1(k) \log (p_1(k)/p_2(k)) }  where \eqn{p_1} and \eqn{p_2} are the probability mass functions of \eqn{x_1} and \eqn{x_2}, respectively, and \eqn{k} is 
the index for the classes.

   
   The chi-squared divergence is given by \eqn{ \sum_k (p_1(k)-p_2(k))^2/p_2(k) }.
  
   Note that both the KL divergence and the chi-squared divergence are not symmetric
   in  \eqn{x_1} and \eqn{x_2}.    The chi-squared divergence can be derived as a 
   quadratic approximation of twice the KL divergence.

}
\value{
  \code{KL.plugin} returns the KL divergence.

  \code{chi2.plugin} returns the chi-squared divergence. 
}


\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}

\seealso{\code{\link{KL.Dirichlet}}, \code{\link{KL.shrink}}, \code{\link{KL.empirical}}, \code{\link{mi.plugin}}, \code{\link{discretize2d}}. }


\examples{
# load entropy library 
library("entropy")

# probabilities for two random variables
freqs1 = c(1/5, 1/5, 3/5)
freqs2 = c(1/10, 4/10, 1/2) 

# KL divergence between x1 to x2
KL.plugin(freqs1, freqs2)

# and corresponding (half) chi-squared divergence
0.5*chi2.plugin(freqs1, freqs2)

## relationship to Pearson chi-squared statistic

# Pearson chi-squared statistic and p-value
n = 30 # sample size (observed counts)
chisq.test(n*freqs1, p = freqs2) # built-in function

# Pearson chi-squared statistic from Pearson divergence
pcs.stat = n*chi2.plugin(freqs1, freqs2) # note factor n
pcs.stat

# and p-value
df = length(freqs1)-1 # degrees of freedom
pcs.pval = 1-pchisq(pcs.stat, df)
pcs.pval
}
\keyword{univar}
