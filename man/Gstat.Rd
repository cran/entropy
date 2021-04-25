\name{Gstat}
\alias{Gstat}
\alias{Gstatindep}
\alias{chi2stat}
\alias{chi2statindep}

\title{G Statistic and Chi-Squared Statistic}

\usage{
Gstat(y, freqs, unit=c("log", "log2", "log10"))
chi2stat(y, freqs, unit=c("log", "log2", "log10"))
Gstatindep(y2d, unit=c("log", "log2", "log10"))
chi2statindep(y2d, unit=c("log", "log2", "log10"))
}

\description{

  \code{Gstat} computes the G statistic. 

  \code{chi2stat} computes the Pearson chi-squared statistic. 

  \code{Gstatindep} computes the G statistic between the empirical observed joint distribution and the product distribution obtained from its marginals.

  \code{chi2statindep} computes the Pearson chi-squared statistic of independence.
}

\arguments{
  \item{y}{observed vector of counts.}
  \item{freqs}{vector of expected frequencies (probability mass function). Alternatively, counts may be provided.}  
  \item{y2d}{matrix of counts.}

  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   The observed counts in \code{y} and \code{y2d} are used to determine the total sample size.

   The G statistic equals two times the sample size times the KL divergence between
   empirical observed frequencies and expected frequencies. 

   The Pearson chi-squared statistic equals sample size times chi-squared divergence
   between empirical observed frequencies and expected frequencies.   It is a quadratic
   approximation of the G statistic.

   The G statistic between the empirical observed joint distribution and the product
   distribution obtained from its marginals is equal to  two times the sample size times mutual
   information.  

   The Pearson chi-squared statistic of independence equals the Pearson chi-squared statistic 
   between the empirical observed joint distribution and the product distribution obtained from 
   its marginals.  It is a quadratic approximation of the corresponding G statistic.

   The G statistic and the Pearson chi-squared statistic are asymptotically chi-squared distributed 
   which allows to compute corresponding p-values.
}
\value{
    A list containing the test statistic \code{stat}, the degree of freedom \code{df} used to calculate the
    p-value \code{pval}.
}


\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}

\seealso{\code{\link{KL.plugin}},
\code{\link{chi2.plugin}}, \code{\link{mi.plugin}}, \code{\link{chi2indep.plugin}}.}


\examples{
# load entropy library 
library("entropy")

## one discrete random variable

# observed counts in each class
y = c(4, 2, 3, 1, 6, 4)
n = sum(y) # 20

# expected frequencies and counts
freqs.expected = c(0.10, 0.15, 0.35, 0.05, 0.20, 0.15)
y.expected = n*freqs.expected


# G statistic (with p-value) 
Gstat(y, freqs.expected) # from expected frequencies
Gstat(y, y.expected) # alternatively from expected counts

# G statistic computed from empirical KL divergence
2*n*KL.empirical(y, y.expected)


## Pearson chi-squared statistic (with p-value) 
# this can be viewed an approximation of the G statistic
chi2stat(y, freqs.expected) # from expected frequencies
chi2stat(y, y.expected) # alternatively from expected counts

# computed from empirical chi-squared divergence
n*chi2.empirical(y, y.expected)

# compare with built-in function
chisq.test(y, p = freqs.expected) 


## joint distribution of two discrete random variables

# contingency table with counts
y.mat = matrix(c(4, 5, 1, 2, 4, 4), ncol = 2)  # 3x2 example matrix of counts
n.mat = sum(y.mat) # 20


# G statistic between empirical observed joint distribution and product distribution
Gstatindep( y.mat )

# computed from empirical mutual information
2*n.mat*mi.empirical(y.mat)


# Pearson chi-squared statistic of independence
chi2statindep( y.mat )

# computed from empirical chi-square divergence
n.mat*chi2indep.empirical(y.mat)

# compare with built-in function
chisq.test(y.mat) 

}
\keyword{univar}
