\name{entropy.empirical}
\alias{freqs.empirical}
\alias{entropy.empirical}
\alias{KL.empirical}
\alias{chi2.empirical}
\alias{mi.empirical}
\alias{chi2indep.empirical}

\title{Empirical Estimators of Entropy and Mutual Information and Related Quantities}

\usage{
freqs.empirical(y)
entropy.empirical(y, unit=c("log", "log2", "log10"))
KL.empirical(y1, y2, unit=c("log", "log2", "log10"))
chi2.empirical(y1, y2, unit=c("log", "log2", "log10"))
mi.empirical(y2d, unit=c("log", "log2", "log10"))
chi2indep.empirical(y2d, unit=c("log", "log2", "log10"))
}

\description{
  \code{freqs.empirical} computes the empirical frequencies from counts \code{y}.

  \code{entropy.empirical} estimates the Shannon entropy H 
   of the random variable Y from the corresponding observed counts \code{y}
   by plug-in of the empirical frequencies.

  \code{KL.empirical} computes the empirical Kullback-Leibler (KL) divergence 
   from counts \code{y1} and \code{y2}.

  \code{chi2.empirical} computes the empirical chi-squared divergence
   from counts \code{y1} and \code{y2}.

  \code{mi.empirical} computes the empirical mutual information from a table of counts \code{y2d}.

  \code{chi2indep.empirical} computes the empirical chi-squared divergence of independence
   from a table of counts \code{y2d}.
  
}

\arguments{
  \item{y}{vector of counts.}
  \item{y1}{vector of counts.}
  \item{y2}{vector of counts.}
  \item{y2d}{matrix of counts.}

  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
   The empirical entropy estimator is a plug-in estimator: 
   in the definition of the Shannon entropy the
   bin probabilities are replaced by the respective empirical frequencies.

   The empirical entropy estimator is the maximum likelihood estimator.
   If there are many zero counts and the sample size is small
   it is very inefficient and also strongly biased.
}
\value{
    \code{freqs.empirical} returns the empirical frequencies.

    \code{entropy.empirical} returns an estimate of the Shannon entropy. 

    \code{KL.empirical} returns an estimate of the KL divergence. 

    \code{chi2.empirical} returns the empirical chi-squared divergence. 

    \code{mi.empirical} returns an estimate of the mutual information. 

    \code{chi2indep.empirical} returns the empirical chi-squared divergence of independence. 
}


\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}

\seealso{\code{\link{entropy}}, \code{\link{entropy.plugin}}, \code{\link{KL.plugin}},
\code{\link{chi2.plugin}}, \code{\link{mi.plugin}}, \code{\link{chi2indep.plugin}},
\code{\link{Gstat}}, \code{\link{Gstatindep}}, \code{\link{chi2stat}}, 
\code{\link{chi2statindep}}, \code{\link{discretize}}.}


\examples{
# load entropy library 
library("entropy")


## a single variable: entropy

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# empirical frequencies
freqs.empirical(y)

# empirical estimate of entropy
entropy.empirical(y)


## examples with two variables: KL and chi-squared divergence

# observed counts for first random variables (observed)
y1 = c(4, 2, 3, 1, 6, 4)
n = sum(y1) # 20

# counts for the second random variable (expected)
freqs.expected = c(0.10, 0.15, 0.35, 0.05, 0.20, 0.15)
y2 = n*freqs.expected

# empirical Kullback-Leibler divergence
KL.div = KL.empirical(y1, y2)
KL.div

# empirical chi-squared divergence
cs.div = chi2.empirical(y1, y2)
cs.div 
0.5*cs.div  # approximates KL.div

## note: see also Gstat and chi2stat


## joint distribution of two discrete random variables

# contingency table with counts for two discrete variables
y.mat = matrix(c(4, 5, 1, 2, 4, 4), ncol = 2)  # 3x2 example matrix of counts
n.mat = sum(y.mat) # 20

# empirical estimate of mutual information
mi = mi.empirical(y.mat)
mi

# empirical chi-squared divergence of independence
cs.indep = chi2indep.empirical(y.mat)
cs.indep
0.5*cs.indep # approximates mi

## note: see also Gstatindep and chi2statindep

}
\keyword{univar}
