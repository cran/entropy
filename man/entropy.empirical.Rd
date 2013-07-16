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

  \code{chi2.empirical} computes the empirical chi-squared statistic
   from counts \code{y1} and \code{y2}.

  \code{mi.empirical} computes the empirical mutual information from a table of counts \code{y2d}.

  \code{chi2indep.empirical} computes the empirical chi-squared statistic of independence
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

    \code{chi2.empirical} returns the empirical chi-squared statistic. 

    \code{mi.empirical} returns an estimate of the mutual information. 

    \code{chi2indep.empirical} returns the empirical chi-squared statistic of independence. 
}


\author{
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}

\seealso{\code{\link{entropy}}, \code{\link{entropy.plugin}}, \code{\link{KL.plugin}},
\code{\link{chi2.plugin}}, \code{\link{mi.plugin}}, \code{\link{chi2indep.plugin}}, \code{\link{discretize}}.}


\examples{
# load entropy library 
library("entropy")


# a single variable

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# empirical frequencies
freqs.empirical(y)

# empirical estimate of entropy
entropy.empirical(y)


# example with two variables

# observed counts for two random variables
y1 = c(4, 2, 3, 1, 10, 4)
y2 = c(2, 3, 7, 1, 4, 3)

# empirical Kullback-Leibler divergence
KL.empirical(y1, y2)

# half of the empirical chi-squared statistic
0.5*chi2.empirical(y1, y2)


## joint distribution example

# contingency table with counts for two discrete variables
y2d = rbind( c(1,2,3), c(6,5,4) )

# empirical estimate of mutual information
mi.empirical(y2d)

# half of the empirical chi-squared statistic of independence
0.5*chi2indep.empirical(y2d)


}
\keyword{univar}
