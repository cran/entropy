\name{entropy.Dirichlet}
\alias{freqs.Dirichlet}
\alias{entropy.Dirichlet}
\alias{KL.Dirichlet}
\alias{chi2.Dirichlet}
\alias{mi.Dirichlet}
\alias{chi2indep.Dirichlet}

\title{Dirichlet Prior Bayesian Estimators of Entropy, Mutual Information 
    and Other Related Quantities}

\usage{
freqs.Dirichlet(y, a)
entropy.Dirichlet(y, a, unit=c("log", "log2", "log10"))
KL.Dirichlet(y1, y2, a1, a2, unit=c("log", "log2", "log10"))
chi2.Dirichlet(y1, y2, a1, a2, unit=c("log", "log2", "log10"))
mi.Dirichlet(y2d, a, unit=c("log", "log2", "log10"))
chi2indep.Dirichlet(y2d, a, unit=c("log", "log2", "log10"))
}

\description{
  \code{freqs.Dirichlet} computes the Bayesian estimates
   of the bin frequencies using the  Dirichlet-multinomial       
   pseudocount model.

  \code{entropy.Dirichlet} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} by plug-in of Bayesian estimates
   of the bin frequencies using the  Dirichlet-multinomial       
   pseudocount model.

  \code{KL.Dirichlet} computes a Bayesian estimate of the Kullback-Leibler (KL) divergence 
   from counts \code{y1} and \code{y2}.

  \code{chi2.Dirichlet} computes a Bayesian version of the chi-squared divergence
   from counts \code{y1} and \code{y2}.

  \code{mi.Dirichlet} computes a Bayesian estimate of mutual information of two random variables.

  \code{chi2indep.Dirichlet} computes a Bayesian version of the chi-squared divergence of 
   independence from a table of counts \code{y2d}.
}

\arguments{
  \item{y}{vector of counts.}
  \item{y1}{vector of counts.}
  \item{y2}{vector of counts.}
  \item{y2d}{matrix of counts.}
  \item{a}{pseudocount per bin.}
  \item{a1}{pseudocount per bin for first random variable.}
  \item{a2}{pseudocount per bin for second random variable.}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
}
\details{
  The Dirichlet-multinomial pseudocount entropy estimator
   is a Bayesian plug-in estimator: 
   in the definition of the Shannon entropy the
   bin probabilities are replaced by the respective Bayesian estimates
   of the frequencies, using a model with a Dirichlet prior and a multinomial likelihood.

  The parameter \code{a} is a parameter of the Dirichlet prior, and in effect
  specifies the pseudocount per bin.  Popular choices of \code{a} are:
  \itemize{
   \item{a=0:}{maximum likelihood estimator (see \code{\link{entropy.empirical}}) } 
   \item{a=1/2:}{Jeffreys' prior; Krichevsky-Trovimov (1991) entropy estimator}
   \item{a=1:}{Laplace's prior}
   \item{a=1/length(y):}{Schurmann-Grassberger (1996) entropy estimator}
   \item{a=sqrt(sum(y))/length(y):}{minimax prior}
  }

  The pseudocount \code{a} can also be a vector so that for each bin an 
  individual pseudocount is added.
 
}
\value{
  \code{freqs.Dirichlet} returns the Bayesian estimates of the frequencies . 

  \code{entropy.Dirichlet} returns the Bayesian estimate of the Shannon entropy. 

  \code{KL.Dirichlet} returns the Bayesian estimate of the KL divergence. 

  \code{chi2.Dirichlet} returns the Bayesian version of the chi-squared divergence. 

  \code{mi.Dirichlet} returns the Bayesian estimate of the mutual information. 

  \code{chi2indep.Dirichlet} returns the Bayesian version of the chi-squared divergence of independence. 

}

\references{
  Agresti, A., and D. B. Hitchcock. 2005. Bayesian inference for categorical
  data analysis. Stat. Methods. Appl. \bold{14}:297--330.
 
  Krichevsky, R. E., and V. K. Trofimov. 1981. The performance of universal encoding.
  IEEE Trans. Inf. Theory \bold{27}: 199-207.  

  Schurmann, T., and P. Grassberger. 1996. Entropy estimation of symbol sequences.
  Chaos \bold{6}:41-427.

}
\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy}}, 
         \code{\link{entropy.shrink}},
         \code{\link{entropy.empirical}}, 
         \code{\link{entropy.plugin}},
         \code{\link{mi.plugin}}, \code{\link{KL.plugin}}, \code{\link{discretize}}.
}

\examples{
# load entropy library 
library("entropy")


# a single variable

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# Dirichlet estimate of frequencies with a=1/2
freqs.Dirichlet(y, a=1/2)

# Dirichlet estimate of entropy with a=0
entropy.Dirichlet(y, a=0)

# identical to empirical estimate
entropy.empirical(y)

# Dirichlet estimate with a=1/2 (Jeffreys' prior)
entropy.Dirichlet(y, a=1/2)

# Dirichlet estimate with a=1 (Laplace prior)
entropy.Dirichlet(y, a=1)

# Dirichlet estimate with a=1/length(y)
entropy.Dirichlet(y, a=1/length(y))

# Dirichlet estimate with a=sqrt(sum(y))/length(y)
entropy.Dirichlet(y, a=sqrt(sum(y))/length(y))


# example with two variables

# observed counts for two random variables
y1 = c(4, 2, 3, 1, 10, 4)
y2 = c(2, 3, 7, 1, 4, 3)

# Bayesian estimate of Kullback-Leibler divergence (a=1/6)
KL.Dirichlet(y1, y2, a1=1/6, a2=1/6)

# half of the corresponding chi-squared divergence
0.5*chi2.Dirichlet(y1, y2, a1=1/6, a2=1/6)


## joint distribution example

# contingency table with counts for two discrete variables
y2d = rbind( c(1,2,3), c(6,5,4) )

# Bayesian estimate of mutual information (a=1/6)
mi.Dirichlet(y2d, a=1/6)

# half of the Bayesian chi-squared divergence of independence
0.5*chi2indep.Dirichlet(y2d, a=1/6)


}
\keyword{univar}
