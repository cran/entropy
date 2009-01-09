\name{entropy.Dirichlet}
\alias{entropy.Dirichlet}
\alias{freqs.Dirichlet}
\alias{mi.Dirichlet}
\title{Family of Dirichlet Entropy and Mutual Information Estimators}

\usage{
entropy.Dirichlet(y, a, unit=c("log", "log2", "log10"))
mi.Dirichlet(y, a, unit=c("log", "log2", "log10"))
freqs.Dirichlet(y, a)
}

\description{
  \code{entropy.Dirichlet} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y} by plug-in of Bayesian estimates
   of the bin frequencies using the  Dirichlet-multinomial       
   pseudocount model.

  \code{mi.Dirichlet} estimates the corresponding mutual information of two random variables.

  \code{freqs.Dirichlet} computes the Bayesian estimates
   of the bin frequencies using the  Dirichlet-multinomial       
   pseudocount model.
}

\arguments{
  \item{y}{vector or matrix of counts.}
  \item{a}{pseudocount per bin.}
  \item{unit}{the unit in which entropy is measured.}
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
  \code{entropy.Dirichlet} returns an estimate of the Shannon entropy. 

  \code{mi.Dirichlet} returns an estimate of the mutual information. 

  \code{freqs.Dirichlet} returns the underlying frequencies. 
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
  Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{entropy}}, 
         \code{\link{entropy.shrink}},
         \code{\link{entropy.NSB}}, 
         \code{\link{entropy.ChaoShen}}, 
         \code{\link{entropy.empirical}}, 
         \code{\link{entropy.plugin}},
         \code{\link{mi.plugin}}.
}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

# Dirichlet estimate with a=0
entropy.Dirichlet(y, a=0)

# compare to empirical estimate
entropy.empirical(y)

# Dirichlet estimate with a=1/2
entropy.Dirichlet(y, a=1/2)

# Dirichlet estimate with a=1
entropy.Dirichlet(y, a=1)

# Dirichlet estimate with a=1/length(y)
entropy.Dirichlet(y, a=1/length(y))

# Dirichlet estimate with a=sqrt(sum(y))/length(y)
entropy.Dirichlet(y, a=sqrt(sum(y))/length(y))


# contigency table with counts for two discrete variables
y = rbind( c(1,2,3), c(6,5,4) )

# Dirichlet estimate of mutual information (with a=1/2)
mi.Dirichlet(y, a=1/2)
}
\keyword{univar}
