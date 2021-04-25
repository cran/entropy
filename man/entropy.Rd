\name{entropy}
\alias{entropy}
\alias{freqs}
\title{Estimating Entropy From Observed Counts}

\usage{
entropy(y, lambda.freqs, method=c("ML", "MM", "Jeffreys", "Laplace", "SG",
    "minimax", "CS", "NSB", "shrink"), unit=c("log", "log2", "log10"), verbose=TRUE, ...)
freqs(y, lambda.freqs, method=c("ML", "MM", "Jeffreys", "Laplace", "SG",
    "minimax", "CS", "NSB", "shrink"), verbose=TRUE)
}

\description{
  \code{entropy} estimates the Shannon entropy H of the random variable Y
   from the corresponding observed counts \code{y}.

   \code{freqs} estimates bin frequencies from the counts \code{y}.
}

\arguments{
  \item{y}{vector of counts.}
  \item{method}{the method employed to estimate entropy (see Details).}
  \item{unit}{the unit in which entropy is measured. 
                The default is "nats" (natural units). For 
                computing entropy in "bits" set \code{unit="log2"}.}
  \item{lambda.freqs}{shrinkage intensity (for "shrink" option). }
  \item{verbose}{verbose option (for "shrink" option).  }
  \item{\dots}{option passed on to \code{\link{entropy.NSB}}.}
}
\details{
  The \code{entropy} function allows to estimate entropy from observed counts by a variety
  of methods:
 \itemize{
  \item{\code{method="ML"}:}{maximum likelihood, see \code{\link{entropy.empirical}} }
  \item{\code{method="MM"}:}{bias-corrected maximum likelihood, see \code{\link{entropy.MillerMadow}} }
  \item{\code{method="Jeffreys"}:}{\code{\link{entropy.Dirichlet}} with \code{a=1/2} }
  \item{\code{method="Laplace"}:}{\code{\link{entropy.Dirichlet}} with \code{a=1} }
  \item{\code{method="SG"}:}{\code{\link{entropy.Dirichlet}} with \code{a=a=1/length(y)} }
  \item{\code{method="minimax"}:}{\code{\link{entropy.Dirichlet}} with \code{a=sqrt(sum(y))/length(y} }
  \item{\code{method="CS"}:}{see \code{\link{entropy.ChaoShen}} }
  \item{\code{method="NSB"}:}{see \code{\link{entropy.NSB}} }
  \item{\code{method="shrink"}:}{see \code{\link{entropy.shrink}} }
 }
 The \code{freqs} function estimates the underlying bin frequencies.  Note that
 estimated frequencies are not
 available for \code{method="MM"}, \code{method="CS"} and \code{method="NSB"}. In these
 instances a vector containing NAs is returned.
}
\value{
  \code{entropy} returns an estimate of the Shannon entropy. 

  \code{freqs} returns a vector with estimated bin frequencies (if available).
}

\author{
  Korbinian Strimmer (\url{http://www.strimmerlab.org}).
}
\seealso{\code{\link{entropy-package}}, \code{\link{discretize}}.}

\examples{
# load entropy library 
library("entropy")

# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  

entropy(y, method="ML")
entropy(y, method="MM")
entropy(y, method="Jeffreys")
entropy(y, method="Laplace")
entropy(y, method="SG")
entropy(y, method="minimax")
entropy(y, method="CS")
#entropy(y, method="NSB")
entropy(y, method="shrink")
}
\keyword{univar}
