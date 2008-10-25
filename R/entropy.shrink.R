### entropy.shrink.R  (2008-10-25)
###
###    Shrinkage entropy and mutual information estimator
###
### Copyright 2008 Korbinian Strimmer
###
###
### This file is part of the `entropy' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# shrinkage estimate of entropy 

# y:  a vector of counts (may include zeros)
 

entropy.shrink = function(y, unit=c("log", "log2", "log10"), target=1/length(y), verbose=TRUE)
{
  f = freqs.shrink(y, target=target, verbose=verbose)
  h = entropy.plugin(f, unit=unit)
  attr(h, "lambda.freqs") = attr(f, "lambda.freqs") # shrinkage intensity

  return( h )
}


freqs.shrink = function (y, target = 1/length(y), verbose = TRUE) 
{
    n = sum(y)
    u = y/n

    if (n==1 || n==0)
    {
      lambda.freqs = 1
       u.shrink = rep(0, length(y)) + target
    }
    else
    {
      lambda.freqs = get.lambda.shrink(n, u, target, verbose)
      u.shrink = lambda.freqs * target + (1 - lambda.freqs) * u
    }
    attr(u.shrink, "lambda.freqs") = lambda.freqs
    
    return(u.shrink)
}


mi.shrink = function(y, unit=c("log", "log2", "log10"), target=1/length(y), verbose=TRUE)
{
  f = freqs.shrink(y, target=target, verbose=verbose)
  mi = mi.plugin(f, unit=unit)
  attr(mi, "lambda.freqs") = attr(f, "lambda.freqs") # shrinkage intensity

  return( mi )
}


## private function

get.lambda.shrink = function(n, u, t, verbose)
{
  # *unbiased* estimator of variance of u
  varu = u*(1-u)/(n-1)
  
  # misspecification
  msp = sum( (u-t)^2 )

  # estimate shrinkage intensity  
  if (msp == 0)
  {
    #warning("Overshrinkage")
    lambda = 1
  }
  else
    lambda = sum( varu ) / msp
  
  if (lambda > 1)
  {
    lambda = 1 # truncate at 1
    #warning("Overshrinkage")
  }
  
  if (lambda < 0)
  {
    lambda = 0
    #warning("Undershrinkage")
  }
  
  if (verbose)
  {
    cat(paste("Estimated shrinkage intensity lambda: ", 
      round(lambda, 4)) , "\n")
  }

  return(lambda)
}




