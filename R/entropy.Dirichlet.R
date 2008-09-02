### entropy.Dirichlet.R  (2008-08-20)
###
###    Family of Dirichlet entropy estimators
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


# estimate entropy based on Dirichlet-multinomial pseudocount model 

# y:  a vector of counts (may include zeros)
# a:  pseudocount per bin

# some choices for a:
# a = 0          :   empirical estimate
# a = 1          :   Laplace
# a = 1/2        :   Jeffreys
# a = 1/m        :   Schurmann-Grassberger  (m: number of bins)
# a = sqrt(n)/m  :   minimax

entropy.Dirichlet = function(y, a, unit=c("log", "log2", "log10"))
{
  return( entropy.plugin(freqs.Dirichlet(y, a), unit=unit) )
}

freqs.Dirichlet = function(y, a)
{
  ya = y+a          # counts plus pseudocounts
  na = sum(ya)      # total number of counts plus pseudocounts
  pa = ya/na        # empirical frequencies adjusted with pseudocounts

  return(pa)
}


