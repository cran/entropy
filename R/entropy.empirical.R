### entropy.empirical.R  (2008-08-20)
###
###    Empirical entropy estimator
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


# compute empirical entropy 
# y is a vector of counts (may include zeros)
entropy.empirical = function(y, unit=c("log", "log2", "log10"))
{
  return( entropy.plugin(freqs.empirical(y), unit=unit) )
}

# empirical frequencies
freqs.empirical = function(y)
{
  return( y/sum(y) )
}

