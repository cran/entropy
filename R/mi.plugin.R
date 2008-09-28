### mi.plugin.R  (2008-09-28)
###
###    Plug-in mutual information estimator
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


mi.plugin = function(freqs2d, unit=c("log", "log2", "log10"))
{
  unit = match.arg(unit)

  freqs2d = as.matrix(freqs2d/sum(freqs2d)) # just to make sure ...

  freqs.x = apply(freqs2d, 1, sum) # marginal frequencies
  freqs.y = apply(freqs2d, 2, sum)

  H.xy = entropy.plugin(freqs2d, unit=unit)
  H.x = entropy.plugin(freqs.x, unit=unit)
  H.y = entropy.plugin(freqs.y, unit=unit)
  
  MI = H.x + H.y - H.xy

  return(MI)
}

