

# This script can be used to generate histograms with different distances from uniformity that can be labelled in order to find an approximate to the acceptance threshold.
# In particular, if you want to derive your personal acceptance threshold, just label some histograms yourself.


# setup

rm(list = ls())

#setwd('~/NR/ProjectRankHistograms/simstudy/Rank')

library(data.table)
devtools::load_all()

plot_dir = './plots/'
dir.create(plot_dir,showWarnings = FALSE)

# specify the parameters for the study:

set.seed(102030)

n_tot = 100 # how many histograms are you willing to label

start_at = 0 # If you change your mind later on and want to label more just set this to the number of histograms you have already labelled.

ks = c(5,6,8,10) # which bin sizes do you want to consider?

Dists = c(seq(0.1,0.5,by = 0.05),0.6) # which distances do you want to consider?


# make total n divisible by rounding up:
n_tot = ceiling(n_tot /(length(ks)*length(Dists))) * length(ks)*length(Dists)

# number of histograms to plot per page:

nrow = 7

ncol = floor(nrow/sqrt(2))

n_pp = ncol * nrow


###############

#initialize data table

dt = data.table(k = rep(ks,n_tot / length(ks)), Dist = rep(Dists,each = n_tot / length(Dists)))

permutation = sample(n_tot,n_tot,replace = FALSE)

dt = dt[,index := permutation + start_at]

setkey(dt,index)



################

#create plots:

n_p = ceiling(n_tot / n_pp)

pdf(paste0(plot_dir,'Histograms.pdf'),width = 7*ncol,height = 7*nrow)

  par(mfrow = c(nrow,ncol),mar = c(4,4,4,4),cex = 1.5, oma = c(4,4,4,4))

  for(page_index in 1:n_p)
  {

    plot_indices = ((page_index-1) * n_pp + 1 + start_at) : min((page_index * n_pp + start_at),n_tot + start_at)

    for(i in plot_indices){
      k = dt[index == i,k]
      Dist = dt[index == i,Dist]
      dist_hist(Dist = Dist,k = k,main = i)
    }

  }

dev.off()

##### save the key, which histogram has what distance to uniformity for later analysis #####

save(dt,file = 'hist_dist_key.RData')
