

#### analyze the results from the distance distribution for the supplementary material ####
# setup

rm(list = ls())

#setwd('~/Documents/NR/ProjectRankHistograms/simstudy')

library(data.table)
library(ggplot2)

#########################

plot_dir = '../../Figures/' # adjust to your file system

dir.create(plot_dir,showWarnings = FALSE)

# font size specifications:

fac = 1 # multiplicator to scale all font sizes equally

title_size = 20 * fac
labels_size = 16 * fac
ticks_size = 12 * fac


# get data

load(file = './Data/distdist_approx_variousXdist.RData')

######################

# for a range of alpha and c and n and k test whether the histogram test based on n,k,c has size alpha:

alpha_vec = c(0.05,0.1,0.33)

c_vec = c(0.2,0.25,0.3)

n_vec = results_unif[,unique(n)]
k_vec = results_unif[,unique(k)]

supp_ecdf = as.numeric(colnames(results_unif)[3:ncol(results_unif)]) # support of the ecdf

for(distr in c('unif','F1','F2'))
{

  results = copy(get(paste0('results_',distr)))

  if(distr == 'unif') distr_expr = 'uniform distribution'
  if(distr == 'F1') distr_expr = 'sloped'
  if(distr == 'F2') distr_expr = 'U-shaped'



  c_dt = as.data.table(expand.grid( k = k_vec, n = n_vec, c = c_vec))

  for(cc in c_vec)
  {
    print(paste0('c = ',cc))
      for(nn in  n_vec)
      {
        for(kk in k_vec)
        {
          ecdf = matrix(results[k == kk & n == nn,3:ncol(results)])

          # get the probability to be above or equal c
          rel_ind = min(which(supp_ecdf >= cc))
          prob_dist_above_c = 1 - unlist(ecdf[rel_ind])

          c_dt[k == kk & n == nn & c == cc , power :=  prob_dist_above_c]
        }
      }

  }

  assign(paste0('c_dt_',distr),c_dt)

  for(cc in c_vec)
  {
    pp = ggplot(data = c_dt[n %in% c(30,50,70,100) & c == cc],
                aes(x = k,y = power,color = factor(n),linetype = factor(n),shape = factor(n))) +
      geom_line() + geom_point(size = 2)

    pp = pp + scale_x_continuous(breaks = 2:12, labels = 2:12) + theme(panel.grid.minor = element_blank()) + ylim(c(0,1))


    pp = pp +ggtitle(paste0(distr_expr,', c = ',cc)) + labs(color = 'n', linetype = 'n',shape = 'n')

    pp = pp + theme(plot.title = element_text(size = title_size),
                    axis.title = element_text(size = labels_size),
                    axis.text = element_text(size = ticks_size),
                    legend.title = element_text(size = labels_size),
                    legend.text = element_text(size = ticks_size))

    #pdf(file = paste0(plot_dir,'power_',distr,'_c',100* cc,'.pdf'))
      print(pp)
    #dev.off()
  }
}
