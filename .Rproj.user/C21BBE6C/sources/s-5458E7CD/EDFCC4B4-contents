

#### analyze the results from the distance distribution ####


# setup

rm(list = ls())

library(data.table)
library(ggplot2)

#########################

plot_dir = '../../Figures/' # adjust this to your local File structure

dir.create(plot_dir,showWarnings = FALSE)

# get data

load(file = './Data/distdist_approx.RData')

######################

# for a range of alpha and c and n and k, test whether the histogram test based on n,k,c has size alpha:

alpha_vec = c(0.05,0.1,0.33)

c_vec = c(0.2,0.25,0.3)

n_vec = results[,unique(n)]
k_vec = results[,unique(k)]

supp_ecdf = as.numeric(colnames(results)[3:ncol(results)])

c_alpha_dt = as.data.table(expand.grid( k = k_vec, n = n_vec, c = c_vec, alpha = alpha_vec))

for(cc in c_vec)
{
  for(aa in alpha_vec)
  {
    print(paste0('c = ',cc,', alpha = ',aa))
    for(nn in  n_vec)
    {
      for(kk in k_vec)
      {
        ecdf = matrix(results[k == kk & n == nn,3:ncol(results)])

        # get the probability to be above or equal c
        rel_ind = min(which(supp_ecdf >= cc))
        prob_dist_above_c = 1 - unlist(ecdf[rel_ind])

        c_alpha_dt[k == kk & n == nn & c == cc & alpha == aa, level_alpha := (aa >= prob_dist_above_c)]
      }
    }
  }
}

c_alpha_dt[(level_alpha), max_k := max(k),by = .(n,c,alpha)]



###  plot k_max as function of n ###

title_fs = 25 # title font size
ax_fs = 22 # axis font size
ticks_fs = 18 #ticks font size


for(aa in  alpha_vec)
{
  pp = ggplot(data = c_alpha_dt[!is.na(max_k) & alpha == aa]) +
    geom_line(mapping = aes(y = max_k, x = n, color = factor(c),linetype = factor(c))) +
    geom_point(mapping = aes(y = max_k, x = n, color = factor(c),shape = factor(c)),size = 2)

  pp = pp + ggtitle(substitute(alpha ~ '=' ~ aa,list(aa = aa))) + xlab('n')

  pp = pp + scale_y_continuous(name = 'k',breaks = k_vec,labels = as.character(k_vec)) + scale_x_continuous(breaks = c(seq(20,200,by=20))) +

  # legend

  if(aa != 0.33)
  {
   theme(legend.position = 'none')
  } else {
    labs(shape = 'c',linetype = 'c',color = 'c')
  }

  # control background grid

  pp = pp + theme(panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = 'white',size = 0.5) )

  # font sizes:

  pp = pp + theme(title = element_text(size = title_fs)) +
    theme(axis.title = element_text(size = ax_fs)) + theme(legend.title = element_text(size = ax_fs)) +
    theme(axis.text = element_text(size = ticks_fs)) + theme(legend.text = element_text(size = ticks_fs))


  pdf(paste0(plot_dir,'k_by_n_a',100*aa,'.pdf'))
    print(pp)
  dev.off()
}

#########################

# plot the false rejection probability as a function of n:

ks = c(4,6,9)

cs = c(0.2,0.25,0.3)

plot_dt = as.data.table(expand.grid(n = results[,unique(n)],k = ks, c = cs))

for(kk in ks)
{
  for(cc in cs)
  {
    plot_dt[k == kk & c == cc, p_val := 1 - results[k == kk,get(as.character(cc))]]
  }
}

#########################

for(cc in  cs)
{
  pp = ggplot(data = plot_dt[c == cc]) +
    geom_line(mapping = aes(y = p_val, x = n, color = factor(k),linetype = factor(k))) +
    geom_point(mapping = aes(y = p_val, x = n, color = factor(k),shape = factor(k)),size = 2)

  pp = pp + ggtitle(substitute('c =' ~ cc,list(cc = cc))) + xlab('n')

  pp = pp + scale_y_continuous(name = 'false rejection probability') + scale_x_continuous(breaks = c(seq(20,200,by=20)))

   # legend

   if(cc != 0.3)
    {
      pp = pp + theme(legend.position = 'none')
    } else {
      pp = pp + labs(shape = 'k',linetype = 'k',color = 'k')
    }


  # font sizes:

  pp = pp + theme(title = element_text(size = title_fs)) +
    theme(axis.title = element_text(size = ax_fs)) + theme(legend.title = element_text(size = ax_fs)) +
    theme(axis.text = element_text(size = ticks_fs)) + theme(legend.text = element_text(size = ticks_fs))


  pdf(paste0(plot_dir,'rp_by_n_c',100*cc,'.pdf'))
    print(pp)
  dev.off()
}


#######################################################################
### it follows some additional analysis not presented in the paper: ###


# for a range of alpha, c and k find the min n such that k_opt(n) = k

min_n_dt = as.data.table(expand.grid(k = k_vec,c = c_vec,alpha = alpha_vec))

for(aa in alpha_vec)
{
  for(cc in c_vec)
  {
    for(kk in 2: 12)
    {
      c_alpha_new = c_alpha_dt[(level_alpha),][k == kk & abs(c-cc) < 1e-8 & abs(aa-alpha) < 1e-8,]
      if(c_alpha_new[,.N] > 0)
      {
        min_n = c_alpha_new[,min(n)]
        min_n_dt[k == kk &  abs(c - cc) < 1e-8 & abs(alpha - aa) < 1e-8, n := min_n]
      }
    }
  }
}

# checkout results:

# %in% with tolerance that takes vectors test and reference and returns a logical vector of length test:

intol = function(test,reference,tol = 1e-8)
  {
  temp = function(x){sum(abs(x - reference) < tol) > 0}
  return(unlist(lapply(test,FUN = temp)))
  }

min_n_dt[!is.na(n)][intol(c,c(0.1,0.2,0.3,0.4))][ intol(alpha,0.05)]

#  plot n_min as function of k

c_sel = c(0.2,0.3,0.4)

for(aa in  alpha_vec)
{
  pp = ggplot(data = min_n_dt[!is.na(n) & alpha == aa & intol(c,c_sel)],aes(y = n,x = k,color = factor(c),linetype = factor(c),shape = factor(c))) + geom_line() + geom_point(size = 1)

  pp = pp +ggtitle(paste0('alpha = ',aa))

  print(pp)
}

