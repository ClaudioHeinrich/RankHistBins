

#### analyze the results from the distance distribution ####
# setup

rm(list = ls())

setwd('~/NR/ProjectRankHistograms/simstudy')

library(data.table)
library(ggplot2)

#########################

# get data

load(file = 'distdist2.RData')
results2 = copy(results)
load(file = 'distdist.RData')

results = rbindlist(list(results,results2))

######################

# for a range of alpha and c and n and k test whether the histogram test based on n,k,c has size alpha:

alpha_vec = c(0.05,0.1,0.33)

c_vec = seq(0.1,0.4,by = 0.05)

n_vec = results[,unique(nn)]
k_vec = results[,unique(kk)]

c_alpha_dt = as.data.table(expand.grid( k = k_vec, n = n_vec, c = c_vec, alpha = alpha_vec))

for(cc in c_vec)
{
  for(aa in alpha_vec)
  {
    print(paste0('c = ',cc,', alpha = ',aa))
    for(nnn in  n_vec)
    {
      for(kkk in k_vec)
      {
        prob_dist_above_c = 1 - results[kk == kkk & nn == nnn][min(which(dist >= cc)),cdf]
        c_alpha_dt[k == kkk & n == nnn & c == cc & alpha == aa, level_alpha := (aa >= prob_dist_above_c)]
      }
    }
  }
}

c_alpha_dt[(level_alpha), max_k := max(k),by = .(n,c,alpha)]

###  plot k_opt as function of n ###

for(aa in  alpha_vec)
{
  pp = ggplot(data = c_alpha_dt[!is.na(max_k) & alpha == aa],aes(y = max_k,x = n,color = factor(c),linetype = factor(c),shape = factor(c),alpha = 0.75)) + geom_line() + geom_point(size = 2)

  pp = pp +ggtitle(paste0('alpha = ',aa))

  print(pp)
}

############

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




###  plot n_min as function of k ###

c_sel = c(0.2,0.3,0.4)

for(aa in  alpha_vec)
{
  pp = ggplot(data = min_n_dt[!is.na(n) & alpha == aa & intol(c,c_sel)],aes(y = n,x = k,color = factor(c),linetype = factor(c),shape = factor(c))) + geom_line() + geom_point(size = 1)

  pp = pp +ggtitle(paste0('alpha = ',aa))

  print(pp)
}

############

selected_k = c(2:6,8,10,12)

aa = 0.05

min_n_dt[!is.na(n) & k %in% selected_k & intol(c,  c(0.2,0.3,0.4)) & alpha  == aa]


###############################
# The following plots show why we're jumping back down every now and then: The cdfs are discontinuous and since the support varies of Dist(H_k) varies in n and k they jump at different times.

dev.off()

plot(results[nn %in% 12 & kk %in% 2,.(dist,cdf)],type = 'l')
lines(results[nn %in% 13 & kk %in% 2,.(dist,cdf)],lty = 2)


### check whether for fixed n, the power is increasing in k ###

c_dt = as.data.table(expand.grid( k = k_vec, n = n_vec[n_vec >= 50], c = c_vec, alpha = alpha_vec))

for(cc in c_vec)
{
  print(paste0('c = ',cc,', alpha = ',aa))
  for(nnn in  n_vec[n_vec >= 50])
  {
    for(kkk in k_vec)
    {
      prob_dist_above_c = 1 - results[kk == kkk & nn == nnn][min(which(dist >= cc)),cdf]
      c_dt[k == kkk & n == nnn & c == cc , power := prob_dist_above_c]
    }
  }
}


for(cc in c_vec)
{
pp = ggplot(data = c_dt[n %in%  n_vec[n_vec >= 50] & c == cc],
            aes(x = k,y = power,color = factor(n),linetype = factor(n),shape = factor(n),alpha = 0.75)) +
  geom_line() + geom_point(size = 2)

pp = pp +ggtitle(paste0('c = ',cc))

print(pp)
}
