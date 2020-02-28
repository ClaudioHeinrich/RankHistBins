


##### derive distribution of D(H_k) under null hypothesis, i.e. when X_i are uniformly distributed. ######

# This script uses Monte-Carlo approximation to derive the distribution of D(H_k) for a wide range of k and n.
# For small n and k the distribution can be derived explicitly, see ./side/01-derive_dist_dist_explicitly


##########################################

##### setup ####

rm(ls = ls())

library(data.table)

set.seed(1)

########## auxiliary functions ##############

get_dist = function(data,k)
{
  # data is a vector containing a uniform sample, or a matrix the rows of which are uniform samples of size n.
  # The function returns the distance to uniformity of a histogram with k bins based on this data.

  if(!('matrix' %in% is(data)))
  {
    data = matrix(data,nrow = 1)
  }

  nr = nrow(data)
  n = ncol(data)

  which.bin = ceiling(data * k)

  hs = NULL

  for(i in 1:k)
  {
    h_i = (k/n) * rowSums(which.bin == i)
    hs = c(hs,h_i)
  }
  hs = matrix(hs,nrow = nr)

  dists = rowMeans(abs(hs-1))

  return(dists)

}



###########################################################

##### get cdf for a range of ks and ns


k_vec = 2:12
n_vec = seq(10,200,by = 10)

N = 1e+5 #number of samples to consider for Monte-Carlo approximation: Note that a 'sample' is a collection of n uniformly distributed points.

supp_ecdf = seq(0,1.2, by = 0.01) # the ecdf is evaluated at these values

unif_data = matrix(runif(N * max(n_vec)),nrow = N)

ecdf_vals = c()

for(n in n_vec)
{
  print(n)
  for(k in k_vec)
  {
    dists =  get_dist(data = unif_data[,1:n], k = k)

    ecdf_vals = c(ecdf_vals, ecdf(dists)(supp_ecdf))
  }
}

ecdf_vals = matrix(ecdf_vals,nrow = length(supp_ecdf))

results = data.table(expand.grid(k = k_vec,n = n_vec) , t(ecdf_vals))

setnames(results,c('k','n',supp_ecdf))

save(results,N,file = './Data/distdist_approx.RData')
