###########################################################

# simulation study for supplementary material:
# This script computes and saves the distribution function for D(H_k) for a range of n and k, for the uniform distribution
# and two alternative distribution F_1 and F_2. F_1 is sloped and F_2 a parabel.


##########################################

##### setup ####

rm(list = ls())

library(data.table)

set.seed(1)

######################

plot_dir = '../Figures/'

# get data

load(file = 'distdist_approx.RData')



########## auxiliary functions ##############

get_dist = function(data,k)
{
  # data is a vector containing a uniform sample or a matrix the rows of which are samples of size n.
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



###################################

# F1_inv is the quantile function for density x/2 + 3/4

F1_inv = function(y, tol = 1e-8)
{
  # y is vector of inputs in [0,1]

  dist_fun = function(x) # distribution function of the mixture distribution
  {
    return((x^2)/4 + x*3/4)
  }

  # get inverse:
  quantile_fct = function(y)
  {
    uniroot(f = function(x){return(dist_fun(x) - y)},
            interval = c(0,1),
            tol = tol)[1]
  }
  quantile_fct_vec = function(y) unlist(lapply(y,quantile_fct))


  return(unname(quantile_fct_vec(y)))

}

# F2_inv is the quantile function for density (x-1/2)^2 + 11/12

F2_inv = function(y, tol = 1e-8)
{
  # y is vector of inputs in [0,1]

  dist_fun = function(x) # distribution function of the mixture distribution
  {
    return((x^3)/3 - (x^2)/2 + 14/12 * x)
  }

  # get inverse:
  quantile_fct = function(y)
  {
    uniroot(f = function(x){return(dist_fun(x) - y)},
            interval = c(0,1),
            tol = tol)[1]
  }
  quantile_fct_vec = function(y) unlist(lapply(y,quantile_fct))


  return(unname(quantile_fct_vec(y)))

}


##### get cdf for a range of ks and ns, note that the support for the cdf is always slightly different

k_vec = 2:12
n_vec = seq(10,200,by = 10)

N = 1e+5 #number of uniform datasets to consider for each n

supp_ecdf = seq(0,1.2, by = 0.01) # the ecdf is evaluated at these values


unif_data = matrix(runif(N * max(n_vec)),nrow = N)
F1_data = matrix(F1_inv(runif(N * max(n_vec))),nrow = N) # takes a little time, reduce N to run faster...
F2_data = matrix(F2_inv(runif(N * max(n_vec))),nrow = N) # takes a little time, reduce N to run faster...

# look at the distributions F_1 and F_2:
hist(as.vector(F1_data))
hist(as.vector(F2_data))


for(distr in c('unif','F1','F2'))
{
  ecdf_vals = c()
  print(distr)
  for(n in n_vec)
  {
    print(n)
    for(k in k_vec)
    {
      dists =  get_dist(data = get(paste0(distr,'_data'))[,1:n], k = k)

      ecdf_vals = c(ecdf_vals, ecdf(dists)(supp_ecdf))
    }
  }

  ecdf_vals = matrix(ecdf_vals,nrow = length(supp_ecdf))

  assign(paste0('results_',distr), value = data.table(expand.grid(k = k_vec,n = n_vec) , t(ecdf_vals)))

  setnames(get(paste0('results_',distr)),c('k','n',supp_ecdf))
}


save(results_unif,results_F1,results_F2,N,file = './Data/distdist_approx_variousXdist.RData')

