


##### derive distribution of D(H_k) for a range of bin numbers, when X_1,...,X_n are uniformly distributed. #######

# This script derives the distribution D(H_k) explicitly, NOT using Monte-Carlo approximation. For reasonable runtimes you should set n <= 80 and k <= 10

##########################################

##### setup ####

rm(ls = ls())

library(data.table)


########## auxiliary functions ##############



# given a vector of natural numbers x_i that sum to n, return the probability of this vector under a uniform multinomial distribution:

prob_xs = function(x_vec, k = length(x_vec), n = sum(x_vec))
{
  p = factorial(n)/((k^n)*prod(factorial(x_vec)))
}


# given a vector of k natural numbers x_i that sum to n, compute the corresponding distance from uniformity:

dist_xs = function(x_vec, k = length(x_vec), n = sum(x_vec))
{
  hs = k * x_vec/n
  return(sum(abs(hs-1))/k)
}


# The number of possible vectors (x_1,...,x_k) is large (proportional to k^n)
# In order to reduce computational costs, we consider only the case x_1 <= x_2 <= ... <= x_n.
# Thus, at the end we need to multiply the probability by the number of permutations leading to new elements of {0,...,n}^k.
# This number is derived by the following function:

n_perm = function(x_vec, k = length(x_vec))
{
  t_vec = as.vector(table(x_vec)) # how often does each element occur?
  return(factorial(k)/prod(factorial(t_vec)))
}


# The following function finds recursively all possible combinations for x_1 <= x_2 <= ... <= x_k such that x_1 + ... + x_k = n.

all_xs = function(k,n){

  if(n < 0)
  {
    return(NULL)
  }else if (k == 1)
    {
    return(as.matrix(n))
    }else if(k > 1)
    {
      ret_val = NULL
      for(i in 0:n){
        red_val = all_xs(k = k-1, n = n - k*i)

        if(!is.null(red_val))
        {
          nr = nrow(red_val)
          ret_val = rbind(ret_val,matrix(c(rep(i,nr), i + red_val),nrow = nr))
        }
      }
      return(ret_val)
    }
}



get_sub_dt = function(dt,kk,k_max)
{
  if(kk < k_max)
  {
    return(copy(dt[get(paste0('x',k_max - kk)) == 0][,1:(k_max - kk) := NULL]))
  } else
  {return(dt)
      }
}

# auxiliary function: match with tolerance, search for the maximum value in x that matches ref, fill in all zeros with the last non-zero number:

select_in <- function(x, ref, tol=1e-6){
  testone <- function(value) abs(x - value) < tol
  maxwhich = function(y)
    {
    if(sum(y) == 0) return(0)
    else max(which(y))
    }
  ret_val = apply(sapply(ref, testone),MARGIN = 1,FUN = maxwhich)

  temp = cumsum(ret_val != 0)
  min_zero_ind = min(which(temp != 0))
  end = length(temp)

  ret_val[min_zero_ind : end] = ret_val[ret_val != 0][temp[min_zero_ind:end]]

  return(ret_val)

}

###################################







##### get cdf for a range of ks and ns, note that the support for the cdf is always slightly different


k_vec = 2:7
n_vec = seq(10,50,by = 10)

results = data.table()


for(n in n_vec)
{

  # get all possible combinations

  dt = as.data.table(all_xs(max(k_vec),n))

  setnames(dt,paste0('x',1:ncol(dt)))

  for(k in k_vec)
  {
    print(paste0('n = ',n,', k = ',k))

    ddt = get_sub_dt(dt,k,max(k_vec))

    # get the corresponding probabilities, distances from uniformity and permutations that lead to new elements of {0,...,n}^k:

    probs = apply(ddt, MARGIN = 1, FUN = prob_xs, n = n, k = k)
    dists = apply(ddt, MARGIN = 1, FUN = dist_xs, n = n, k = k)
    n_perms = apply(ddt, MARGIN = 1, FUN = n_perm, k = k)

    # add to ddt
    ddt[, probs := probs]
    ddt[, dists := dists]
    ddt[,nperm := n_perms]

    # get cdf

    setkey(ddt,dists)
    ddt[,cdf :=  cumsum(nperm *probs)]

    c_supp = seq(0,2*(k-1)*n,2)/(k*n)

    c_inds = select_in(c_supp,ddt[,dists])

    # initialize cdf

    ccdf = c(c_inds[c_inds == 0], ddt[c_inds[c_inds != 0], cdf])

    if(c_inds[1] == 0) ccdf[1] = 0
    if(c_inds[1] != 0) ccdf[1] = ddt[c_inds[1], cdf]

    ccdf[c_inds != 0] = ddt[c_inds[c_inds != 0], cdf]

    dt_temp = data.table(kk = k, nn = n, dist = c_supp, cdf = ccdf)

    results = rbindlist(list(results,dt_temp))
  }

}

save(results,file = 'distdist.RData')
