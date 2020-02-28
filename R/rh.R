#' Function for computing transformed ranks and generating rank histograms with any bin number
#'
#' @param data either a vector of length n containing the ranks of the observation or an n x m+1 - matrix containing all ranks, the observation ranks as first column.
#' @param ens_size ensemble size m. If this is NULL it is inferred from the data. This should be avoided when only the observation ranks are provided.
#' @param ... arguments passed on to the function hist.
#' @param seed The transformed ranks rely on randomization. You can fix the seed in order to make it reproducable.
#' @param plot Logical. If TRUE, a plot is generated, otherwise the transformed ranks are returned.
#'
#' @author Claudio Heinrich
#' @examples
#' r_i = sample(1:10,100,replace = TRUE)
#' rh(data = r_i, k = 3, ens_size = 10)

rh = function(data, k = max(data), ens_size = NULL,...,seed = NULL,plot = TRUE)
{
  if(!is.null(seed))
  {
    set.seed(seed)
  }

  if(is.matrix(data))
  {
    if(is.null(ens_size))
    {
      ens_size = max(data)
    }
    data = as.vector(data[,1])
  }

  if(is.null(ens_size))
  {
    warning('Ensemble size is not provided, so it is set to the maximum observed rank which may be smaller...')
    ens_size = max(data)
  }

  n = length(data)

  U_i = runif(n)

  transformed_ranks = (data - 1 + U_i)/(ens_size + 1)

  brks = seq(0,1,length.out = k+1)

  if(plot)
  {
    return(hist(x = transformed_ranks,breaks = brks,...))
  } else {return(transformed_ranks)}
}
