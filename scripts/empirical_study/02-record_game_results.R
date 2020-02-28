
### record labelled results ###

# setup

rm(list = ls())

library(data.table)
devtools::load_all()

# specify the parameters for the study:

set.seed(102030)

load('hist_dist_key.RData')

##### import labels that were not anonymous: #####

names = c() # you can use this if you have different people labelling histograms and you also want to derive for each name separately
sheets = c() # there are 19 sheets out there.

# Name1:

nn  = 'Name1'

names = c(names,nn)

index_vec = 1:56 # which histograms did Person1 label?
sheet = (index_vec[1] - 1)/56 + 1 # assumes the default, that 56 histograms are printed on one sheet (back and front)
sheets = c(sheets,sheet)

lab_vec = c(F,F,T,T,T,T,F,F,F,F,T,T,T,F,T,T,F,F,F,T,F,T,F,NA,T,T,F,T,F,T,F,F,F,T,F,T,F,F,F,T,T,T,T,F,F,T,T,F,F,T,F,T,F,T,T,F) # how did the person label? Put TRUE (or T) if the histogram was believed to be uniform, put NA for missing labels

length(index_vec) == length(lab_vec) # quality check: did you record as many values as there are on this sheet?

assign(paste0('dt_',sheet),data.table(name = nn,index = index_vec,label = lab_vec))


### continue for all sheets: ###

names = c(names,'Name2') # A name may label multiple sheets


index_vec = 57:112
sheet = (index_vec[1] - 1)/56 + 1
sheets = c(sheets,sheet)

lab_vec = c(T,F,T,F,rep(NA,3),F,NA,T,NA,F,F,rep(NA,8),T,rep(NA,5),F,rep(NA,28))

length(index_vec) == length(lab_vec)

assign(paste0('dt_',sheet),data.table(name = 'Name2',index = index_vec,label = lab_vec))

#

index_vec = 113:168
sheet = (index_vec[1] - 1)/56 + 1
sheets = c(sheets,sheet)

lab_vec = c(NA,NA,F,NA,rep(NA,8),F,F,T,F,T,F,T,T,rep(NA,6),F,NA,rep(NA,28))

length(index_vec) == length(lab_vec)

assign(paste0('dt_',sheet),data.table(name = 'Name2',index = index_vec,label = lab_vec))



####################################

###   merge and save    ###

dt_lab = data.table()

for(s in sort(sheets))
{
  dt_temp = get(paste0('dt_',s))
  if(!('name' %in% colnames(dt_temp)))
  {
    dt_temp[,name := 'anonymous']
  }
  dt_lab = rbindlist(list(dt_lab,dt_temp))
}


dt = merge(dt,dt_lab,by = 'index',all = TRUE)

save(dt,file = 'results_histogram_labelling.RData')

