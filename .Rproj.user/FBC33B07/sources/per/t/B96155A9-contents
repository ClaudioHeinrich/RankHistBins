

### analyzes the results of the histogram labelling. Note that the example labels provided are purely for illustration and do not lead to the results in the paper, or any reasonable results at all... ###

# setup

rm(list = ls())

library(data.table)
library(ggplot2)
devtools::load_all()

# specify the parameters for the study:

set.seed(102030)

a = load('results_histogram_labelling.RData')


# exploratory:

ks = unique(dt[,k])
Dists = unique(dt[,Dist])
names = unique(dt[!is.na(name),name])

dt_expl = as.data.table(expand.grid(k = ks, Dist = Dists, name = names))

for(kk in ks)
{
  for(DD in Dists)
  {
    for(nn in names)
    {
      dt_expl[k == kk & Dist == DD & name == nn,
              accepts := dt[k == kk & Dist == DD & name == nn][(label),.N]]
      dt_expl[k == kk & Dist == DD & name == nn,
              rejects := dt[k == kk & Dist == DD & name == nn][(!label),.N]]
    }
  }
}

setkey(dt_expl,'Dist','k')

dt_expl = dt_expl[!is.na(accepts) & !is.na(rejects)]

# some plots:

dt_new = dt_expl[, .(sum(accepts)/(sum(accepts) + sum(rejects)),sum(rejects)/(sum(accepts) + sum(rejects))), by = Dist]

plot(dt_new[,.(Dist,V1)],type = 'l', col = 'blue',ylim = c(0,1),main = 'accepts by dist.')

# accepts by k:

dt_new = dt_expl[, .(sum(accepts)/(sum(accepts) + sum(rejects)),sum(rejects)/(sum(accepts) + sum(rejects))), by = k]

plot(dt_new[,.(k,V1)],type = 'l', col = 'blue',ylim = c(0,1),main = 'accepts by k.')

# accepts by labeller:

dt_new = dt_expl[, .(sum(accepts)/(sum(accepts) + sum(rejects)),sum(rejects)/(sum(accepts) + sum(rejects))), by = name]

plot(dt_new[,.(name,V1)], col = 'blue',ylim = c(0,1),main = 'accepts by labeler.')



############################################

### plot for acceptence rate by distance ###

plot_dir = paste0('./plots/')


title_fs = 20 # title font size
ax_fs = 16 # axis font size
ticks_fs = 14 #ticks font size


dt_new = dt_expl[, .(sum(accepts)/(sum(accepts) + sum(rejects)),sum(rejects)/(sum(accepts) + sum(rejects))), by = .(Dist,k)]

pp = ggplot(dt_new) + geom_line(aes(x = Dist,y = V1,colour = factor(k),linetype = factor(k)))

pp = pp + xlab(expression('D(H'['k']*')')) + ylab('acceptance rate') + ggtitle('Acceptance rate by distance') + scale_colour_discrete(name = 'k') +  scale_linetype_discrete(name = 'k')

pp = pp + theme(title = element_text(size = title_fs)) +
  theme(axis.title = element_text(size = ax_fs)) + theme(legend.title = element_text(size = ax_fs)) +
  theme(axis.text = element_text(size = ticks_fs)) + theme(legend.text = element_text(size = ticks_fs))

pdf(paste0(plot_dir,'Acceptance_by_distance.pdf'))

  print(pp)

dev.off()




#### minimize the misclassification rate ####

library(stats)

dt = dt[!is.na(label)]

c_vec = Dists

# roc curve: positive means the classifier accepts as well as the Statistician

tprs = c()
fprs = c()

for(c in seq(0,1,length.out = 100))
{
TPR = dt[Dist <= c & label == T,.N]/dt[label == T,.N]
tprs = c(tprs,TPR)

FPR = dt[Dist <= c & label == F,.N]/dt[label == F,.N]
fprs = c(fprs,FPR)
}

plot(x = fprs,y = tprs,type = 'l')

#AUC:

vec_int = function(x,y)
{
  dx = x[2:length(x)] - x[1:(length(x)-1)]
  mv_y = (y[2:length(x)] + y[1:(length(x)-1)])/2
  return(sum(mv_y * dx))
}

auc = vec_int(fprs,tprs)

print(auc)

### misclassification rate

mcrs = c()
for(c in c_vec)
{
FP = dt[Dist <= c & label == F,.N]
FN = dt[Dist > c & label == T,.N]
MCR = (FP + FN)/dt[,.N]
mcrs = c(mcrs,MCR)
}

### ggplot misclassification rate ###

plot_dt = data.table(c = c_vec,mc = mcrs )

spline_int <- as.data.frame(spline(plot_dt$c, plot_dt$mc))

pp = ggplot(plot_dt) + geom_point(aes(x = c, y = mc),size = 2) + geom_line(data = spline_int, aes(x = x, y = y),linetype = 2,alpha = 0.5)

pp = pp + ylab('misclassification rate') + ggtitle(expression('Misclassification rate of C'['c']))

pp = pp + theme(title = element_text(size = title_fs)) +
  theme(axis.title = element_text(size = ax_fs)) +
  theme(axis.text = element_text(size = ticks_fs))

pdf(paste0(plot_dir,'MCrate_by_c.pdf'))

  print(pp)

dev.off()


