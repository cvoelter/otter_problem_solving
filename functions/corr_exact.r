#function deriving an exact test of a correlation coefficient (also in case tied observations do exist)
#written by Roger Mundry
#version Oct. 29, 2011
#arguments are
#x, y: vectors with numbers (are assumed to be aligned correctly)
#corr.method: the correlation coefficient applied (possible are "spearman" (the default), "kendall", and "pearson"
#exact.thresh: for samples larger than this value an approximate permutation test is applied
#(default is 9, and I recommend to stick with this)
#n.appr.perm: number of permuations used in the approximate case
#(default is 10000, and I recommend to stick with this or enlarge it)
#returns a data frame which should be self explaining
require(gtools)
corr.exact<-function(x, y, corr.method="spearman", exact.thresh=9, n.appr.perm=10000){
  is.not.na=(!is.na(x))&(!is.na(y))
  x=x[is.not.na]
  y=y[is.not.na]
  N=length(x)
  if (N>1 & sd(x)>0 & sd(y)>0){
    rho.orig=cor(x,y,method=corr.method)
    rho.perm=c()
    if (N<exact.thresh){
      perms=permutations(N,N)
      for (i in 1:nrow(perms)){
        rho.perm=c(rho.perm,cor(x[perms[i,]],y,method=corr.method))
      }
      nperms=nrow(perms)
      method="exact"
    }else{
      perm=1:length(x)
      l.perm=length(x)
      nperms=n.appr.perm
      method="approximate"
      rho.perm=rho.orig
      for (i in 1:(nperms-1)){
        perm=sample(perm,l.perm,replace=F)
        rho.perm=c(rho.perm,cor(x[perm],y,method=corr.method))
      }
    }
    remark=""
    result=data.frame(rho=rho.orig,N=N,P=sum(abs(rho.perm)>=abs(rho.orig))/nperms,n_perms=nperms, method, correlation_method=corr.method, remark="none")
  }else{
    if(N<2){
      remark="not tested because sample too small"
    }else if(sd(x)==0 | sd(y)==0){
      remark="not tested because variance in x and/or y is zero"
    }
    result=data.frame(rho=NA,N=N,P=NA,n_perms=NA, method="none", correlation_method="none", remark=remark)
  }
  row.names(result)=""
  return(result)
}
