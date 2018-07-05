PF=function(data,group,rept=10000){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  Fobs=as.numeric(summary(aov(data~as.factor(group)))[[1]][["F value"]][1]);
  Fstar=numeric(rept);

  for(i in 1:rept){
    perm.group=sample(group);
    Fstar[i]=as.numeric(summary(aov(data~as.factor(perm.group)))[[1]][["F value"]][1])
  }

  pvalue=mean(Fstar>=Fobs);
  return(list(p.value=pvalue))
}
