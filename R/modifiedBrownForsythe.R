MBF=function(data,group){

  hacim <- tapply(data, group, length)
  grupsayisi <- length(tapply(data, group, length))
  ortalama <- tapply(data, group, mean)
  varyans <- tapply(data, group, var)
  N=sum(hacim);

  B=sum(hacim*(ortalama-mean(data))^2)/sum((1-(hacim/sum(hacim)))*varyans);
  df11=(sum((1-(hacim-sum(hacim)))*varyans)^2);
  df12=sum(((1-(hacim-sum(hacim)))^2*(varyans)^2)/(hacim-1));
  df2=(df11/df12);
  df1=((sum((1-hacim/N)*varyans))^2)/((sum(varyans^2))+(sum(hacim*varyans/N))^2-(2*sum(hacim*varyans^2/N)));

  pvalue=1-pf(B,df1,df2);
  return(list(p.value=pvalue))
}

