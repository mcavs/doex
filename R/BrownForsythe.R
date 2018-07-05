BF=function(data,group){

  hacim <- tapply(data, group, length)
  grupsayisi <- length(tapply(data, group, length))
  ortalama <- tapply(data, group, mean)
  varyans <- tapply(data, group, var)

  B=sum(hacim*(ortalama-mean(data))^2)/sum((1-(hacim/sum(hacim)))*varyans);
  df1=sum((1-(hacim-sum(hacim)))*varyans)^2;
  df2=sum(((1-(hacim-sum(hacim)))^2*(varyans)^2)/(hacim-1));
  df=(df1/df2);

  pvalue=1-pf(B,grupsayisi-1,df);
  return(list(test.statistic=B,p.value=pvalue))
}

