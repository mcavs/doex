AF=function(data,group){

  hacim <- tapply(data, group, length)
  grupsayisi <- length(tapply(data, group, length))
  ortalama <- tapply(data, group, mean)
  varyans <- tapply(data, group, var)
  N=sum(hacim);

  AF=((N-grupsayisi)/(grupsayisi-1))*((sum(hacim*(ortalama-mean(ortalama))^2))/(sum((hacim-1)*varyans)));
  c=((N-grupsayisi)/(N*(grupsayisi-1)))*(sum((N-hacim)*varyans)/sum((hacim-1)*varyans));

  df1=((sum((1-hacim/N)*varyans))^2)/((sum(varyans^2))+(sum(hacim*varyans/N))^2-(2*sum(hacim*varyans^2/N)));
  df2=(sum((hacim-1)*varyans))^2/(sum((hacim-1)*varyans^2));

  pvalue=1-pf(AF/c,df1,df2);
  return(list(test.statistic=AF,p.value=pvalue))
}
