BX=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  N=sum(hacim);
  genelortalama=sum(hacim*ortalama)/N;
  f=hacim/N;

  df1=(sum((1-f)*varyans))^2/((sum(varyans*f))^2+sum((varyans^2)*(1-2*f)));
  df2=((sum((1-f)*varyans))^2)/((sum(varyans^2*(1-f)^2))/sum(hacim-1));

  BF=(sum(hacim*((ortalama-genelortalama)^2)))/(sum((1-(hacim/N))*varyans));

  pvalue=1-pf(BF,df1,df2);
  return(list(test.statistic=BF,p.value=pvalue))
}

