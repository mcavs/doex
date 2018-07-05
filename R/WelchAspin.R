WA=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)

  w=(hacim/varyans)/sum(hacim/varyans);
  genelortalama=sum(w*ortalama);
  t=(ortalama-genelortalama)/sqrt(varyans/hacim);
  v=hacim-1;
  a=sum((1-w)^2/v);

  WA=(sum(t^2)/(grupsayisi-1))/(1+((2*grupsayisi-2)/(grupsayisi^2-1))*a);

  pvalue=1-pf(WA,grupsayisi-1,(grupsayisi^2-1)/(3*a));
  return(list(test.statistic=WA,p.value=pvalue))
}

