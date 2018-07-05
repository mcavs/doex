B2=function(alpha,data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)

  w=(hacim/varyans)/sum(hacim/varyans);
  genelortalama=sum(w*ortalama);
  t=(ortalama-genelortalama)/sqrt(varyans/hacim);
  v=hacim-1;
  zc=qnorm(1-alpha/2);
  c=(4*v^2+((10*zc^2+15)/(24)))/((4*v^2)+v+((4*zc^2+9)/(12)))*sqrt(v)
  z=c*sqrt(log(1+(t^2/v)));

  BK=sum(z^2);

  pvalue=1-pchisq(BK,grupsayisi-1);
  return(list(test.statistic=BK,p.value=pvalue))
}

