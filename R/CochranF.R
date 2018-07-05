CF=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  w=hacim/varyans;
  h=w/sum(w);

  C=sum(w*(ortalama-sum(h*ortalama))^2);

  pvalue=1-pchisq(C,grupsayisi-1);
  return(list(test.statistic=C,p.value=pvalue))
}
