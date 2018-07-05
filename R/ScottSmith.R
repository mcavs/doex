SS=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  varyans=varyans*((hacim-1)/(hacim-3));

  SS=sum((hacim*(ortalama-mean(data))^2)/varyans);

  pvalue=1-pchisq(SS,grupsayisi);
  return(list(test.statistic=SS,p.value=pvalue))
}
