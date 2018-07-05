PB=function(data,group,rept=10000){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)

  sb=sum(hacim*ortalama^2/varyans)-((sum(hacim*ortalama/varyans))^2)/(sum(hacim/varyans));

  p=0;
  for(i in 1:rept){
    z=rnorm(grupsayisi);
    u=rchisq(grupsayisi,hacim-1);
    PB=(sum(z^2*(hacim-1)/u))-(((sum((sqrt(hacim)*z*(hacim-1))/(sqrt(varyans)*u)))^2)/(sum(hacim*(hacim-1)/(varyans*u))));
    if(PB>sb){p=p+1}
  }
  pvalue=p/rept;
  return(list(p.value=pvalue))
}
