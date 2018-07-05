AGF=function(data,group,rept=10000){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  w=hacim/varyans;
  q=sqrt(w/sum(w));

  T=sum(w*(ortalama-sum(q*ortalama))^2);

  p=0;
  for(i in 1:rept){
    z=rnorm(grupsayisi);
    u=rchisq(grupsayisi,hacim-1);
    y.=sum(q*z);
    t=sum(((hacim-1)/u)*((z-q*y.)^2));
    if(t>=T){p=p+1}
  }
  pvalue=p/rept;
  return(list(p.value=pvalue))
}
