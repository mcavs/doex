JF=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  w=hacim/varyans;
  h=w/sum(w);
  ortalama.=sum(ortalama*w)/sum(w);

  A=sum((1-w/sum(w))^2/(hacim-1));
  c=(grupsayisi-1)+2*A-(6*A/(grupsayisi+1));
  v=(grupsayisi-1)*(grupsayisi+1)/(3*A);

  T=sum(w*(ortalama-sum(h*ortalama))^2);

  pvalue=1-pf(T/c,grupsayisi-1,v);
  return(list(p.value=pvalue))
}
