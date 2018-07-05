MW=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  c=(hacim-1)/(hacim-3);
  w=hacim/(c*varyans);
  h=w/sum(w);


  AWT1=sum(w*(ortalama-sum(h*ortalama))^2);
  AWT2=(grupsayisi-1)+((2*((grupsayisi-2)/(grupsayisi+1))*sum((1-h)^2/(hacim-1))));
  AWT=AWT1/AWT2;

  df=((grupsayisi^2-1)/3)/sum((1-h)^2/(hacim-1));

  pvalue=1-pf(AWT,grupsayisi-1,df);

  return(list(test.statistic=AWT,p.value=pvalue))
}
