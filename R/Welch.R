WE=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  agirlik1=hacim/varyans;
  agirlik2=agirlik1/sum(agirlik1);

  W1=sum(agirlik1*(ortalama-sum(agirlik2*ortalama))^2);
  W2=(grupsayisi-1)+((2*((grupsayisi-2)/(grupsayisi+1))*sum((1-agirlik2)^2/(hacim-1))));
  W=W1/W2;

  df=((grupsayisi^2-1)/3)/sum((1-agirlik2)^2/(hacim-1));

  pvalue=1-pf(W,grupsayisi-1,df);

  return(list(test.statistic=W,p.value=pvalue))
}
