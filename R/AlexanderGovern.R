AG=function(data,group){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)

  w=(hacim/varyans)/sum(hacim/varyans);
  genelortalama=sum(w*ortalama);
  t=(ortalama-genelortalama)/sqrt(varyans/hacim);
  v=hacim-1;
  a=v-0.5;
  b=48*a^2;
  c=sqrt(a*log(1+(t^2/v)));
  z=c+(((c^3)+(3*c))/b)-((4*c^7+33*c^5+240*c^3+855*c)/(10*b^2+8*b*c^4+1000*b));

  AD=sum(z^2);

  pvalue=1-pchisq(AD,grupsayisi-1);
  return(list(test.statistic=AD,p.value=pvalue))
}
