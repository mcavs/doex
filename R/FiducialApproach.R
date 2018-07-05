FA=function(data,group,rept=10000){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  N=sum(hacim);

  ro=sum(hacim*ortalama^2/varyans)-((sum(hacim*ortalama/varyans))^2)/(sum(hacim/varyans));

  p=0;
  for(i in 1:rept){
    t=rt(grupsayisi,hacim-1);
    rl=sum(t^2)-(((sum(sqrt(hacim)*t/sqrt(varyans)))^2)/(sum(hacim/varyans)));
    if(rl>=ro){p=p+1}
  }
  pvalue=p/rept;
  return(list(p.value=pvalue))
}
