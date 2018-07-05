OS=function(data,group,nout=1,rept=10000){

  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)
  N=sum(hacim);

  vary=function(data){return(var(sample(data,length(data)-nout)))}
  varyans.=tapply(data, group, vary)

  maxvar=max(varyans.);
  maxvar.=max(varyans./hacim);

  U=(1/hacim)+(1/hacim*sqrt((1/(hacim-1))*((maxvar/varyans.)-1)))
  V=(1/hacim)-(1/hacim*sqrt(((hacim-1))*((maxvar/varyans.)-1)))

  #gruplara ayırma
  a=1;b=0;top=0;data.=numeric(length(data));
  for(i in 1:grupsayisi){
    b=b+hacim[i];
    data.[a:(b-1)]=data[a:(b-1)]*U[i];
    data.[b]=data[b]*V[i];
    a=b+1;
  }
  ###
  ortalama.=tapply(data., group, sum)
  ortalama..=sum(ortalama.)/grupsayisi;
  F=sum(((ortalama.-ortalama..)/sqrt(maxvar.))^2);

  p=0;
  for(i in 1:rept){
    t=rt(grupsayisi,hacim-2);
    Q=sum((t-mean(t))^2);
    if(Q>F){p=p+1}
  }
  pvalue=p/rept;
  return(list(p.value=pvalue));
}
