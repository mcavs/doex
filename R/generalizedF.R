GF=function(data,group,rept=10000){
  hacim=tapply(data, group, length)
  grupsayisi=length(tapply(data, group, length))
  ortalama=tapply(data, group, mean)
  varyans=tapply(data, group, var)

  v=matrix(0,grupsayisi,rept);
  b=matrix(0,grupsayisi,rept);
  toplam1=numeric(rept);
  toplam2=numeric(rept);
  terim=matrix(0,grupsayisi,rept);
  terimtoplami=numeric(rept);
  for(i in 1:grupsayisi){
    v[i,]=rchisq(rept,as.numeric(hacim[i])-1);
    b[i,]=(hacim[i]/((hacim[i]-1)*varyans[i]))*v[i,];
    toplam1=toplam1+(b[i,]*ortalama[i]);
    toplam2=toplam2+b[i,];
  }
  for(i in 1:grupsayisi){
    terim[i,]=b[i,]*(ortalama[i]-toplam1/toplam2)^2;
    terimtoplami=terimtoplami+terim[i,];
  }
  U=rchisq(rept,grupsayisi-1);
  pvalue=mean(U>terimtoplami);

  return(list(p.value=pvalue))
}
