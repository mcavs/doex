MGF=function(data,group,pred="tb",rept=10000){

  if(pred=="tb"){
    mu.tb=function(data,h=8.2){
      t0=median(data);s0=mad(data);n=length(data);
      fi=function(x){res=0;
      if(abs(x)<1){res=x*((1-x^2)^2)}
      return(res)
      }
      dfi=function(x){
        res=0;
        if(abs(x)<1){res=1-(6*(x^2))+(5*(x^4))};
        return(res)
      }
      repeat{
        sum1=0;sum2=0;sum3=0;
        z=(data-t0)/(h*s0);
        for(i in 1:n){
          sum1=sum1+fi(z[i]);
          sum2=sum2+dfi(z[i]);
          sum3=sum3+fi(z[i])^2;
        }
        mu=t0+(h*s0)*(sum1/sum2);
        vary=((h*s0)^2)*(n*sum3/((sum2)^2));
        if(abs(t0-mu)<0.000001){
          break;
        }
        t0=mu;
      }
      return(mu)
    }
    vary.tb=function(data){
      vary=mad(data^2)
      return(vary)
    }

    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.tb)
    varyans=tapply(data, group, vary.tb)

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
  }
  if(pred=="otb"){
    mu.otb=function(data,h=8.2){
      t0=median(data);s0=median(abs(data-t0));z=(data-t0)/(h*s0);n=length(data);
      fi=function(x){res=0;
      if(abs(x)<=1){res=x*(1-x^2)^2}
      return(res)
      }
      dfi=function(x){
        res=0;
        if(abs(x)<=1){res=1-(6*(x^2))+(5*(x^4))};
        return(res)
      }
      sum1=0;sum2=0;sum3=0;
      for(i in 1:n){
        sum1=sum1+fi(z[i]);
        sum2=sum2+dfi(z[i]);
        sum3=sum3+fi(z[i])^2;
      }
      mu=t0+(h*s0)*(sum1/sum2);
      vary=((h*s0)^2)*(n*sum3/(sum2)^2);
      return(mu)
    }
    vary.otb=function(data,h=8.2){
      t0=median(data);s0=median(abs(data-t0));z=(data-t0)/(h*s0);n=length(data);
      fi=function(x){res=0;
      if(abs(x)<=1){res=x*(1-x^2)^2}
      return(res)
      }
      dfi=function(x){
        res=0;
        if(abs(x)<=1){res=1-(6*(x^2))+(5*(x^4))};
        return(res)
      }
      sum1=0;sum2=0;sum3=0;
      for(i in 1:n){
        sum1=sum1+fi(z[i]);
        sum2=sum2+dfi(z[i]);
        sum3=sum3+fi(z[i])^2;
      }
      mu=t0+(h*s0)*(sum1/sum2);
      vary=((h*s0)^2)*(n*sum3/(sum2)^2);
      return(vary)
    }
    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.otb)
    varyans=tapply(data, group, vary.otb)

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
  }
  if(pred=="aw"){
    mu.aw=function(data,h=2.4){
      t0=median(data);
      s0=mad(data);
      n=length(data);
      repeat{
        z=(data-t0)/(h*s0);
        mu=t0+((h*s0)*atan(sum(sin(z))/sum(cos(z))));
        vary=((h*s0)^2)*((n*sum(sin(z)^2))/(sum(cos(z))^2));
        if(abs(t0-mu)<0.0001){
          break;
        }
        t0=mu;
      }
      return(mu)
    }
    vary.aw=function(data){
      vary=mad(data^2)
      return(vary)
    }
    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.aw)
    varyans=tapply(data, group, vary.aw)

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
  }
  if(pred=="oaw"){
    mu.oaw=function(data,h=2.4){
      t0=median(data);s0=median(abs(data-t0));z=(data-t0)/(h*s0);n=length(data);
      mu=t0+((h*s0)*atan(sum(sin(z))/sum(cos(z))));
      vary=((h*s0)^2)*((n*sum(sin(z)^2))/(sum(cos(z))^2));
      return(mu)
    }
    vary.oaw=function(data,h=2.4){
      t0=median(data);s0=median(abs(data-t0));z=(data-t0)/(h*s0);n=length(data);
      mu=t0+((h*s0)*atan(sum(sin(z))/sum(cos(z))));
      vary=((h*s0)^2)*((n*sum(sin(z)^2))/(sum(cos(z))^2));
      return(vary)
    }
    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.oaw)
    varyans=tapply(data, group, vary.oaw)

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
  }
  if(pred=="huber"){
    mu.huber=function(y, k=1.5, tol = 1.0e-6){
      y <- y[!is.na(y)]
      n <- length(y)
      mu <- median(y)
      s <- mad(y)
      if(s == 0) stop("cannot estimate scale: MAD is zero for this sample")
      repeat{
        yy <- pmin(pmax(mu-k*s,y),mu+k*s)
        mu1 <- sum(yy)/n
        if(abs(mu-mu1) < tol*s) break
        mu <- mu1
      }
      return(mu)
    }
    vary.huber=function(y, k=1.5, tol = 1.0e-6){
      y <- y[!is.na(y)]
      n <- length(y)
      mu <- median(y)
      s <- mad(y)
      if(s == 0) stop("cannot estimate scale: MAD is zero for this sample")
      repeat{
        yy <- pmin(pmax(mu-k*s,y),mu+k*s)
        mu1 <- sum(yy)/n
        if(abs(mu-mu1) < tol*s) break
        mu <- mu1
      }
      return(s^2)
    }
    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.huber)
    varyans=tapply(data, group, vary.huber)

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
  }
  if(pred=="hubers"){
    mu.hubers=function(y, k = 1.5, mu, s, initmu = median(y), tol = 1.0e-6){
      mmu=missing(mu); ms=missing(s)
      y=y[!is.na(y)]
      n=length(y)
      if(mmu) {
        mu0=initmu
        n1=n-1
      } else {
        mu0=mu
        mu1=mu
        n1=n
      }
      if(ms) {
        s0=mad(y)
        if(s0 == 0.0) return(list(mu=mu0, s = 0))
      } else {s0=s;s1=s}
      th=2*pnorm(k)-1
      beta=th + k^2*(1-th) - 2*k*dnorm(k)
      for(i in 1:30) {
        yy=pmin(pmax(mu0-k*s0,y), mu0+k*s0)
        if(mmu) mu1=sum(yy)/n
        if(ms) {
          ss=sum((yy-mu1)^2)/n1
          s1=sqrt(ss/beta)
        }
        if((abs(mu0-mu1) < tol*s0) && abs(s0-s1) < tol*s0) break
        mu0=mu1; s0=s1
      }
      return(mu0)
    }
    vary.hubers=function(y, k = 1.5, mu, s, initmu = median(y), tol = 1.0e-6){
      mmu=missing(mu); ms <- missing(s)
      y=y[!is.na(y)]
      n=length(y)
      if(mmu) {
        mu0=initmu
        n1=n - 1
      } else {
        mu0=mu
        mu1=mu
        n1=n
      }
      if(ms) {
        s0=mad(y)
        if(s0 == 0.0) return(list(mu=mu0, s = 0))
      } else {s0=s;s1=s}
      th=2*pnorm(k)-1
      beta=th + k^2*(1-th) - 2*k*dnorm(k)
      for(i in 1:30) {
        yy=pmin(pmax(mu0-k*s0,y), mu0+k*s0)
        if(mmu) mu1=sum(yy)/n
        if(ms) {
          ss=sum((yy-mu1)^2)/n1
          s1=sqrt(ss/beta)
        }
        if((abs(mu0-mu1) < tol*s0) && abs(s0-s1) < tol*s0) break
        mu0=mu1; s0=s1
      }
      return(s0^2)
    }
    hacim=tapply(data, group, length)
    grupsayisi=length(tapply(data, group, length))
    ortalama=tapply(data, group, mu.hubers)
    varyans=tapply(data, group, vary.hubers)

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
  }

  return(list(p.value=pvalue))
}

