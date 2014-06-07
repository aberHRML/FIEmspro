`mytmat` <-
function(tmat,tint,rngb,rnge,srngb,srnge,lim)
{
  fmoz=floor(tmat)
  cmoz=ceiling(tmat)
  deltaf=tmat-fmoz
  fmoz[deltaf>=lim]=0
  cmoz[deltaf<lim]=0
  rtpmozphsc=fmoz+cmoz
  tmat=rtpmozphsc
  tro=nrow(tmat)
  tco=ncol(tmat)
  tmatr=matrix(0,tro,2000);
  stmatr=matrix(0,tro,2000);

  for (ti in rngb:rnge) {
    tj=1
    while (tmat[ti,tj]>0 & tj<tco) {
      tmatr[ti,tmat[ti,tj]]=tmatr[ti,tmat[ti,tj]]+tint[ti,tj]
      tj=tj+1
    }
  }
  ##==='sub background'===
  ## just in case there are less scans than expected -
  ##   use maximum scan number for scan 'y4' instead:
  if (srnge>tro) {
    print(srnge)
    flush.console()   ## for Windows
    srnge=tro
    print(srnge)
    flush.console()   ## for Windows
  }
  if (srngb>0) {
    for (ti in srngb:srnge) {
      tj=1;
      while (tmat[ti,tj]>0 & tj<tco) {
        stmatr[ti,tmat[ti,tj]]=stmatr[ti,tmat[ti,tj]]+tint[ti,tj]
        tj=tj+1
      }
    }
  }
  ## here ideally:  y2 - y1 = y4 - y3:
  ## changed from mean(signal)-mean(backgrd) to mean(signal-backgrd)!
  tmatr=tmatr-stmatr
  tmatr[tmatr<0]=0
  b = apply(tmatr,2,mean)
  return(t(b))
}

