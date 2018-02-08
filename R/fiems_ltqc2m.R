`fiems_ltqc2m` <-
function(inn,hrng,lrng,itp,itn,scrng,limit)
{
  rngbegi=scrng[1]
  rngende=scrng[2]
  subrngbegi=scrng[3]
  subrngende=scrng[4]
  nc = nc_open( inn, write=FALSE, readunlim=TRUE, verbose=FALSE)
  
  ## get list of names, dimension of data and the data itself
  namesx=matrix(0,length(nc$var),1)
  datdim=matrix(0,length(nc$var),1)
  for (i in 1:length(nc$var)) {
    vx <- nc$var[[i]]
    dx <- ncvar_get( nc, vx )
    namesx[i]=vx$name
    datdim[i]=dim(dx)
    if (i==8)  Scindex=dx
    if (i==12) RT=dx
    if (i==14) Rmin=dx
    if (i==15) Rmax=dx
    if (i==16) Sctype=dx
    if (i==19) MZ=dx
    if (i==20) Int=dx
  }
  rmz=datdim[19]
  reti=datdim[12]
  Scind=Scindex[2:reti]
  Scind=c(Scind,rmz)
  deltasc=Scind-Scindex
  rm(Scind)

  ## generate MZ and Int matrix for each scan
  matmozsc=matrix(0,nrow=reti,ncol=max(deltasc))  ## zero matrix
  matintsc=matrix(0,nrow=reti,ncol=max(deltasc))  ## zero matrix
  for (j in 1:(reti-1)) {
    ix=deltasc[j]
    if (ix>0) {
      i1=Scindex[j]+1
      i2=Scindex[j+1]
      matmozsc[j,1:ix]=MZ[i1:i2]
      matintsc[j,1:ix]=Int[i1:i2]
    }
  }
  ## get last scan
  j=reti
  ix=deltasc[j]
  if (ix>0) {
    i1=Scindex[j]+1
    i2=rmz
    matmozsc[j,1:ix]=MZ[i1:i2]
    matintsc[j,1:ix]=Int[i1:i2]
  }

  ## find scans of same kind:
  lfitph=which(Sctype==itp & Rmin==hrng)
  lfitnh=which(Sctype==itn & Rmin==hrng)
  lfitpl=which(Sctype==itp & Rmin==lrng)
  lfitnl=which(Sctype==itn & Rmin==lrng)

  ## generate mz and int raw-matrices:
  itpmozphsc<-matmozsc[lfitph,]
  itpintphsc<-matintsc[lfitph,]
  itpmoznhsc<-matmozsc[lfitnh,]
  itpintnhsc<-matintsc[lfitnh,]
  itpmozplsc<-matmozsc[lfitpl,]
  itpintplsc<-matintsc[lfitpl,]
  itpmoznlsc<-matmozsc[lfitnl,]
  itpintnlsc<-matintsc[lfitnl,]

  ## get the binned mass-spectrum for the analytical run:
  itpph <- mytmat(itpmozphsc,itpintphsc,rngbegi,rngende,subrngbegi,subrngende,limit)
  itpnh <- mytmat(itpmoznhsc,itpintnhsc,rngbegi,rngende,subrngbegi,subrngende,limit)
  itppl <- mytmat(itpmozplsc,itpintplsc,rngbegi,rngende,subrngbegi,subrngende,limit)
  itpnl <- mytmat(itpmoznlsc,itpintnlsc,rngbegi,rngende,subrngbegi,subrngende,limit)

  return(list(ph=itpph,nh=itpnh,pl=itppl,nl=itpnl))
  nc_close(nc)
}

