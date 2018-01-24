`fiems_lctc2m` <-
function(inn,scrng,limit)
{
  rngbegi=scrng[1]
  rngende=scrng[2]
  subrngbegi=scrng[3]
  subrngende=scrng[4]
  nc=nc_open( inn, write=FALSE, readunlim=TRUE, verbose=FALSE)
  
  ## get list of names, dimension of data and the data itself
  namesx=matrix(0,length(nc$var),1)
  datdim=matrix(0,length(nc$var),1)
  for (i in 1:length(nc$var)) {
    vx <- nc$var[[i]]
    dx <- ncvar_get( nc, vx )
    namesx[i]=vx$name
    datdim[i]=dim(dx)
    if (i==14) Scindex=dx
    if (i==4)  RT=dx
    if (i==12) Rmin=dx
    if (i==13) Rmax=dx
    if (i==17) MZ=dx
    if (i==18) Int=dx
  }
  rmz=datdim[17]
  reti=datdim[4]
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

  ## get final binned mass-spectrum for the analytical run:
  itp <- mytmat(matmozsc,matintsc,rngbegi,rngende,subrngbegi,subrngende,limit)

  return(list(ph=itp))
  nc_close(nc)
}

