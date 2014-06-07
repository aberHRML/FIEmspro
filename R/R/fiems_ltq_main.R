`fiems_ltq_main` <-
function(my_path,runinfo, y1,y2,y3,y4,limit=0.7,
                           itp=65537,itn=131073,hrng=50,lrng=15,
                           save.file=TRUE,file.name="LTQ-mean.RData")
{
  ## validity checking
  if (missing(my_path) || missing(runinfo)|| missing(y1)|| missing(y2)
      || missing(y3)|| missing(y4)) 
    stop("Some arguments are missing")
  
  scrng = c(y1,y2,y3,y4)
  loop  = 0
  ## timer start:
  time1 <- timer_start()
  
  ## load run-info file:
  mypath = paste(my_path,"/",runinfo,sep = "")
  out    = my_path
  tab    = read.csv(mypath)
  nro    = nrow(tab)
  print(" Well - tea or coffee? Because ... ")
  for(path_root in paste(tab$pathcdf,"/",tab$filecdf,sep = "")){
    loop=loop+1
    ## go...
    mymsg = paste(loop," of ",nro," files done",sep = "")
    flush.console()   ## for Windows
    result <- fiems_ltqc2m(path_root,hrng,lrng,itp,itn,scrng,limit)
    if (loop==1) {
      posh=result$ph
      negh=result$nh
      posl=result$pl
      negl=result$nl
      ## provide user with time estimate:
      time2 <- timer_end(time1)
      estt=time2$ss*nro/60
      if (estt>60) {
        estt=round(estt/60, digits = 1)
        print(paste(" ... that might take about ",estt," hours..."))
      } else {                                               
        estt=round(estt, digits = 1)
        print(paste(" ... that might take about ",estt," minutes..."))
      }
    } else {
      posh=rbind(posh,result$ph)
      negh=rbind(negh,result$nh)
      posl=rbind(posl,result$pl)
      negl=rbind(negl,result$nl)
    }
    print(mymsg)
    flush.console()   ## for Windows
  }
  
  ltq <- list(runinfo=tab,posh=posh,posl=posl,negh=negh,negl=negl,
              limit=limit,scrng=scrng)

  ## save all matrices as 'LTQ-mean.rda'
  if(save.file) {
    out1 = paste(out,"/",file.name,sep = "")
    save(ltq, file = out1)
  
    ## Additionally, save as tab-delimited file to analyse elsewhere:
    write.table(posh,file=paste(out,"/posh.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
    write.table(negh,file=paste(out,"/negh.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
    write.table(posl,file=paste(out,"/posl.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
    write.table(negl,file=paste(out,"/negl.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
    myparam=rbind(scrng,limit)
    write.table(myparam,file=paste(out,"/myparam.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
  }
  
  ## timer end:
  time2 <- timer_end(time1)
  mytime=paste("...in ",time2$dt)
  print(mytime)
  
  return(ltq)
}

