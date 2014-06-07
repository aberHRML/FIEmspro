`fiems_lct_main` <-
function(my_path,runinfo,y1,y2,y3,y4,limit=0.82,
                           save.file=TRUE,file.name="LCT-mean.RData")
{
  ## validity checking
  if (missing(my_path) || missing(runinfo)|| missing(y1)|| missing(y2)
      || missing(y3)|| missing(y4)) 
    stop("Some arguments are missing")
  
  scrng = c(y1,y2,y3,y4)
  loop=0
  ## timer start:
  time1 <- timer_start()

  ## load run-info file:
  mypath=paste(my_path,"/",runinfo,sep = "")
  out=my_path
  tab=read.csv(mypath)
  nro=nrow(tab)

  print(" Well - tea or coffee? Because ... ")
  
  for(path_root in paste(tab$pathcdf,"/",tab$filecdf,sep = ""))
    {
      loop=loop+1
      ## go...
      mymsg=paste(loop," of ",nro," files done",sep = "")
      flush.console()   ## for Windows
      result <- fiems_lctc2m(path_root,scrng,limit)
      if (loop==1) {
        mat=result$ph
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
        mat=rbind(mat,result$ph)
      }
      print(mymsg)
      flush.console()   ## for Windows
    }

  lct <- list(runinfo=tab,mat=mat,scrng=scrng,limit=limit)

  if (save.file) {
    out1=paste(out,"/",file.name,sep = "")
    save(lct, file = out1)

    ## Additionally, save as tab-delimited file to analyse elsewhere:
    write.table(mat,file=paste(out,"/mat.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
    myparam=rbind(scrng,limit)
    write.table(myparam,file=paste(out,"/myparam.txt",sep=""),sep="\t",
                row.names =F,col.names=F)
  }

  ## timer end:
  time2 <- timer_end(time1)
  mytime=paste("...in ",time2$dt)
  print(mytime)

  return(lct)
}

