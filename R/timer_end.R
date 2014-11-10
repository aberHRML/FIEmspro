`timer_end` <-
function(time1) 
{
  ## timer end:
  t2h=format(Sys.time(), "%H")
  t2m=format(Sys.time(), "%M")
  t2s=format(Sys.time(), "%OS3")
  t2h=type.convert(t2h)
  t2m=type.convert(t2m)
  t2s=type.convert(t2s)
  t2d=date()
  t1h=time1$hh
  t1m=time1$mm
  t1s=time1$ss
  t1d=time1$dd

  if (t2h>=t1h) {
    tdh=t2h-t1h
  } else {
    tdh=24-t1h+t2h
  }
  if (t2m>=t1m) {
    tdm=t2m-t1m
  } else {
    tdm=60-t1m+t2m
    tdh=tdh-1
  }
  if (t2s>=t1s) {
    tds=abs(t2s-t1s)
  } else {
    tds=abs(60-t1s+t2s)
    tdm=tdm-1
  }
                                        #tds=abs(t2s-t1s)
  tds=round(tds,2)
  if (tdh>=1) {
    dtime=paste(tdh,"hr",tdm,"min")
  } else {
    dtime=paste(tdm,"min",tds,"sec")
  }

  return(list(dt=dtime,dd1=t1d,dd2=t2d,ss=tds))
}

