`timer_start` <-
function() 
{
  ## timer start:
  t1h=format(Sys.time(), "%H")
  t1m=format(Sys.time(), "%M")
  t1s=format(Sys.time(), "%OS3")
  t1h=type.convert(t1h)
  t1m=type.convert(t1m)
  t1s=type.convert(t1s)
  t1d=date()
  return(list(hh=t1h,mm=t1m,ss=t1s,dd=t1d))
}

