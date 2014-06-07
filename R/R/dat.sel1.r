dat.sel1<-function (dat, cl, pwise = NULL, mclass = list(),pars=NULL){


    combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {
        if (mode(n) != "numeric" || length(n) != 1 || n < 1 || 
            (n%%1) != 0) 
            stop("bad value of n")
        if (mode(r) != "numeric" || length(r) != 1 || r < 1 || 
            (r%%1) != 0) 
            stop("bad value of r")
        if (!is.atomic(v) || length(v) < n) 
            stop("v is either non-atomic or too short")
        if ((r > n) & repeats.allowed == FALSE) 
            stop("r > n and repeats.allowed=FALSE")
        if (set) {
            v <- unique(sort(v))
            if (length(v) < n) 
                stop("too few different elements")
        }
        v0 <- vector(mode(v), 0)
        if (repeats.allowed) 
            sub <- function(n, r, v) {
                if (r == 0) 
                  v0
                else if (r == 1) 
                  matrix(v, n, 1)
                else if (n == 1) 
                  matrix(v, 1, r)
                else rbind(cbind(v[1], Recall(n, r - 1, v)), 
                  Recall(n - 1, r, v[-1]))
            }
        else sub <- function(n, r, v) {
            if (r == 0) 
                v0
            else if (r == 1) 
                matrix(v, n, 1)
            else if (r == n) 
                matrix(v, 1, n)
            else rbind(cbind(v[1], Recall(n - 1, r - 1, v[-1])), 
                Recall(n - 1, r, v[-1]))
        }
        sub(n, r, v[1:n])
    }
    func <- function(choices,g) {
        if (is.null(choices)) {
            choices <- g
        }
        else {
            choices <- unique(choices)
        }
        i <- pmatch(choices, g)
        if (any(is.na(i))) 
            stop("'choices' should be one of ", paste(g, collapse = ", "))
        if (length(choices) == 1) {
            com <- combinations(length(g), 2, v = g)
            idx <- sapply(1:nrow(com), function(x) {
                if (match(choices, com[x, ], nomatch = 0) > 0) 
                  return(T)
                else (F)
            })
            com <- com[idx, ]
        }
        else {
            com <- combinations(length(choices), 2, v = choices)
        }
        reslist<-NULL
        for(k in 1:nrow(com)){reslist[[k]]=com[k,]}
        return(reslist)
    }

   if (missing(dat) || missing(cl)) 
        stop(" The data set and/or class label are missing!")
   cl <- as.factor(cl)
   g <- levels(cl)
   cl.list<-bin.cl<-m.cl<-NULL
   if(is.list(pwise) & length(pwise)==0)
      pwise=g
   if(is.list(mclass) & length(mclass)==0)
      mclass=g

   if(!is.null(pwise)){
    if (is.list(pwise)) {
        bin.cl <- lapply(pwise, function(x) func(x,levels(cl)))
        bin.cl <- do.call("c", bin.cl)
        bin.cl <- unique(bin.cl)
    }
    else {
        bin.cl <- func(pwise,g)
    }
    cl.list=bin.cl
   }
   if(!is.null(mclass)){
    if (!is.list(mclass))       
        m.cl <- list(sort(unique(mclass)))
    else
        m.cl <- lapply(mclass,function(x) sort(unique(x)))
    cl.list=c(cl.list,m.cl)
   }
   cl.list <-unique(cl.list)
   
   res.list<-NULL
   for(i in 1:length(cl.list)){
    nam=paste(cl.list[[i]],collapse="~")
    l=sort(unlist(lapply(cl.list[[i]],function(x) which(cl==x))))
    dat.i=dat[l,,drop=F]
    cl.i=factor(cl[l])
    tridx<-NULL
    if(!is.null(pars)){
      tridx<-trainind(cl.i,pars)
    }
    ret<-list(name=nam,dat=dat.i,cl=cl.i,pars=pars,tr.idx=tridx,lsamp=l)
    class (ret) <- "dlist"
    res.list[[i]]<-ret
   }
   
   res.list
   
} 
   

