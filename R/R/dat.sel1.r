#' Generate Data Set List
#' 
#' Generate subset data sets based on class labels. This function allows
#' generation of a selection of pairwise problems and/or multiple class
#' problems. The main objective of this function is to alleviate generation of
#' several variables containing data matrices, class information or validation
#' strategy. Each subset can enter \code{\link{accest}} and
#' \code{\link{feat.rank.re}} analysis without specifying both class and data
#' matrix. Additionally, each subset can also contain validation parameters so
#' that direct comparison between classifiers and feature ranking technique can
#' be easily done.
#' 
#' This function is used to provide the data set for the binary combination of
#' the class factor. If \code{pwise} is \code{list()}, the binary combination
#' of for all class labels will be done. If \code{pwise} has one class label,
#' the comparisons between this one and any other class are done. If
#' \code{pwise} has more than three class lables, enumerate the combinations or
#' permutations of the elements of \code{pwise}. For details, see
#' \code{examples} below.
#' 
#' @usage dat.sel1(dat, cl, pwise = NULL, mclass = list(),pars=NULL)
#' @param dat A data frame or matrix.
#' @param cl A factor or vector of class.
#' @param pwise The vector or list of class labels to be chosen for binary
#' classification.
#' @param mclass The vector or list of class labels to be chosen to be included
#' in a multiple classification task.
#' @param pars Partitioning information.
#' @return A list with components: \item{nam}{ Discrimination task.  }
#' \item{dat}{ Subset of the dataset.  } \item{cl}{ Class labels.  }
#' \item{pars}{ Object of type pars.  } \item{tr.idx}{ Object of type trainind.
#' } \item{lsamp}{ Sample ids in the original data matrix.  }
#' @author David \email{dle@@aber.ac.uk}
#' @keywords manip
#' @examples
#' 
#' data(abr1)
#' x<-abr1$pos[,110:120]
#' y<-factor(abr1$fact$day) 
#' 
#' ## generate data set with all pairwise containing class "1"
#' dat1 <- dat.sel1(x,y,pwise="1",mclass=NULL)
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' ## generate data set with pairwise between classes "1" and "H"
#' dat1 <- dat.sel1(x,y,pwise=c("1","H"),mclass=NULL)
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' ## generate data set with pairwise between classes "1", "2", "H"
#' dat1 <- dat.sel1(x,y,pwise=c("2","1","H"),mclass=NULL)
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' ## generate data set with all pairwises containing "1" and "H"
#' dat1 <- dat.sel1(x,y,pwise=list("1","H"),mclass=NULL)
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' ## generate data set with 3 classes "1", "2" and "H"
#' dat1 <- dat.sel1(x,y,pwise=NULL,mclass=c("1","2","H"))
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' ## generate data set with all pairwises containing "1" and "H"
#' ## on which partitioning is a 1*5CV
#' pars=valipars(sampling = "cv", niter = 1, nreps = 5)
#' dat1 <- dat.sel1(x,y,pwise=list("1","H"),mclass=NULL, pars=pars)
#' unlist(lapply(dat1,function(x) x$name))
#' 
#' print(dat1[[1]])
#' 
#' 
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
   

