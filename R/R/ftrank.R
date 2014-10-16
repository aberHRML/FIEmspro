feat.rank.re.default <- function(x,y,method, pars = valipars(),tr.idx=NULL, clmpi=NULL, seed=NULL,...)
{
  ## validity checking
  if (missing(x) || missing(y)) 
    stop("data set or class are missing")
  if(length(dim(x)) != 2)
    stop("'x' must be a matrix or data frame")
  if (!is.factor (y)) 
    stop("y must be a factor.")
  if (nrow(x) != length(y)) stop("x and y don't match.")
  if (length(unique(y)) < 2) 
    stop("Classification needs at least two classes.")
  if (any(is.na(x)) || any(is.na(y))) 
    stop("NA is not permitted in data set or class labels.")

  cl <- as.factor(y)
  nam=paste(levels(y),collapse="~")
  dat <- as.matrix(x)
  n   <- nrow(x)        ## number of samples
  p   <- ncol(x)        ## size of feature

  ## construct index of train data
  if(is.null(tr.idx)){
    if (pars$sampling == "cv" && pars$nreps > n ){
      pars$sampling <- "loocv"
      pars$niter    <- 1
    }
    if (pars$sampling == "cv" && pars$nreps < 2)
         stop("Number of fold (nreps) for cv must greater than 1")
    tr.idx <- trainind(y, pars = pars)
  }
  pars$niter    <- length(tr.idx)
  pars$nreps    <- length(tr.idx[[1]])

  all=do.call(method, c(list(x,y), list(...)))

  rank.list  <- list()
  order.list <- list()
  stats.list <- list()
  cat("Iteration (",pars$niter,"):",sep="")

  for (i in 1:pars$niter) {
   cat(" ", i, sep = "")
   flush.console()   ## for Windows
   train.ind <- tr.idx[[i]]
    res <- list()    
        params<-list(x=x,y=y,train.ind=train.ind,method=method,
          args=list(...),seed=seed,i=i)
        if(!is.null(clmpi))
          res<-clusterApplyLB(clmpi,1:length(train.ind),
            FIEmspro:::inloopfeatrk,params)
        else{
          for (j in 1:length(train.ind)) {
            res[[j]] <- FIEmspro:::inloopfeatrk(j,params)
          }
        }
    rank.list[[i]]  <- sapply(res, function(x) cbind(x$fs.rank))
    order.list[[i]] <- sapply(res, function(x) cbind(x$fs.order))
    stats.list[[i]] <- sapply(res, function(x) cbind(x$stats))
    rownames(rank.list[[i]])  <- colnames(x)
    rownames(stats.list[[i]]) <- colnames(x)
    rownames(order.list[[i]]) <- 1:ncol(x)
  } 
  cat("\n")
  rank.list  <- do.call("cbind",rank.list) 
  order.list <- do.call("cbind",order.list) 
  stats.list <- do.call("cbind",stats.list) 
  fs.stats   <- apply(stats.list, 1, mean)
  
  ## -----------------------------------------------------------------------
  ## Use Borda count to get the final feature order
  fs.score  <- apply(rank.list,1,sum)
  fs.order  <- order(fs.score, decreasing=F)  ## feature order from best to worst.   
  fs.rank   <- order(fs.order, decreasing=F)  ## feature rank score.
  names(fs.rank) <- rownames(rank.list)   
  temp     <- names(fs.rank[fs.order])
  if (!is.null(temp)) 
    fs.order <- noquote(temp)
  ## -----------------------------------------------------------------------

dots<-list(...)
if(length(dots)==0)
 argfct="default"
else
 argfct=paste(paste(names(unlist(dots)),unlist(dots),sep="="),collapse=",")

parsfct<-function(pars){
if(pars$sampling=="loocv")
  rep="loocv"
else{
 str<-"NSt"
 if(pars$strat) str<-"St"
 if(pars$niter>1)
   rep<-paste(pars$niter,"x",str,"-",pars$nreps,"-",pars$sampling,sep="")
 else
   rep<-paste(str,"-",pars$nreps,"-",pars$sampling,sep="")
}
if(!is.null(pars$div))
  rep=paste(rep,"-",round(pars$div,2),sep="")

rep
}
####

  ret <- list(method     = method,
              fs.order   = fs.order,       ## feature order
              fs.rank    = fs.rank,        ## feature rank
              fs.stats   = fs.stats,       ## means of stats
              rank.list  = rank.list,      ## full feature rank list
              order.list = order.list,     ## full feature order list
              stats.list = stats.list,     ## full feature stats list
              pars       = pars,           ## resampling parameters
              tr.idx     = tr.idx,         ## index of training samples.
              argfct     = argfct,         ## arguemtns passed to fs method
              all        = all,            ## feature ranking without re-sampling    
              cl.task = nam, ### addedd by dle to keep trace of the classification task
               pars.mini = parsfct(pars)
              )
  class (ret) <- "feat.rank.re"
  return(ret)
}


inloopfeatrk<-function(j,params){
            x.tr <- as.matrix(params$x[params$train.ind[[j]], , drop = F])
            y.tr <- params$y[params$train.ind[[j]]]
            if(!is.null(params$seed))
              set.seed(10000*params$i + params$seed)
            if(length(params$arg)==0)
              do.call(params$method, list(x.tr, y.tr))
            else
              do.call(params$method, c(list(x.tr, y.tr), params$args))          
}

######################


#' Wrapper for Resampling Based Feature Ranking
#' 
#' Wrapper for performing feature ranking method on multiple subsets of the
#' original data. At each iteration, only the training samples defined in
#' \code{tr.idx} (or optionally \code{pars} only) are used to rank the
#' variables.  Features rank, order and saliency indicators calculated on the
#' whole data are also given in the output. As for \code{\link{accest}} this
#' function allows the use of multiple processors as long as the cluster has
#' been set up with the \pkg{snow} package. Data input can be in the form of
#' \code{data} matrix + \code{class} vector, following the classic formula type
#' or derived from \code{\link{dat.sel1}}.
#' 
#' The structure of the \code{feat.rank.re} object is as follows: \describe{
#' \item{method}{Feature ranking method used.} \item{fs.rank}{A vector of final
#' feature ranking scores.} \item{fs.order}{A vector of final feature order
#' from best to worst.} \item{fs.stats}{A vector of means of statistics or
#' measurements in all computation.} \item{rank.list}{Feature rank list of all
#' computation.} \item{order.list}{Feature order list of all computation.}
#' \item{pars}{Resampling parameters.} \item{tr.idx}{Index of training
#' samples.} \item{pars.min}{Condensed form of the resampling strategy for
#' output purposes.} \item{cl.task}{Condensed form of the classification task.}
#' \item{all}{Feature ranking object originated from the overall dataset.} }
#' 
#' @aliases feat.rank.re print.feat.rank.re feat.rank.re.formula
#' feat.rank.re.default feat.rank.re.dlist
#' @usage feat.rank.re(\dots{})
#' 
#' \method{feat.rank.redefault}(x,y,method,pars =
#' valipars(),tr.idx=NULL,clmpi=NULL, seed=NULL, \dots{})
#' 
#' \method{feat.rank.reformula}(formula, data = NULL, \dots{})
#' 
#' \method{feat.rank.redlist}(dlist, method, pars = NULL, tr.idx = NULL,
#' \dots{})
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots{}} That
#' is, the response is the grouping factor and the right hand side specifies
#' the (non-factor) discriminators.
#' @param x A matrix or data frame containing the explanatory variables.
#' @param dlist A matrix or data frame containing the explanatory variables if
#' no formula is given as the principal argument.
#' @param data Data frame from which variables specified in \code{formula} are
#' preferentially to be taken.
#' @param y A factor specifying the class for each observation.
#' @param method Feature ranking method to be used.  See
#' \code{\link{fs.techniques}} for details.
#' @param pars A list of resampling scheme or validation method such as
#' \emph{Leave-one-out cross-validation}, \emph{Cross-validation},
#' \emph{Bootstrap} and \emph{Randomised validation (holdout)}.  See
#' \code{\link{valipars}} for details.
#' @param tr.idx User defined index of training samples of type
#' \code{trainind}.  Generated by \code{trainind} if \code{tr.idx=NULL}.
#' @param clmpi snow cluster information
#' @param seed Seed.
#' @param \dots Additional parameters to be passed to \code{method}.  See
#' \code{\link{fs.techniques}} for details.
#' @return \code{feat.rank.re} object.
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{valipars}}, \code{\link{ftrank.agg}},
#' \code{\link{fs.techniques}}
#' @keywords classif
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' ## Select classes 1 and 2
#' pars   <- valipars(sampling="boot",niter=2,nreps=5)
#' dat <- dat.sel1(x, y, pwise=c("1","2"),mclass=NULL,pars=pars)
#' 
#' ## multiple rankings using AUC
#' z      <- feat.rank.re(dat[[1]],method="fs.auc")
#' 
#' ## print content of z
#' names(z)
#'                
#' 
feat.rank.re <- function (...) UseMethod ("feat.rank.re")

print.feat.rank.re <- function(x,...){

  cat("Method:\t\t", x$method)
  cat("\nArguments:\t", x$argfct)
  cat("\nDiscrimination:\t", x$cl.task)
  cat("\n\nNo. of iteration:\t", x$pars$niter)
  cat("\nSampling:\t\t", x$pars$sampling)
  cat("\nNo. of replications:\t", x$pars$nreps)

}

######################
feat.rank.re.dlist<-function(dlist,method,pars=NULL,tr.idx=NULL,...)
{
#  call <- match.call()
  if (!inherits(dlist, "dlist")) 
    stop("method is only for dlist objects")

  cl=as.factor(dlist$cl)
  dat=as.matrix(dlist$dat)
  if(is.null(pars)){
    if(!is.null(dlist$pars))
      pars=dlist$pars
    else
      pars=valipars()
  }
  if(!is.null(dlist$tr.idx) & is.null(tr.idx))
    tr.idx=dlist$tr.idx
  
  ret <- feat.rank.re.default(dat, cl, method=method, pars = pars, tr.idx = tr.idx, ...)
  return(ret)
}

######################
feat.rank.re.formula <- function (formula, data = NULL, ...)
{
  call <- match.call()
  if (!inherits(formula, "formula")) 
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))
  m$... <- NULL
  m$scale <- NULL
  m[[1]] <- as.name("model.frame")
  m$na.action <- na.action
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  x <- model.matrix(Terms, m)
  y <- model.extract(m, "response")
  ret <- feat.rank.re.default (x, y, ...)
  return (ret)
}

##########################


#' Tidy up multiple resampling based ranking results.
#' 
#' Convenience function to ease the output of \code{\link{summ.ftrank}}
#' objects.
#' 
#' This function has been designed to provide maximum facilities to ease both
#' screen printing and file write of complex list of resampling based feature
#' ranking resulting from different comparisons and/or different FR settings.
#' It uses heavily the information contained in the FR definition table
#' (\code{frdef} in the \code{frsum} object) to: 1) merge different tables (1
#' FR method on 1 dataset) in a bigger table (several FR and/or datasets) 2)
#' sort each ranking table given a statistical output and 3) group tables into
#' subsets corresponding to FR technique or datasets. File writing option can
#' be rather useful when 1), 2) or 3) must be repeated manually several times:
#' \itemize{ \itemParameter \code{nam} controls the string that can be added at
#' the end of each column in order to avoid confusion when merging individual.
#' For e.g. if Alg is chosen, the value contained in the column Alg of
#' \code{frsum$frdef} is added to Stats, Rank etc...  \itemIf a table should be
#' sorted before merging, the parameter \code{sorting} should be set
#' accordingly. For e.g. sorting the FR result according to the rank is easliy
#' made by \code{sorting="Rank")}. The field \code{decreasing} should be also
#' set to FALSE if results must presented in increasing order.  \itemBy setting
#' the parameter \code{tidy} to one of the column name of \code{frsum$frdef}, a
#' list of tables involving only identical values in \code{tidy} will be merged
#' together.  In the example involving 5 discrimination tasks and 2 feature
#' ranking methods illustrated below, results are either sorted by FR technique
#' or by discrimination problem.  \itemResults can be redirect to a file or
#' several files so that further manipulation can be made in a more
#' \code{friendly} spreadsheet software. When the results are tidied, several
#' files are generated.  }
#' 
#' @usage tidy.ftrank(frsum, lmod = NULL, sorting = "Stat", tidy = "DisId", nam
#' = "AlgId", decreasing = TRUE, file = NULL)
#' @param frsum \code{mfr.sum} objects
#' @param lmod List of objects to be printed out - Default all objects
#' @param sorting Should the results be sorted according to an argument
#' contained in \code{frsum$frdef} - Default sorted by variable name
#' @param tidy Tidy the output according to an argument contained in
#' \code{frsum$frdef}
#' @param nam Concatenate the original column names with content in to an
#' argument contained in \code{frsum$frdef}
#' @param decreasing Sorting order if argument \code{sorting} is not NULL
#' @param file Write results into one file or several files
#' @return List of tables or list of files.
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{summ.ftrank}}
#' @keywords manip
#' @examples
#' 
#' 
#' ################################################################
#' ### Example involving 5 discrimination tasks by 2 feature ranking techniques
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' ## Select classes all pairwise problems involving class with 1 
#' dat <- dat.sel1(x, y, pwise="1",mclass=NULL,pars=valipars(sampling="boot",niter=2,nreps=5))
#' 
#' ### Perform AUC and Random Forest ranking
#' resauc = lapply(dat, function(x) feat.rank.re(x,method="fs.auc"))
#' resrf = lapply(dat, function(x) feat.rank.re(x,method="fs.rf",ntree=100))
#' 
#' ################################################################
#' ## Aggregate all the models
#' mfr=ftrank.agg(resauc,resrf)
#' print(mfr$frdef)
#' ## Compute mrp-val for all the FR models
#' frsum=summ.ftrank(mfr,qtl=.7)
#' 
#' ## print the FR components
#' print(frsum$frdef)
#' 
#' ## have a look at the first 5 variables in the second model
#' print(frsum$frsum[[2]][1:5,])
#' 
#' ################################################################
#' ##### Let's concentrate on the models 1 and 6 first
#' ## No need to tidy here - no sorting of the results and 
#' ## add the content of "Alg" in the columns names
#' res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting=NULL,nam="Alg")
#' 
#' ## because we did not tidy the ranking tables
#' ## rankings from models 1 and 6 are concatenated
#' ## in the first field of res 
#' ## print out the first 5 lines
#' res[1:5,]
#' 
#' ## Same as before but we sort the results according to "Stat" 
#' ## in decreasing order
#' res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting="Stat",nam="Alg",decreasing=TRUE)
#' res[1:5,]
#' 
#' ## Same as before but we sort the results according to "Rank" 
#' ## in increasing order (of course)
#' res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting="Rank",nam="Alg",decreasing=FALSE)
#' res[1:5,]
#' 
#' ################################################################
#' ###### Tidy all the rankings according to the discrimination task
#' ## in this case we set tidy="Dis"
#' ## all models: lmod=NULL
#' ## sorting according to Stat
#' ## append the name contained in Alg to the column name
#' 
#' res=tidy.ftrank(frsum,lmod=NULL,tidy="Dis",sorting="Stat",nam="Alg")
#' 
#' ## Discrimination task tags are the name for each field of res
#' names(res)
#' ## same as before for discrimination'1~2' or 1
#' res[[1]][1:5,]
#' res$'1~2'[1:5,]
#' 
#' ## discrimination'2~3' or 2 ...
#' res[[2]][1:5,]
#' 
#' 
#' ################################################################
#' ###### Tidy all the rankings according to the FR method
#' ## in this case we set tidy="Alg" for e.g.
#' ## all models: lmod=NULL
#' ## sorting according to Stat
#' ## append the name contained in Dis to the column name
#' 
#' res=tidy.ftrank(frsum,lmod=NULL,tidy="Alg",sorting="Stat",nam="Dis")
#' 
#' ## FR technique method are the name for each field or res
#' names(res)
#' 
#' ## the top 5 variables highlighted by AUC in the 5 comparisons ...
#' res$'fs.auc'[1:5,]
#' 
#' ################################################################
#' ###### Redirect the output to one or several files
#' ## print each tables to a CSV files starting by "testtidy"
#' ## followed by the string contained in tidy (here "Dis")
#' \dontrun{tidy=tidy.ftrank(frsum,lmod=NULL,tidy="Dis",sorting="Stat",nam="Alg",file="testtidy")}
#' 
#' 
#' 
tidy.ftrank<-function(frsum,lmod=NULL,sorting="Stat",tidy="DisId",nam="AlgId",decreasing=TRUE,file=NULL){

    if (!inherits(frsum, "mfr.sum")) 
        stop("method is only valid for mfr.sum objects")
    if (is.null(lmod)) 
        lmod = 1:length(frsum$frsum)
    lnam <- NULL
    if (!is.null(nam)) 
        lnam = frsum$frdef[lmod, dimnames(frsum$frdef)[[2]] == nam]
    ltidy = list(lmod)
    nametidy <- NULL
    if (!is.null(tidy)) {
        tmp = frsum$frdef[lmod, dimnames(frsum$frdef)[[2]] == 
            tidy]
        ltidy = list()
        for (k in unique(tmp)) {
            ltidy = c(ltidy, list(lmod[tmp == k]))
            nametidy = c(nametidy, k)
        }
    }

    ### check if same number of rows
    add.rows=0
    num.rows=unique(unlist(lapply(frsum[[1]],nrow)))
    if(length(num.rows)>1){add.rows=max(num.rows)}
    
    listres <- lapply(ltidy, function(x) {
        res <- NULL
        for (i1 in 1:length(x)) {
            i = x[i1]
            tmp <- frsum$frsum[[i]]
            if (!is.null(sorting)) {
                col = which(dimnames(tmp)[[2]] == sorting)
                idx = order(tmp[, col],decreasing=TRUE)
                tmp <- tmp[idx, ]
            }
            tmp = cbind(dimnames(tmp)[[1]], tmp)
            dimnames(tmp)[[2]][1] = "VarName"
            if (!is.null(lnam)) 
                dimnames(tmp)[[2]] = paste(dimnames(tmp)[[2]], 
                  lnam[x[i1]], sep = "-")
        if(add.rows>1)
             tmp=rbind(tmp,matrix(NA,add.rows-nrow(tmp),ncol(tmp)))

            res = cbind(res, tmp)
        }
        if(add.rows>1)
         dimnames(res)[[1]]=1:nrow(res)
         
        return(res)
    })
    if (!is.null(nametidy)) 
        names(listres) = nametidy
    if (!is.null(file)) {
        lapply(1:length(listres), function(x) {
            fname = paste(file, "-allfr.csv", sep = "")
            if (!is.null(nametidy)) 
                fname = paste(file, "-", nametidy[x], ".csv", 
                  sep = "")
            write.table(listres[[x]], file = fname, sep = ",", 
                row.names = FALSE)
        })
    }
    else{
    	if(length(listres)==1)
    	   return(listres[[1]])
    	 
    	return(listres)
    }
} ## end of function


##############


#' Summarise multiple resampling based feature ranking outputs
#' 
#' This routine performs further computations on a list of \code{feat.rank.re}
#' objects contained in a \code{mfr.obj} object (see \code{\link{ftrank.agg}}).
#' If only one \code{feat.rank.re} result from \code{\link{feat.rank.re}} is
#' analyzed, it is easier to pass it first to \code{\link{ftrank.agg}} (see
#' example).Two calculations are made: 1) Computation of the pseudo mrp-value
#' from the resampling based feature (see \code{\link{fs.mrpval}}) and 2)
#' adjusted p values if p values have calculated (see \code{\link{p.adjust}}).
#' 
#' The resulting list as two component: one is equal to the total number of
#' \code{feat.rank.re} objects (i.e one resampling experiment) and one field is
#' a table that summarises each \code{feat.rank.re} (as for
#' \code{\link{ftrank.agg}}). In the first component, each table may have a
#' different number of columns depending if the FR method outputs p-values or
#' not: \describe{ \item{Stat:}{Original statistics calculated on the overall
#' data.} \item{Rank:}{Original feature rank calculated on the overall data.}
#' \item{pval:}{Original feature p-value calculated on the overall data
#' (optional).} \item{pval-xxx:}{Feature adjusted p-value by method xxx if
#' p-value available (i.e. previous column).} \item{mrpval-xxx:}{Pseudo
#' multiple resampling p-value using a given \code{qtl} value xxx.}
#' \item{AvgRk:}{Average feature rank calculated from the ranks found for each
#' training data partition.} \item{SdevRk:}{Associated feature rank standard
#' deviation calculated from the ranks found for each training data partition.}
#' }
#' 
#' @usage summ.ftrank(lclas, lmod = NULL, qtl = 0.25, padjust = "fdr")
#' @param lclas mfr.obj object - See details in \code{\link{ftrank.agg}}
#' @param lmod List of models to be considered in lclas
#' @param qtl Quantile - See details in \code{\link{fs.mrpval}}
#' @param padjust p value adjustement method - See details in
#' \code{\link{p.adjust}}
#' @return \code{mfr.sum} object: \item{frsum}{List of tables corresponding to
#' each \code{feat.rank.re} object - See details} \item{frdef}{Summary of each
#' \code{feat.rank.re} object as in \code{\link{ftrank.agg}}}
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{fs.mrpval}}, \code{\link{tidy.ftrank}},
#' \code{\link{p.adjust}}
#' @keywords manip classif
#' @examples
#' 
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' ## Select classes 1 and 2
#' dat <- dat.sel1(x, y, pwise="1",mclass=NULL,pars=valipars(sampling="boot",niter=2,nreps=5))
#' 
#' reswelch = feat.rank.re(dat[[1]],method="fs.welch")
#' mfr=ftrank.agg(reswelch)
#' print(mfr$frdef)
#' frsum=summ.ftrank(mfr,lmod=1,qtl=.3)
#' 
#' ## print the FR components
#' print(frsum$frdef)
#' 
#' ## have a look at the first 5 variables
#' print(frsum$frsum[[1]][1:5,])
#' 
#' 
summ.ftrank<-function(lclas,lmod=NULL,qtl=0.25,padjust="fdr"){

if(!inherits(lclas, "ftrank.agg"))
  stop("method is only valid for ftrank.agg objects")

infct<-function(tmp,qtl=NULL,padjust="fdr"){
lnames=c("Stat","Rank")
summ=cbind(tmp$all$stats,tmp$all$fs.rank)
if(!is.null(tmp$all$pval)){
 summ=cbind(summ,tmp$all$pval)
 lnames=c(lnames,"pval")
 for(ipadj in padjust){
   summ=cbind(summ,p.adjust(tmp$all$pval,ipadj))
   lnames=c(lnames,paste("pval",ipadj,sep="-"))
 }
}
if(!is.null(qtl))
 for(iqtl in qtl){
   mrpval=fs.mrpval(tmp,iqtl)$mrpval
   summ=cbind(summ,mrpval)
   lnames=c(lnames,paste("mrpval",iqtl,sep="-"))
 }
x<-tmp$rank.list
summ=cbind(summ,apply(x, 1, mean),apply(x, 1, sd))
lnames=c(lnames,"AvgRk", "SdevRk")
dimnames(summ)=list(names(tmp$all$fs.rank),lnames)
summ
}

if(is.null(lmod))
  lmod=1:length(lclas$ftrank)

res<-lapply(lmod,function(x) infct(lclas$ftrank[[x]],qtl,padjust))
            
res=list(frsum=res,frdef=lclas$frdef[lmod,])

class (res) <- "mfr.sum"
return(res)

}

#############


#' Aggregation of resampling based feature ranking results
#' 
#' Aggregate \code{feat.rank.re} objects and list of \code{feat.rank.re}
#' objects to form \code{ftrank.agg} object. The main utilities of this
#' function is to concatenate in a single list various results derived from
#' several \code{feat.rank.re} calls in order to facilitate post analysis
#' additional treatments and sorting of the results.
#' 
#' \code{ftdef} filed in the result list is a table with 9 columns which are
#' automatically generated to summarise the content of each individual
#' \code{feat.rank.re}. it is also aimed at avoiding confusions if the same
#' method is applied on the same discrimination problem but with different
#' settings, different resampling partitioning or even different data sets.
#' Each column is described as follows: \describe{ \item{list("Mod")}{Unique
#' identifier for each resampling based feature rankings.}\item{:}{Unique
#' identifier for each resampling based feature rankings.}
#' \item{list("Alg")}{Name of the classification technique as specified in the
#' call of \code{\link{feat.rank.re}}.}\item{:}{Name of the classification
#' technique as specified in the call of \code{\link{feat.rank.re}}.}
#' \item{list("Arg")}{Arguments passed to the FR technique during the call of
#' \code{\link{feat.rank.re}}.}\item{:}{Arguments passed to the FR technique
#' during the call of \code{\link{feat.rank.re}}.} \item{list("Pars")}{Summary
#' of the resampling strategy adopted during the call of
#' \code{\link{feat.rank.re}}.}\item{:}{Summary of the resampling strategy
#' adopted during the call of \code{\link{feat.rank.re}}.}
#' \item{list("Dis")}{Discrimination task involved. By default, this is equal
#' to the actual levels of the class vector passed to
#' \code{\link{feat.rank.re}} separated by \code{~}.}\item{:}{Discrimination
#' task involved. By default, this is equal to the actual levels of the class
#' vector passed to \code{\link{feat.rank.re}} separated by \code{~}.}
#' \item{list("AlgId")}{Unique algorithm identifier based on the columns Alg,
#' Arg and Pars so that no confusion is possible with Alg if several rankings
#' have been built with the same FR technique but with different parameters
#' and/or resampling strategy. This column can be modified by the
#' user.}\item{:}{Unique algorithm identifier based on the columns Alg, Arg and
#' Pars so that no confusion is possible with Alg if several rankings have been
#' built with the same FR technique but with different parameters and/or
#' resampling strategy. This column can be modified by the user.}
#' \item{list("DisId")}{Unique algorithm identifier based on the columns Dis in
#' order to simplified the name of the discrimination task if there are many
#' classes involved and/or class level have a long name. This column can be
#' modified by the user.}\item{:}{Unique algorithm identifier based on the
#' columns Dis in order to simplified the name of the discrimination task if
#' there are many classes involved and/or class level have a long name. This
#' column can be modified by the user.} \item{list("Other")}{Empty column that
#' can be amended to store extra information.}\item{:}{Empty column that can be
#' amended to store extra information.} }
#' 
#' @usage ftrank.agg(...)
#' @param \dots \code{feat.rank.re} objects and/or list of \code{feat.rank.re}
#' objects
#' @return \code{ftrank.agg} objects: \item{ftrank}{List of \code{feat.rank.re}
#' objects} \item{frdef}{Summary of each \code{feat.rank.re} object - See
#' details}
#' @author David Enot \email{dle@@aber.ac.uk}
#' @seealso \code{\link{feat.rank.re}}
#' @keywords manip
#' @examples
#' 
#' data(abr1)
#' y   <- factor(abr1$fact$class)
#' x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
#' dat <- dat.sel1(x, y, pwise=list(c("1","2"),c("3","2")),mclass=NULL,
#'   pars=valipars(sampling="boot",niter=2,nreps=5))
#' 
#' resauc = lapply(dat, function(x) feat.rank.re(x,method="fs.auc"))
#' resrf = lapply(dat, function(x) feat.rank.re(x,method="fs.rf",ntree=100))
#' 
#' mfr=ftrank.agg(resauc,resrf)
#' 
#' ### Print out characteristics of each individual FR objects
#' print(mfr$frdef)
#' 
#' ### Number of objects in mfr
#' length(mfr$ftrank)
#' 
#' ### This is FR object num.1
#' mfr$ftrank[[1]]
#'  
#' 
ftrank.agg<-function(...){

## add cleaning to remove identical lines
## accept mfr.obj as well

lclas=list(...)
ncla=length(lclas)
mfr.obj<-list()
for(i in 1:ncla){
if(inherits(lclas[[i]], "feat.rank.re"))
  mfr.obj[[length(mfr.obj)+1]]<-lclas[[i]]
else
 for(k in 1:length(lclas[[i]])){mfr.obj[[length(mfr.obj)+1]]<-lclas[[i]][[k]]}
}
cl.def<-lapply(mfr.obj,function(x) c(x$method,x$argfct,x$pars.mini,x$cl.task))
cl.def<-matrix(unlist(cl.def),ncol=4,byrow=TRUE)
cl.def<-cbind(paste("Mod_",1:nrow(cl.def),sep=""),cl.def)
tmp<-rep("",nrow(cl.def))
tmp2<-paste(cl.def[,2],cl.def[,3])
for(i in unique(tmp2)){
 tmp[tmp2==i]=which(unique(tmp2)==i)
}
cl.def<-cbind(cl.def,paste("Alg_",tmp,sep=""))
tmp<-rep("",nrow(cl.def))
for(i in unique(cl.def[,5])){
 tmp[cl.def[,5]==i]=which(unique(cl.def[,5])==i)
}
cl.def<-cbind(cl.def,paste("Dis_",tmp,sep=""))
cl.def<-cbind(cl.def,rep("",nrow(cl.def)))
dimnames(cl.def)=list(1:nrow(cl.def),
   c("Model","Alg","Arg","Pars","Dis","AlgId","DisId","Other"))
mfr.obj=list(ftrank=mfr.obj,frdef=cl.def)
class (mfr.obj) <- "ftrank.agg"
return(mfr.obj)
}
#####


