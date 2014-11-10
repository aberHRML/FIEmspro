\name{tidy.ftrank}
\alias{tidy.ftrank}
\title{ Tidy up multiple resampling based ranking results.}
\description{
 Convenience function to ease the output of \code{\link{summ.ftrank}} objects.
}
\usage{
tidy.ftrank(frsum, lmod = NULL, sorting = "Stat", tidy = "DisId", nam = "AlgId", decreasing = TRUE, file = NULL)
}
\arguments{
  \item{frsum}{\code{mfr.sum} objects}
  \item{lmod}{List of objects to be printed out - Default all objects}
  \item{sorting}{Should the results be sorted according to an argument contained in \code{frsum$frdef} - Default sorted by variable name}
  \item{tidy}{Tidy the output according to an argument contained in \code{frsum$frdef}}
  \item{nam}{Concatenate the original column names with content in to an argument contained in \code{frsum$frdef}}
  \item{decreasing}{Sorting order if argument \code{sorting} is not NULL}
  \item{file}{Write results into one file or several files}
}
\details{
 This function has been designed to provide maximum facilities to ease both screen printing and file write of complex list of resampling based feature ranking resulting from different comparisons and/or different FR settings. It uses heavily the information contained in the FR definition table (\code{frdef} in the \code{frsum} object) to: 1) merge different tables (1 FR method on 1 dataset) in a bigger table (several FR and/or datasets) 2) sort each ranking table given a statistical output and 3) group tables into subsets corresponding to FR technique or datasets. File writing option can be rather useful when 1), 2) or 3) must be repeated manually several times:
 \itemize{
  \item{}{Parameter \code{nam} controls the string that can be added at the end of each column in order to avoid confusion when merging individual. For e.g. if Alg is chosen, the value contained in the column Alg of \code{frsum$frdef} is added to Stats, Rank etc...}
  \item{}{If a table should be sorted before merging, the parameter \code{sorting} should be set accordingly. For e.g. sorting the FR result according to the rank is easliy made by \code{sorting="Rank")}. The field \code{decreasing} should be also set to FALSE if results must presented in increasing order.}
  \item{}{By setting the parameter \code{tidy} to one of the column name of \code{frsum$frdef}, a list of tables involving only identical values in \code{tidy} will be merged together.  In the example involving 5 discrimination tasks and 2 feature ranking methods illustrated below, results are either sorted by FR technique or by discrimination problem.}
  \item{}{Results can be redirect to a file or several files so that further manipulation can be made in a more \code{friendly} spreadsheet software. When the results are tidied, several files are generated.}
 }
 
}
\value{
 List of tables or list of files.
}
\author{David Enot \email{dle@aber.ac.uk}}
\seealso{\code{\link{summ.ftrank}}}
\examples{

################################################################
### Example involving 5 discrimination tasks by 2 feature ranking techniques
data(abr1)
y   <- factor(abr1$fact$class)
x <- preproc(abr1$pos , y=y, method=c("log10","TICnorm"),add=1)[,110:500]  
## Select classes all pairwise problems involving class with 1 
dat <- dat.sel1(x, y, pwise="1",mclass=NULL,pars=valipars(sampling="boot",niter=2,nreps=5))

### Perform AUC and Random Forest ranking
resauc = lapply(dat, function(x) feat.rank.re(x,method="fs.auc"))
resrf = lapply(dat, function(x) feat.rank.re(x,method="fs.rf",ntree=100))

################################################################
## Aggregate all the models
mfr=ftrank.agg(resauc,resrf)
print(mfr$frdef)
## Compute mrp-val for all the FR models
frsum=summ.ftrank(mfr,qtl=.7)

## print the FR components
print(frsum$frdef)

## have a look at the first 5 variables in the second model
print(frsum$frsum[[2]][1:5,])

################################################################
##### Let's concentrate on the models 1 and 6 first
## No need to tidy here - no sorting of the results and 
## add the content of "Alg" in the columns names
res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting=NULL,nam="Alg")

## because we did not tidy the ranking tables
## rankings from models 1 and 6 are concatenated
## in the first field of res 
## print out the first 5 lines
res[1:5,]

## Same as before but we sort the results according to "Stat" 
## in decreasing order
res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting="Stat",nam="Alg",decreasing=TRUE)
res[1:5,]

## Same as before but we sort the results according to "Rank" 
## in increasing order (of course)
res=tidy.ftrank(frsum,lmod=c(1,6),tidy=NULL,sorting="Rank",nam="Alg",decreasing=FALSE)
res[1:5,]

################################################################
###### Tidy all the rankings according to the discrimination task
## in this case we set tidy="Dis"
## all models: lmod=NULL
## sorting according to Stat
## append the name contained in Alg to the column name

res=tidy.ftrank(frsum,lmod=NULL,tidy="Dis",sorting="Stat",nam="Alg")

## Discrimination task tags are the name for each field of res
names(res)
## same as before for discrimination'1~2' or 1
res[[1]][1:5,]
res$'1~2'[1:5,]

## discrimination'2~3' or 2 ...
res[[2]][1:5,]


################################################################
###### Tidy all the rankings according to the FR method
## in this case we set tidy="Alg" for e.g.
## all models: lmod=NULL
## sorting according to Stat
## append the name contained in Dis to the column name

res=tidy.ftrank(frsum,lmod=NULL,tidy="Alg",sorting="Stat",nam="Dis")

## FR technique method are the name for each field or res
names(res)

## the top 5 variables highlighted by AUC in the 5 comparisons ...
res$'fs.auc'[1:5,]

################################################################
###### Redirect the output to one or several files
## print each tables to a CSV files starting by "testtidy"
## followed by the string contained in tidy (here "Dis")
\dontrun{tidy=tidy.ftrank(frsum,lmod=NULL,tidy="Dis",sorting="Stat",nam="Alg",file="testtidy")}


}
\keyword{manip}
