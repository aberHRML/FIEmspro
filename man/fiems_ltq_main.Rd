% wll-26-02-2007
%
\name{fiems_ltq_main}
\alias{fiems_ltq_main}
\title{
  LTQ Mass Binning
}
\description{
  Main Routine for \sQuote{Mass Binning} to nominal mass and \sQuote{Mass Spectrum} 
  generation in high-throughput Flow Injection Electrospray Ionisation Mass Spectrometry 
  (FIE-MS). This routine reads ANDI NetCDF files (\code{*.cdf}) of LTQ \code{*.raw} 
  data files converted in the Xconvert program (Xcalibur, Thermo Finnigan).                     
}

\usage{
fiems_ltq_main(my_path,runinfo, y1,y2,y3,y4,limit=0.7,
               itp=65537,itn=131073,hrng=50,lrng=15,
               save.file=TRUE,file.name="LTQ-mean.RData")
}
\arguments{
  \item{my_path}{ A character string indicating the working directory where 
  \code{runinfo.csv} file and folder containing \code{*.cdf}-files are located.
  }
  \item{runinfo}{ A \code{*.csv} file containing at least the following run 
  information (header row): \code{pathcdf} and \code{filecdf} . For details, see the 
  description in \code{Examples} below.
  }
  \item{y1}{ A numeric value used for mass spectrum generation: start scan
  \sQuote{sample}. For details, see the description in \code{Examples} below. 
  }
  \item{y2}{ A numeric value used for mass spectrum generation: end scan 
  \sQuote{sample}. For details, see the description in \code{Examples} below. 
  }
  \item{y3}{ A numeric value used for mass spectrum generation: start scan 
  \sQuote{background}. For details, see the description in \code{Examples} below. 
  }
  \item{y4}{ A numeric value used for mass spectrum generation: end scan 
  \sQuote{background}. For details, see the description in \code{Examples} below. 
  }
  \item{limit}{ A numeric value defining the rounding limit for binning 
  m/z-values to nominal mass. 
  }
  \item{itp}{\code{itp} is an identifier for Scan Type \sQuote{full} and Data Type 
  \sQuote{centroid} (Instrument Setup, Xcalibur) for data scans acquired in positive 
  ionisation mode using the LTQ (Default=65537).
  }
  \item{itn}{\code{itp} is an identifier for Scan Type \sQuote{full} and Data Type 
  \sQuote{centroid} (Instrument Setup, Xcalibur) for data scans acquired in negative 
  ionisation mode using the LTQ (Default=131073).
  }
  \item{hrng}{\code{hrng} is an identifier for the high mass range. The argument
  has to match the \sQuote{First Mass (m/z)} of the scan range used for acquiring 
  data using the LTQ \sQuote{Instrument Setup -> Mass Range: normal}.
  }
  \item{lrng}{\code{lrng} is an identifier for the low mass range. The argument
  has to match the \sQuote{First Mass (m/z)} of the scan range used for acquiring 
  data in LTQ \sQuote{Instrument Setup -> Mass Range: low}.
  }
  \item{save.file}{A logical value indicating whether or not to save the results
  (default is \code{TRUE}).
  }
  \item{file.name}{A character for saved file name if \code{save.file} is \code{TRUE}.
  }
}

\details{
 This routine is designed to handle four Scan Events (Xcalibur, Thermo 
 Finnigan). Each \code{*.cdf}-file will result four mass spectra. Principle in 
 brief: load \code{*.cdf}-file (\code{pathcdf} and \code{filecdf} information in 
 \code{runinfo.csv}); sort scans (m/z and intensity values) into four lists. 
 For each list: bin m/z-values to nominal mass 
 between \sQuote{limit - 1} and \sQuote{limit}; sum up intensities of binned m/z values; 
 generate sample matrix \sQuote{smat} between scans y1 and y2 and background matrix 
 \sQuote{bmat} between scans y3 and y4; subtract: mat=smat-bmat; calculate mean of 
 resulting matrix \sQuote{mat}; potential negative values are set to \sQuote{zero}. The 
 implemented timer-function should be accurate for up to 24 hours which 
 could comprise more than 7000 \code{*.cdf}-files per experiment.
 See the description in the \code{Examples} below. 
}

\value{
 A list containing the following components:
  \item{posh}{
    Matrix [runs x nominal masses] of high mass range in positive ionisation 
    mode.
  }
  \item{posl}{
    Matrix [runs x nominal masses] of low mass range in positive ionisation 
    mode.
  }
  \item{negh}{
    Matrix [runs x nominal masses] of high mass range in negative ionisation 
    mode.
  }
  \item{negl}{
    Matrix [runs x nominal masses] of low mass range in negative ionisation 
    mode.
  }
  \item{runinfo}{
    Same as argument stored for reference purposes. Additional information for 
    each run like sample name or class can be used for further analysis 
    (e.g. nlda).
  }
  \item{scrng}{
    A vector of \code{y1}, \code{y2}, \code{y3} and \code{y4} stored for reference purposes.
  }
  \item{limit}{
    Same as argument stored for reference purposes.
  }
}

\note{
 The returned values are saved by default as \code{LTQ-mean.RData} in folder \code{my_path}.
 Additionally, single items are saved by default as TEXT files: \code{posh.txt}, 
 \code{posl.txt}, \code{negh.txt}, \code{negl.txt}, \code{myparam.txt} 
 (containing \code{scrng} and \code{limit} for reference purposes).

}

\author{
  Manfred Beckmann \email{meb@aber.ac.uk}
}

\seealso{
  \code{\link{fiems_lct_main}}
}

\examples{
## To run fiems_lqt_main, copy and paste the following code segment. Uncomment
## and change the file path and name appropriately.


## Example profiles can be downloading on the FIEmspro webpage
## 050509-Abr1.zip must be extrated in folder that defines 'my_path'

## For e.g.
\dontrun{my_path <- "D:/Temp/050509-Abr1"}
## The same folder should also contain a 'runinfo' file
## e.g.
\dontrun{runinfo <- "runinfo.csv"}
## Process each profile defined in 'runinfo'
\dontrun{tmp <- fiems_lct_main(my_path,runinfo,35,95,190,250,limit=0.82,
               save.file=TRUE,file.name="LTQ-mean.RData")}

## ===================================================================
## Arguments and matrices are saved in 'my_path', ideally the working 
## directory of the experiment. For explanations regarding input 
## arguments see below ...

## required is a file named by default 'runinfo.csv'
## (comma separated variables, generated in e.g. MS-Excel)
## the structure should be the following to ease data pre-processing:

##    A     |             B                   |    C       |   D
## injorder | pathcdf                         | filecdf    | batch
##-----------------------------------------------------------------
##  1       | D:/../070122-ABR1-A-repeat/cdf  | 01.cdf     | 1
##  2       | D:/../070122-ABR1-A-repeat/cdf  | 02.cdf     | 1
## and so on...

##  Columns:
## 'injorder' is injection order of samples (good for investigating drifts)
## 'pathcdf'  is path of folder containing "*.cdf"-files. Each run-sequence
##            or batch of runs might have its own folder.
## 'filecdf'  is the actual filename of an "*.cdf"-file.
## 'batch'    is the number of the batch the run belongs to (good for
##            investigating batch effects)

## In practice the file will contain further information regarding sample name,
## class/group information and probably other meta-data describing a 
## sample.

## LTQ Instrument Method for Flow-Injection-ESI-MS (FIE-MS):
## - 1 Segment, 5 min Acquisition
## - 4 Scan Events:
##   -- 1: ITMS + c norm o(50.0-2000.0)
##   -- 2: ITMS + c low injrf=20.0 o(15.0-200.0)
##   -- 3: ITMS - c norm o(50.0-2000.0)
##   -- 4: ITMS - c low injrf=20.0 o(15.0-200.0)

## Infusion Profile (Sketch):
##           _
##          / \
##         /   \
##        /     \_
##       /        \______
## _____/                \________________________
## 0       1        2        3         4         5 [min]
##       |--- ---|               |-------|
##      [x1]   [x2]            [x3]    [x4]   [scan reading]
##        sample                background
## Using the above LTQ Instrument Method for FIE-MS
##   the actual scan readings x1 to x4 of e.g. scan event 1 have to be
##   subtracted by 1 (the Scan Event) and
##   divided by 4 (total of 4 Scan Events):
##      e.g.  [y1] = ([x1]-1)/4  =>  scrange = c(y1,y2,y3,y4)
##      with (ideally):  y2 - y1 = y4 - y3

## Raw data conversion to ANDI NetCDF-file: 
##      XConvert-program (Xcalibur, Thermo-Finnigan)


}

\keyword{manip}
