% wll-26-02-2007
%
\name{fiems_lct_main}
\alias{fiems_lct_main}
\title{
  LCT Mass Binning
}
\description{
  Main Routine for \sQuote{Mass Binning} to nominal mass and \sQuote{Mass Spectrum} generation 
  in high-throughput Flow Injection Electrospray Ionisation Mass Spectrometry 
  (FIE-MS). This routine reads ANDI NetCDF files (\code{*.cdf}) of LCT/Q-ToF \code{*.raw} 
  data files converted in the DataBridge program (Dbridge, MassLynx, Micromass).                     
}
\usage{
fiems_lct_main(my_path,runinfo,y1,y2,y3,y4,limit=0.82,
               save.file=TRUE,file.name="LCT-mean.RData")
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
  \item{save.file}{A logical value indicating whether or not to save the results
  (default is \code{TRUE}).
  }
  \item{file.name}{A character for saved file name if \code{save.file} is \code{TRUE}.
  }
}

\details{
 This routine is designed to handle only one MassLynx (Micromass) specific 
 function (e.g. data acquired at one cone voltage). Each \code{*.cdf}-file will 
 result in one mass spectrum. Principle in brief: load \code{*.cdf}-file (\code{pathcdf} 
 and \code{filecdf} information in \code{runinfo.csv}); bin m/z-values to nominal mass 
 between \sQuote{limit-1} and \sQuote{limit}; sum up intensities of binned m/z values; 
 generate sample matrix \sQuote{smat} between scans y1 and y2 and background matrix 
 \sQuote{bmat} between scans y3 and y4; subtract: mat=smat-bmat; calculate mean of 
 resulting matrix \sQuote{mat}; potential negative values are set to \sQuote{zero}. The 
 implemented timer-function should be accurate for up to 24 hours which 
 could comprise more than 7000 \code{*.cdf}-files per experiment.
}

\value{
 A list containing the following components:
  \item{mat}{
    Single matrix [runs x nominal masses] of the full mass range [0:2000] in 
    the ionisation mode.
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
 The returned values are saved by default as \code{LCT-mean.RData} in folder \code{my_path}.
 Additionally, single items are saved by default as TEXT files: \code{mat.txt}, 
 \code{myparam.txt} (containing \code{scrng} and \code{limit} for reference purposes).
}

\author{
  Manfred Beckmann \email{meb@aber.ac.uk}
}

\seealso{
  \code{\link{fiems_ltq_main}}
}

\examples{

## Example profiles can be downloaded on the FIEmspro webpage
## 021016Pot-24_LCT_ESI_-.zip must be extracted in folder that defines 'my_path'

## For e.g.
\dontrun{my_path <- "D:/Temp/021016Pot-24_LCT_ESI_-"}
## The same folder should also contain a 'runinfo' file
## For e.g.
\dontrun{runinfo <- "runinfo.csv"}
## Process each profile defined in 'runinfo'
\dontrun{tmp <- fiems_lct_main(my_path,runinfo,15, 25,50,	60,limit=0.82,
               save.file=TRUE,file.name="LCT-mean.RData")}

## ===================================================================
## Arguments and matrices are saved in 'my_path', ideally the working 
## directory of the experiment. For explanations regarding input 
## arguments see below ...

# required is a file named by default 'runinfo.csv'
# (comma separated variables, generated in e.g. MS-Excel);
# the structure should be the following to ease data pre-processing:

#    A     |                  B                   |       C         |  D
# injorder | pathcdf                              | filecdf         |batch
#-------------------------------------------------------------------------
#  1       | D:/../Pot-LCT-2001-bc/Test_LCT_ESI_- | 021016MAN10.CDF | 1
#  2       | D:/../Pot-LCT-2001-bc/Test_LCT_ESI_- | 021016MAN11.CDF | 1
# and so on...

#  Columns:
# 'injorder' is injection order of samples (good for investigating drifts)
# 'pathcdf'  is path of folder containing "*.cdf"-files. Each run-sequence
#            or batch of runs might have its own folder.
# 'filecdf'  is the actual filename of an "*.cdf"-file.
# 'batch'    is the number of the batch the run belongs to (good for
#            investigating batch effects)

# In practice the file will contain further information regarding sample name,
# class/group information and probably other meta-data describing a sample.


## LCT Instrument Method for Flow-Injection-ESI-MS (FIE-MS):
## - 1 Function only, either positive or negative ionization mode
## - m/z range: 65.0-1000.0  (default max = 2000)   resolution: 4000
## - 2 min Acquisition

## Infusion Profile (Sketch):
##          _
##         / \
##        /   \
##       /     \_
##      /        \__
## ____/            \______________________________
## 0         0.5          1          1.5          2 [min]
##       |------|               |------|
##      [y1]   [y2]            [y3]   [y4]   [scan reading]
##        sample               background
## Using the above given LCT Instrument Method for FIE-MS
##   the actual scan readings y1 to y4 are used directly:
##       scrange = c(y1,y2,y3,y4)
##       with (ideally):  y2 - y1 = y4 - y3

## Raw data conversion to ANDI NetCDF-file: 
##       DataBridge program (Dbridge, MassLynx, Micromass)


}

\keyword{manip}
