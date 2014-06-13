package.check <- function(x)
{

# select Bristol CRAN mirror
chooseCRANmirror(ind=73)

	is.installed <- function(x){
		is.element(x, installed.packages()[,1])
			}
			
			
if(!is.installed("randomForest")){
	install.packages("randomFores")
	}
if(!is.installed("MASS")){
	install.packages("MASS")
	}

if(!is.installed("e1071")){
	install.packages("e1071")
	}

if(!is.installed("ggplot2")){
	install.packages("ggplot2")
	}

if(!is.installed("KernSmooth")){
	install.packages("KernSmooth")
	}

if(!is.installed("RColorBrewer")){
	install.packages("RColorBrewer")
	}

if(!is.installed("impute")){
	source("http://bioconductor.org/biocLite.R")
	biocLite("impute")
	}

	
print("..all dependencies installed...")

}
