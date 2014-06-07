% wll-14-06-2006: 
%

\name{predict.nlda}
\alias{predict.nlda}
\title{
Classify Multivariate Observations by 'nlda'
}

\description{
Classify multivariate observations in conjunction with \code{nlda}, and also
project data onto the linear discriminants.
}

\usage{
\method{predict}{nlda}(object, newdata, dim2use = NULL, \dots)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{object}{
  Object of class \code{nlda}.
  }
  \item{newdata}{
  A matrix or data frame of cases to be classified.  
  }
  \item{dim2use}{
   The dimension of rotated data set to be used in prediction.
  }
  \item{\dots}{
    Arguments passed to or from other methods.
  }
}

% ----------------------------------------------------------------------------
\details{
This function is a method for the generic function \code{predict()} for
class \code{nlda}. If \code{newdata} is omitted, the results of training data 
in \code{nlda} object will be returned.
}
% ----------------------------------------------------------------------------
\value{
  A list with components:
  \item{class}{
  The predicted class (a factor).
  }
  \item{x}{
  The projections of test data on discriminant variables.
  }
  \item{prob}{
  The posterior probabilities for the predicted classes.
  }
  \item{xmeans}{
  The group means obtained from training. 
  }
  \item{dim2use}{
   The dimension of rotated data set to be used in prediction.
  }
}
% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk} and Wanchang Lin \email{wll@aber.ac.uk}.
}
% ----------------------------------------------------------------------------
\seealso{
\code{\link{nlda}}, \code{\link{plot.nlda}}
}

% ----------------------------------------------------------------------------
\examples{

data(abr1)
cl   <- factor(abr1$fact$class)
dat  <- abr1$pos

## divide data as training and test data
idx <- sample(1:nrow(dat), round((2/3)*nrow(dat)), replace=FALSE) 

## constrcuct train and test data 
train.dat  <- dat[idx,]
train.t    <- cl[idx]
test.dat   <- dat[-idx,]        
test.t     <- cl[-idx] 

## apply NLDA
model    <- nlda(train.dat,train.t)
pred.te  <- predict(model, test.dat)

## confusion matrix
table(test.t,pred.te$class)

}

\keyword{classif}
