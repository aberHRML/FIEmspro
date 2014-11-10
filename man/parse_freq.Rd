%
\name{parse_freq}
\alias{parse_freq}
\title{
  Output Variable Frequencies in Nested Lists
}
\description{
  Parser to illustrate a possible output of \code{\link{accest}}. The 
  function loop other lists of list and retrieve the frequency of the variable 
  names contained in the component labelled \code{nam}.
}

\usage{
parse_freq(mlist,nam,sorting=TRUE)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{mlist}{List of list}
  \item{nam}{Name of the component of interest}
  \item{sorting}{Should the variable name be sorted by frequency?}
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk}
}


\seealso{
  \code{\link{accest}}, \code{\link{parse_vec}}
}

\examples{

l1=list(list(v=1),list(v=c(1,2)),list(v=1))
l2=list(list(v=c(1,2,3)),list(v=2),list(v=c(3,4)))
res=list(l1,l2)

## show list of list
print(res)

## tidy up
parse_freq(res,nam="v")

######## Second example
## Real world problem described in accest


}

\keyword{manip}
