%
\name{parse_vec}
\alias{parse_vec}
\title{
  Aggregation of Vectors in Nested Lists
}
\description{
  Parser to illustrate a possible output of \code{\link{accest}}. The 
  function loop other lists of list and aggregate vectors (single values)
   contained in component labelled \code{nam}.
}

\usage{
parse_vec(mlist,nam)
}

% ----------------------------------------------------------------------------
\arguments{
  \item{mlist}{List of list}
  \item{nam}{Name of the component of interest}
}

% ----------------------------------------------------------------------------
\author{
  David Enot \email{dle@aber.ac.uk}.
}


\seealso{
  \code{\link{accest}}, \code{\link{parse_freq}}
}

\examples{

l1=list(list(v=1),list(v=2),list(v=3))
l2=list(list(v=4),list(v=5),list(v=6))
res=list(l1,l2)

## show list of list
print(res)

## tidy up
parse_vec(res,nam="v")

######## Second example
## Real world problem described in accest

}

\keyword{manip}
