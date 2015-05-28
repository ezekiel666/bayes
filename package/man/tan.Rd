\name{tan}
\alias{tan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TAN
}
\description{
Tree Augumented Naive Bayes algorithm implementation.
}
\usage{
tan(data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    Dataset representing training data. It's assumed that last column in data set alwayes denotes class.
}
  \item{\dots}{
    Further arguments.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    \item{attributes }{Information about attributes in training dataset.}
    \item{parents }{Structure which represents created Bayes network.}
    \item{conditionalProbabilities }{Values of all possible conditional probabilities.}
    \item{modes }{Mode of each attribute values.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
