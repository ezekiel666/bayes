\name{addCalculatedResults}
\alias{addCalculatedResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dodaj wyniki
}
\description{
Dodaje wyliczone wyniki do poprzednich wyników, jeżeli poprzedni wynik jest NULLem, zwraca same wyniki.
}
\usage{
addCalculatedResults(predictionTable, oldResults = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predictionTable}{
Tablica z przwidzianymi klasami.
}
  \item{oldResults}{
Wyniki, do których zostaną dodane wyliczone wyniki.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{accuracy }{(tt + ff) / (ff + tf + ft +tt)}
  \item{error }{(tf + ft) / (ff + tf + ft +tt)}
  \item{precision }{(tt) / (ft + tt)}
  \item{sensitivity }{(tt) / (tf + tt)}
  \item{falseNegativeRate }{(tf) / (tf + tt)}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ utilities }
