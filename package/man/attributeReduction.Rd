\name{attributeReduction}
\alias{attributeReduction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Redukcja atrybutów.
}
\description{
Atrybuty zostaną zredukowane przy użyciu funkcji prcomp, dla których skumulowana wariancja jest większa niż 99.9\%.
}
\usage{
attributeReduction(data, attributes = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dane, które mają zostać zredukowane.
}
  \item{attributes}{
Liczba atrybutów, która ma zostać utworzona, zamiast użycia skumulowanej wariancji.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Dane ze zmniejszoną liczbą atrybutów.
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
