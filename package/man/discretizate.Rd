\name{discretizate}
\alias{discretizate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dyskretyzacja według równej częstości
}
\description{
Dany wektor zostatnie zdysretyzowany według częstości do podan liczby wartości.
}
\usage{
discretizate(data, maxValues = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Wektor do dyskretyzacji.
}
  \item{maxValues}{
Maksymalna liczba przedziałów.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Zdyskretyzowany wektor, który ma maxymalnie maxValues przedziałów.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
maxValues oznaczy tylko maksymalną liczbę przedziałów, jeżeli istnieje wartość, która zajmie więcej niż jeden przedział, wynikowa liczba dyskretnych wartości będzie mniejsza.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ utilities }
