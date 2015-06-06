\name{readData}
\alias{readData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ładowanie danych
}
\description{
Zostaną załadowane dane SPAM i zostaną wyknonane operacje przygotowawcze.
}
\usage{
readData(s = 0, supervised = FALSE, maxValues = 10, reduce = TRUE, attributes = 30, CV = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
Liczba próbek do pobrania z danych, jeżeli nie potrzeba całych danych. 0 oznacza pobranie całego zestawu danych. Spróbkowane dane będą według rozkładu klasy, ale nie atrybutów.
}
  \item{supervised}{
Czy dykretyzacja ma być z nadzorem. Trwa to o wiele dłużej.
}
  \item{maxValues}{
Maksymalna liczba przedziałów do dyskretyzacji, brak efektu przy dyskretyzacji z nadzorem.
}
  \item{reduce}{
Czy ma nastąpić redukcja atrybutów.
}
  \item{attributes}{
Liczba atrybutów, które mają pozostać po redukcji. 0 aby dokonać automatycznej redukcji atrybutów.
}
  \item{CV}{
Liczba kros walidacji do których maja zostać przygotowane dane. Zostane one podzielone na tyle przedziałów i następnie do każdej wartości zostanie wybrany inna część jako dane testujące.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
