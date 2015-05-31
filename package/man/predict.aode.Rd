\name{predict.aode}
\alias{predict.aode}
\title{
AODE
}
\description{
Faza predykcji algorytmu AODE.
}
\usage{
predict.aode(model, newdata, type = c("class", "raw"), m = 1, ...)
}
\arguments{
  \item{model}{
Model
}
  \item{newdata}{
Macierz atrybutów przykładów dla których ma zostać wykonana predykcja. Nazwy
kolumn muszą odpowiadać tym ze zbioru trenującego
}
  \item{type}{
Wskazuje rodzaj zwracanego wyniku \code{\{"class", "raw"\}}
}
  \item{m}{
Dolne ograniczenie częstości
}
  \item{\dots}{
Pozostałe argumenty
}
}
\details{
}
\value{
Jeśli \code{type = "class"} to dla każdego przykładu zwraca klasę najbardziej
prawdopodobną.
Jeśli \code{type = "raw"} to dla każdego przykładu zwraca listę 
prawdopodobieństw dla każdej z klas.
}
\references{
}
\author{
}
\note{
}
\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{methods}
