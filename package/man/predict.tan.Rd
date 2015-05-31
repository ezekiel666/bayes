\name{predict.tan}
\alias{predict.tan}
\title{
TAN
}
\description{
Metoda wykonująca predykcje na nowych danych przy użyciu modelu uzyskanego
podczas wcześniej przeprowadzonej fazy trenującej.
}
\usage{
\method{tan}{predict}(object, newdata, type = c("class", "raw"), \dots)
}
\arguments{
  \item{object}{
objekt klasy "tan"
}
  \item{newdata}{
Dane dla których ma zostać przeprowadzona predykcja. Kolejność atrybutów musi
odpowiadać atrybutom ze zbioru trenującego. Kolumna zawierająca klasę może 
zostać pominięta.
}
  \item{type}{
Wynik jaki ma zostać zwrócony dla każdego przykładu. "raw" spowoduje zwrócenie
prawdopodobieństwa dla każdej z klas, "class" zwróci klasę najbardziej 
prawdopodobną.
}
  \item{\dots}{
Pozostałe argumenty.
}
}
\value{
Jeśli type = "class" to zostanie zwrócona tablica klas.
Jeśli type = "raw" to zostanie zwrócona tablica retVal. Każdy element tablicy to
lista zawierająca prawdopodobieństwa każdej z klas.
}
\references{
}
\author{
}
\note{
}
\seealso{
\code{\link{tan}}
\code{\link{bayes}}
}
\examples{
data = readData(s = 5, CV = 1, attributes = 10)
model = tan(data[[1]]$dataTrain)
predicted_class = predict(model, data[[1]]$dataTest, type = "class")
predicted_raw = predict(model, data[[1]]$dataTest, type = "raw")
}
\keyword{ method }
