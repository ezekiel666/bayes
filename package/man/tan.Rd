\name{tan}
\alias{tan}
\title{
TAN
}
\description{
Klasyfikator wzmocnionych drzew Bayesa(ang. Tree Augmented Naive Bayes) swoje
działanie opiera na koncepcji sieci Bayesa. Sieć Bayesa jest graficznym modelem 
reprezentującym zbiór zmiennych losowych oraz prawdopodobieństw warunkowych 
zachodzących pomiędzy tymi zmiennymi przy pomocy acyklicznego grafu 
skierowanego. Ideą wprowadzaną przez TAN jest chęć odkrycia warunkowych 
prawdopodobieństw pomiędzy atrybutami za pomocą sieci Bayesa i wykorzystania 
tych zależności w celu poprawy jakości klasyfikacji.
}
\usage{
\method{tan}{tan}(data, \dots)
}
\arguments{
  \item{data}{
Zestaw danych dla którego ma zostać wykonana faza uczenia algorytmu TAN. 
Zostało poczynione założenie, które oczekuje, że we wprowadzanym zbiorze danych
ostatni atrybut reprezentuje klasę.
}
  \item{\dots}{
Inne arguemnty
}
}
\details{
Bardzo ważne jest aby pamiętać o tym, że ostatni atrybut w przekazywanych danych
musi reprezentować klasę. W innym przypadku działanie algorytmu będzie błędne.
}
\value{
    \item{attributes}{Informacje o atrybutach oraz ich wartościach, które 
    znajdują się w trenującym zbiorze danych.}
    \item{parents}{Lista reprezentująca rodziców każdego atrbutu w stworzonej
    sieci Bayesa.}
    \item{conditionalProbabilities}{Lista zawierająca nauczone w fazie
    trenowania prawdopodobieństwa warunkowe.}
    \item{modes}{Lista zawierająca mode wartości każdego z atrybutów 
    znajdujących się w zbiorze trenującym.}
}
\references{
}
\author{
}
\note{
}
\seealso{
\code{\link{predict.tan}}
}
\examples{
data = readData(s = 5, CV = 1, attributes = 10)
dataTrain = data[[1]]$dataTrain
model = tan(dataTrain)
}
\keyword{methods}
