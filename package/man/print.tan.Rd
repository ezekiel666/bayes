\name{tan}
\alias{tan}
\title{
TAN
}
\description{
Wypisuje wartości parametrów modelu TAN.
}
\usage{
\method{tan}{print}(object, \dots)
}
\arguments{
  \item{object}{
Objeckt klasy "tan".
}
  \item{\dots}{
Inne arguemnty
}
}
\references{
}
\author{
}
\note{
}
\seealso{
\code{\link{predict.tan}}
\code{\link{tan}}
}
\examples{
data = readData(s = 5, CV = 1, attributes = 10)
dataTrain = data[[1]]$dataTrain
model = tan(dataTrain)
print(model)
}
\keyword{methods}