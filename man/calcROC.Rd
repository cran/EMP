\name{calcROC}
\alias{calcROC}
\title{
calcROC
}
\description{
Computes the convex hull of the ROC curve created from scores and a positive
class.
}
\usage{
calcROC(score, class, positive = 1)
}
\arguments{
  \item{score}{
  The probability, in the range [0,1] to be scored.
}
  \item{class}{
  The class of the cases. Either 0 or 1.
}
  \item{positive}{
  Optional. The class for which to build the ROC curve. Defaults to 1.
}
}
\value{
An object with the convex hull of the ROC curve, with all the parameters 
necessary to estimate EMP.
\item{n1}{Number of positive cases.}
\item{n2}{Number of negative cases.}
\item{pi0}{Percentage of negative cases.}
\item{pi1}{Percentage of positive cases.}
\item{F1roc}{Cumulative ROC over positive cases.}
\item{F0roc}{Cumulative ROC over negative cases.}
\item{F1ch}{X axis of the convex hull of the ROC curve}
\item{F0ch}{Y axis of the convex hull of ROC curve}
}
\references{
Verbraken, T., Verbeke, W. & Baesens, B., 2011. Novel Profit Maximizing Metrics
for Measuring Classification Performance of Customer Churn Prediction Models.
IEEE Transactions on Knowledge and Data Engineering \bold{25(2)}: 961-973.
}
\author{
Cristian Bravo and Thomas Verbraken.
}
\seealso{
See Also \code{\link{EMP_CS}}.
}
\examples{
# Dummy dataset.
score.ex <- runif(1000, 0, 1)
class.ex <- lapply(score.ex, function(x){rbinom(1,1,x)})
class.ex <- unlist(class.ex)

# Calculate ROC curve.
ROC.ex <- calcROC(score.ex, class.ex)
ROC.ex
}
\keyword{evaluation}
\keyword{classif}
