\name{empRocInfo}
\alias{empRocInfo}
\title{empRocInfo}
\description{
Provides information related to the ROC given a prediction object. This function is not to be called directly in a normal use case. Instead, the other functions in this package call this function when necessary.
}
\usage{
empRocInfo(prediction)
}
\arguments{
\item{prediction}{A prediction object, output of the prediction function in the ROCR package.}
}
\value{
A RocInfo object with six components.
\item{n0}{Number of positive observations.}
\item{n1}{Number of negative observations.}
\item{pi0}{Prior probability of positive observation.}
\item{pi1}{Prior probability of negative observation.}
\item{F0}{Convex hull of ROC y values.}
\item{F1}{Convex hull of ROC x values.}
}
\references{
Verbraken, T., Wouter, V. and Baesens, B. (2013). A Novel Profit Maximizing Metric for Measuring Classification Performance of Customer Churn Prediction Models. Knowledge and Data Engineering, IEEE Transactions on. 25 (5): 961-973. Available Online: \url{http://ieeexplore.ieee.org/iel5/69/6486492/06165289.pdf?arnumber=6165289}
Verbraken, T., Bravo, C., Weber, R. and Baesens, B. (2014). Development and application of consumer credit scoring models using profit-based classification measures. European Journal of Operational Research. 238 (2): 505 - 513. Available Online: \url{http://www.sciencedirect.com/science/article/pii/S0377221714003105}
}
\author{
Seppe vanden Broucke and Thomas Verbraken.
}
\seealso{
See Also \code{\link{empChurn}}, \code{\link{empCreditScoring}}, \code{\link{prediction}}.
}
\examples{
# Construct artificial probability scores and true class labels
score.ex <- runif(1000, 0, 1)
class.ex <- unlist(lapply(score.ex, function(x){rbinom(1,1,x)}))

# Make prediction object (ROCR package)
pred.ex <- prediction(score.ex, class.ex)

# Get convex ROC info
empRocInfo(pred.ex)
}
\keyword{classif}
\keyword{evaluation}
