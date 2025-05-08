\name{empChurn}
\alias{empChurn}
\title{empChurn}
\description{
Estimates the EMP for customer churn prediction, considering constant CLV and a given cost of contact f and retention offer d.
}
\usage{
empChurn(scores, classes, alpha = 6, 
beta = 14, clv = 200, d = 10, f = 1)
}
\arguments{
\item{scores}{A vector of predicted probabilities.}
\item{classes}{A vector of true binary class labels.}
\item{alpha}{Alpha parameter of unimodel beta distribution.}
\item{beta}{Beta parameter of unimodel beta distribution.}
\item{clv}{Constant CLV per retained customer.}
\item{d}{Constant cost of retention offer.}
\item{f}{Constant cost of contact.}
}
\value{
An EMP object with four components.
\item{MP}{The Maximum Profit of the ROC curve at MPfrac cutoff.}
\item{MPfrac}{The percentage of cases that should be excluded, that is, the percentual cutoff at MP profit.}
\item{EMP}{The Expected Maximum Profit of the ROC curve at EMPfrac cutoff.}
\item{EMPfrac}{The percentage of cases that should be excluded, that is, the percentual cutoff at EMP profit.}
}
\references{
Verbraken, T., Wouter, V. and Baesens, B. (2013). A Novel Profit Maximizing Metric for Measuring Classification Performance of Customer Churn Prediction Models. Knowledge and Data Engineering, IEEE Transactions on. 25 (5): 961-973. Available Online: \doi{10.1109/TKDE.2012.50}
Verbraken, T., Bravo, C., Weber, R. and Baesens, B. (2014). Development and application of consumer credit scoring models using profit-based classification measures. European Journal of Operational Research. 238 (2): 505 - 513. Available Online: \doi{10.1016/j.ejor.2014.04.001}
}
\author{
Cristian Bravo, Seppe vanden Broucke and Thomas Verbraken.
}
\seealso{
See Also \code{\link{empChurn}}, \code{\link[ROCR:prediction]{prediction}}.
}
\examples{
# Construct artificial probability scores and true class labels
score.ex <- runif(1000, 0, 1)
class.ex <- unlist(lapply(score.ex, function(x){rbinom(1,1,x)}))

# Calculate EMP measures for customer churn prediction
empChurn(score.ex, class.ex)

# Calculate EMP measures for customer churn prediction with
# lower clv and higher costs
empChurn(score.ex, class.ex, clv = 100, d = 30, f = 5)
}
\keyword{classif}
\keyword{evaluation}
