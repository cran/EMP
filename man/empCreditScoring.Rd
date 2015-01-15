\name{empCreditScoring}
\alias{empCreditScoring}
\title{empCreditScoring}
\description{
Estimates the EMP for credit risk scoring, considering constant ROI and a bimodal LGD function with point masses p0 and p1 for no loss and total loss, respectively.
}
\usage{
empCreditScoring(scores, classes, p0=0.55, p1=0.1, ROI=0.2644)
}
\arguments{
\item{scores}{A vector of predicted probabilities.}
\item{classes}{A vector of true binary class labels.}
\item{p0}{Percentage of cases on the first point mass of the LGD distribution (complete recovery).}
\item{p1}{Percentage of cases on the second point mass of the LGD distribution (complete loss).}
\item{ROI}{Constant ROI per granted loan. A percentage.}
}
\value{
An EMP object with two components.
\item{EMP}{The Expected Maximum Profit of the ROC curve at EMPfrac cutoff.}
\item{EMPfrac}{The percentage of cases that should be excluded, that is, the percentual cutoff at EMP profit.}
}
\references{
Verbraken, T., Wouter, V. and Baesens, B. (2013). A Novel Profit Maximizing Metric for Measuring Classification Performance of Customer Churn Prediction Models. Knowledge and Data Engineering, IEEE Transactions on. 25 (5): 961-973. Available Online: \url{http://ieeexplore.ieee.org/iel5/69/6486492/06165289.pdf?arnumber=6165289}
Verbraken, T., Bravo, C., Weber, R. and Baesens, B. (2014). Development and application of consumer credit scoring models using profit-based classification measures. European Journal of Operational Research. 238 (2): 505 - 513. Available Online: \url{http://www.sciencedirect.com/science/article/pii/S0377221714003105}
}
\author{
Cristian Bravo, Seppe vanden Broucke and Thomas Verbraken.
}
\seealso{
See Also \code{\link{empChurn}}, \code{\link{prediction}}.
}
\examples{
# Construct artificial probability scores and true class labels
score.ex <- runif(1000, 0, 1)
class.ex <- unlist(lapply(score.ex, function(x){rbinom(1,1,x)}))

# Calculate EMP measures for credit risk scoring
empCreditScoring(score.ex, class.ex)

# Calculate EMP measures for credit risk scoring with point masses
# in 0.1 and 0.9, and 0.1 ROI
empCreditScoring(score.ex, class.ex, 0.1, 0.1, 0.1)
}
\keyword{classif}
\keyword{evaluation}
