\name{EMP_CS}
\alias{EMP_CS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EMP_CS
}
\description{
Estimates the EMP for credit scoring, considering constant ROI and a bimodal
LGD function with point masses p0 and p1 for no loss and total loss, 
respectively.
}
\usage{
EMP_CS(ROC, p0 = 0.55, p1 = 0.1, ROI = 0.2644)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ROC}{
  A ROC object, output of calcROC().
}
  \item{p0}{
  Percentage of cases on the first point mass of the LGD distribution
  (complete recovery).
}
  \item{p1}{
  Percentage of cases on the second point mass of the LGD distribution 
  (complete loss).
}
  \item{ROI}{
Constant ROI per granted loan. A percentage.
}
}
\value{
An EMP object with two components.
\item{EMP}{The Expected Maximum Profit of the ROC curve at EMPfrac cutoff.}
\item{EMPfrac}{The percentage of cases that should be excluded, that is, the 
percentual cutoff at EMP profit.}
}
\references{
Verbraken T., Bravo C., Weber, R., and Baesens, B. 2014. Development and 
application of consumer credit scoring models using profit-based classification 
measures. European Journal of Operational Research. Forthcomming.
}
\author{
Cristian Bravo and Thomas Verbraken.
}
\seealso{
See Also \code{\link{calcROC}}.
}
\examples{
# Dummy dataset.
score.ex <- runif(1000, 0, 1)
class.ex <- lapply(score.ex, function(x){rbinom(1,1,x)})
class.ex <- unlist(class.ex)

# Calculate ROC curve.
ROC.ex <- calcROC(score.ex, class.ex)

# Estimates EMP with point masses in 0.1 and 0.9, and 0.1 ROI
EMP.ex <- EMP_CS(ROC.ex, 0.1, 0.1, 0.1)
EMP.ex
}
\keyword{classif}
\keyword{evaluation}
