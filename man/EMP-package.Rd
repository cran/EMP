\name{EMP-package}
\alias{EMP-package}
\alias{EMP}
\docType{package}
\title{
Expected Maximum Profit for Credit Scoring.
}
\description{
The EMP measure is an alternative to AUC that includes the expected profit of
a given model, when compared to a baseline (no model used). Presented in 
Verbraken et al. (2014) as a preferred measure for credit scoring in any 
profit-driven environment. This implementation assumes an LGD distribution
with two point masses, and a constant ROI.
}
\details{
\tabular{ll}{
Package: \tab EMP\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2014-08-24\cr
License: \tab GPL (>=3)\cr
}
The package consists of only two functions, EMP_CS and calcROC. The second one
estimates the convex hull of any ROC function starting from the scores and the
classes, while the second estimates the EMP in this particular case. See the
help pages of each function for details.
}
\author{
Authors: Cristian Bravo and Thomas Verbraken.
Mantainer: Cristian Bravo <crbravo@utalca.cl>.
}
\references{
Verbraken, T., Bravo, C., Weber, R. and Baesens, B. (2014). Development and application of consumer credit scoring models using profit-based classification measures. European Journal of Operational Research. 238 (2): 505 - 513. Available Online: \url{http://www.sciencedirect.com/science/article/pii/S0377221714003105}
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
