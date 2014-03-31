EMP_CS <- function(ROC,p0=0.55,p1=0.1,ROI=0.2644){
  # Adapted from:
  # Verbraken T., Bravo C., Weber, R., and Baesens, B. 2014. Development and 
  # application of consumer credit scoring models using profit-based 
  # classification measures. European Journal of Operational Research.
  # Forthcomming.
  # This software comes with absolutely no warranty. Use at your own risk.
  # That said, it should work! :D.
  # This code calculates the EMP measure for credit scoring, assuming a
  # bimodal distribution for the LGD with point masses p0 and p1.
  # args:
  #   ROC: A ROC object, output of CalcROC().
  #   p0:   Percentage of cases on the first point mass of the LGD distribution
  #   (complete recovery).
  #   p1:   Percentage of cases on the second point mass of the LGD distribution 
  #   (complete loss).
  #   ROI: Constant ROI per granted loan. Percentage.
  # Returns:
  #   An EMP object with the following attributes:
  #     EMP: The Expected Maximum Profit for the dataset.
  #     EMPfrac: The percentage of cases that should be excluded (i.e.
  #               the cut-off percentage of cases).
  
  alpha <- 1-p0-p1
  
  #Calculate lambda
  lambda <- c(0,(ROC$pi1*ROI/ROC$pi0)*diff(ROC$F1ch)/diff(ROC$F0ch))
  lambda <- c(lambda[lambda<1],1)
  
  #EMPC
  lambdaii <- head(lambda,n=-1)
  lambdaie <- tail(lambda,n=-1)
  F0 <- ROC$F0ch[1:length(lambdaii)]
  F1 <- ROC$F1ch[1:length(lambdaii)]
  
  EMP <- sum(alpha*(lambdaie-lambdaii)*(ROC$pi0*F0*(lambdaie+lambdaii)/2 - ROI*F1*ROC$pi1))+(ROC$pi0*tail(F0,n=1)-ROI*ROC$pi1*tail(F1,n=1))*p1
  EMPfrac <- sum(alpha*(lambdaie-lambdaii)*(ROC$pi0*F0+ROC$pi1*F1))+p1*(ROC$pi0*tail(F0,n=1)+ROC$pi1*tail(F1,n=1))

  return(list(EMP=EMP,EMPfrac=EMPfrac))
}