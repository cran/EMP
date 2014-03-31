calcROC <- function(score,class,positive=1){
  # Adapted from:
  # Verbraken, T., Verbeke, W. & Baesens, B., 2011. Novel Profit Maximizing
  # Metrics for Measuring Classification Performance of Customer Churn 
  # Prediction Models. IEEE Transactions on Knowledge and Data Engineering. 
  # 25(2): 961-973. 2013.
  # This software comes with absolutely no warranty. Use at your own risk.
  # That said, it should work! :D
  # Compute the convex hull of the ROC curve created from scores and a 
  # positive class.
  # args:
  #   score: The probability, in the range [0,1] to be scored.
  #   class: The class of the cases. Either 0 or 1.
  #   positive: Optional. The class for which to build the ROC curve. Defaults
  #   to 1.
  # Returns:
  #   CalcROC: An object to pass to EMP_CS().
  
  if(positive == 1){
    score <- -score
    class <- 1-class
  }
  
  x <- sort(score,index.return = TRUE)
  idx <- x$ix
  score <- x$x
  rm(x)

  #Rank the score from small to large and apply the same to the class vector
  class <- class[idx]
  class <- cbind(1-class,class)
  
  #Prior probabilites
  n0 <- sum(class[,1])
  n1 <- sum(class[,2])
  pi0 <- n0/(n0+n1)
  pi1 <- n1/(n0+n1)
  
  #ROC List.
  ROC = list(n1=n1,n0=n0,pi0=pi0,pi1=pi1)
  
  #Fast ROC calculation
  id1 <- !duplicated(score,fromLast=TRUE)
  R <- apply(class,2,cumsum)
  R <- rbind(c(0,0),cbind(R[id1,1]/n0,R[id1,2]/n1))
  ROC$F1roc <- R[,2]
  ROC$F0roc <- R[,1]
  
  #Construct the convex hull
  ROC$F1ch <- 0
  ROC$F0ch <- 0
  j=1;
  toler=10e-9
  while(j<nrow(R)){
    slope_next <- (ROC$F0roc[-(1:j)]-ROC$F0roc[j])/(ROC$F1roc[-(1:j)]-ROC$F1roc[j])
    imax <- which(slope_next >=(max(slope_next)-toler),arr.ind=TRUE)
    imax <- max(imax)
    ROC$F1ch <- rbind(ROC$F1ch,ROC$F1roc[j+imax])
    ROC$F0ch <- rbind(ROC$F0ch,ROC$F0roc[j+imax])
    j <- j+imax
  }
  return(ROC)
}