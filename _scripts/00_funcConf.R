# conf

# This function evaluates one model's performance against another model using
# various machine learning metrics for classified data. 

# model <- a model containing presence of 0 or 1
# comparison <- a comparison model containing presence of 0 or 1
# area <- a vector containing area measurements

conf <- function(model, comparison, area){
  t.positive <- sum(area[(model == 1 & comparison == 1)])
  t.negative <- sum(area[(model == 0 & comparison == 0)])
  f.positive <- sum(area[(model == 1 & comparison == 0)])
  f.negative <- sum(area[(model == 0 & comparison == 1)])

  # Accuracy
  accuracy <- (t.positive + t.negative) / 
    (t.positive + t.negative + f.positive + f.negative)
  
  # Error Rate (1 - Accuracy)
  err <- (f.positive + f.negative) / 
    (t.positive + t.negative + f.positive + f.negative)
  
  # Sensitivity
  sens <- t.positive / (t.positive + f.negative)
  
  # Specificity
  spec <- t.negative / (t.negative + f.positive)
  
  # Precision
  prec <- t.positive / (t.positive + f.positive)
  
  # Recall (same calc as Sensitivity)
  rec <- t.positive / (t.positive + f.negative)
  
  # F-Measure [f <- (2 * prec * rec) / (prec + rec)]
  f.measure <- (2 * t.positive) / (2 * t.positive + f.positive + f.negative)
  
  # Matthews correlation coefficient 
  mcc <- (t.positive * t.negative - f.positive * f.negative) /
    sqrt((t.positive + f.positive) * (t.positive + f.negative) * 
           (t.negative + f.positive) * (t.negative + f.negative))
  
  # Wrap into a dataframe
  conf  <- data.frame(cbind(model = deparse(substitute(model)), 
                            comparison = deparse(substitute(comparison)),
                           t.positive, t.negative, f.positive, f.negative,
                           accuracy, err, sens, prec, rec, 
                           f.measure, mcc))
  return(conf)
}



