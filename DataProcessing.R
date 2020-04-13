
benefit_from_CBH <-function(CBH){
  benefit <- exp(-0.23102 + 0.80001 * log(CBH))
  return (benefit)
}

health_score <- function(benefit){
  score <- round(100 * benefit / (108 * benefit_from_CBH(pi * 5)), digit = 1)
  return (score)
}