#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='schultzryan', token='65FAB75DD80238942F735D08F86B08A4', secret='qSkCRlxEtaonKPUS1p6RmZxb8RzVgRsj2bXqnOb+')
#library(rsconnect)

benefit_from_CBH <-function(CBH){
  benefit <- exp(-0.23102 + 0.80001 * log(CBH))
  return (benefit)
}

health_score <- function(benefit){
  score <- round(100 * benefit / (108 * benefit_from_CBH(pi * 5)), digit = 1)
  return (score)
}