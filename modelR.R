#install.packages("shiny")
# library(mosaic)
# treeData <- read.csv("/Users/wangzhaofang/Desktop/GundProject/GundGallery-DataVisualization/Tree_data.csv")
# logBene = log(treeData$Total.Benefits.for.this.year....)
# logCBH = log(treeData$CBH.ft.)
# model = lm(logBene~logCBH)
# summary(model)

benefit_from_CBH <-function(CBH){
  benefit <- exp(-0.23102 + 0.80001 * log(CBH))
  return (benefit)
}
health_score <- function(benefit){
  score <- round(100 * benefit / (108 * benefit_from_CBH(pi * 5)), digit = 1)
  return (score)
}
