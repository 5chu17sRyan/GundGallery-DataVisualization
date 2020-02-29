install.packages("shiny")
library(mosaic)
treeData <- read.csv("/Users/wangzhaofang/Desktop/GundProject/GundGallery-DataVisualization/Tree_data.csv")
logBene = log(treeData$Total.Benefits.for.this.year....)
logCBH = log(treeData$CBH.ft.)
model = lm(logBene~logCBH)
summary(model)

benefit_from_CBH <-function(CBH){
  benefit <- -0.23102 + 0.80001 * CBH
  return (benefit)
}

