dataset <- read.csv("dataset.csv", sep=";")

library(caret)

NH11<-NH11[which(!is.na(NH11$hypev)),]
index <- createDataPartition(NH11$hypev, p=0.70, list=FALSE)

trainSet <- NH11[ index,]
testSet <- NH11[-index,]

hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=trainSet, family="binomial")
summary(hyp.out)

preds <- predict(hyp.out, type = "response",
                 se.fit = TRUE, interval="confidence",
                 newdata = testSet)

threshold<- 0.45

results<-ifelse(preds$fit < threshold,"2 No", "1 Yes")

testValues<- factor(testSet$hypev)
testValues

resultsFactor<- factor(results)
resultsFactor

coincidences<- 0

for(i in 1:length(resultsFactor)){
  
  if(testValues[i] == resultsFactor[i]) coincidences <- coincidences + 1
  
}
accuracy <- coincidences / length(resultsFactor) * 100

accuracy