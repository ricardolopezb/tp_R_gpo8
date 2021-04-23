dataset <- read.csv("C:/Users/Ricardo/Desktop/dataset.csv", sep=";")



sexo <- factor(dataset$SEXO)
a <- table(sexo)
porcentajeHyM <- prop.table(a)
porcentajeHyM <- porcentajeHyM*100
porcentajeHyM <- round(porcentajeHyM[c(1,2)], 2)
percentageVector <- as.numeric(porcentajeHyM)

fem <-  paste("Fem - ", percentageVector[1], "%", sep="")
masc <- paste("Masc - ", percentageVector[2], "%", sep="")



diab <- dataset[which(dataset$DIABETES == "1"),]
perc <- paste(round((nrow(diab)/nrow(dataset))*100), "%", sep="")



femePlus50 <- subset(dataset, SEXO == "FEME" & EDAD > 50)
masc20to40 <- subset(dataset, SEXO == "MASC" & EDAD >20 & EDAD < 40)



cirugias <- factor(diab$TIPO.DE.CIRUGIA[which(diab$TIPO.DE.CIRUGIA != "#N/A")])
var <- table(cirugias)
mostFreqCirugia <- names(var[which.max(var)])



mas60 <- subset(dataset, EDAD > 60)
numLes <- as.numeric(factor(mas60$NUMERO.DE.LESIONES))
promedioLesiones <- round(mean(numLes))



fem
masc
perc
View(femePlus50)
View(masc20to40)
mostFreqCirugia
promedioLesiones