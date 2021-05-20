dataset <- read.csv("dataset.csv", sep=";")

# 1) Factores de riesgo con procedimientos

riskCirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"),] 
frecRiesgoCirugia<- c(as.numeric(length(factor(which(riskCirugia$EPOC == 1)))), as.numeric(length(factor(which(riskCirugia$DIABETES == 1)))), as.numeric(length(factor(which(riskCirugia$HIPERTENSION.PULMONAR == 1)))), as.numeric(length(factor(which(riskCirugia$OBESIDAD.MORBIDA == 1)))),  as.numeric(length(factor(which(riskCirugia$EDAD >65)))), as.numeric(length(factor(which(riskCirugia$FEY <50))))) 
cols<- c("EPOC", "Diabetes", "Lung Hypertension", "Morbid Obesity", "Age above 65", "FEY less than 50")
rows<- c("Cirugia", "Angioplastia")
riskATC <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]
frecRiesgoATC<- c(as.numeric(length(factor(which(riskATC$EPOC == 1)))), as.numeric(length(factor(which(riskATC$DIABETES == 1)))), as.numeric(length(factor(which(riskATC$HIPERTENSION.PULMONAR == 1)))), as.numeric(length(factor(which(riskATC$OBESIDAD.MORBIDA == 1)))),  as.numeric(length(factor(which(riskATC$EDAD >65)))), as.numeric(length(factor(which(riskATC$FEY <50))))) 

procedRisksData<-data.frame(frecRiesgoCirugia,frecRiesgoATC, row.names = cols)
colnames(procedRisksData)[1]<-"Surgery"
colnames(procedRisksData)[2]<-"Angioplasty"

procedRisksData

barplot(t(as.matrix(procedRisksData)),beside=TRUE,main="Procedure Frequency by Risk Factor",xlab="Risk Factors", ylab="Frequency", col=c("lightblue", "red"),
        legend.text = colnames(procedRisksData),args.legend=list(cex=0.75,x = "top"))


# 2) Relación entre procedimiento y complicaciones

  #