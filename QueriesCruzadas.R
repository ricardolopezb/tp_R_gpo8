dataset <- read.csv("dataset.csv", sep=";")
library(ggplot2)
library(viridis)
library(hrbrthemes)

# 1) Relacion entre Tiempo de Clampeo y TAS y TAD de ingreso

subdatasetNoAngioplastia = dataset[which(dataset$PROCEDIMIENTO != "ANGIOPLASTIA"),]
subdatasetTiempoClampeo = subdatasetNoAngioplastia[which(subdatasetNoAngioplastia$TIEMPO.DE.CLAMPEO != "#N/A"),]
subdatasetTiempoClampeoTAD = subdatasetTiempoClampeo[which(subdatasetTiempoClampeo$TAD.INGRESO.RCV != "#N/A"),]
subdatasetTiempoClampeoTAS = subdatasetTiempoClampeo[which(subdatasetTiempoClampeo$TAS.INGRESO.RCV != "#N/A"),]


tiempoDeClampeoTad = subdatasetTiempoClampeoTAD$TIEMPO.DE.CLAMPEO
tiempoDeClampeoTas = subdatasetTiempoClampeoTAS$TIEMPO.DE.CLAMPEO
tad = subdatasetTiempoClampeoTAD$TAD.INGRESO.RCV[which(subdatasetTiempoClampeoTAD$TAD.INGRESO.RCV != "#N/A")]
tas = subdatasetTiempoClampeoTAS$TAS.INGRESO.RCV[which(subdatasetTiempoClampeoTAS$TAS.INGRESO.RCV != "#N/A")]
plot(tad,tiempoDeClampeoTad)
modeltad = lm(tiempoDeClampeoTad~tad)
abline(modeltad)


plot(tas,tiempoDeClampeoTas)
modeltas = lm(tiempoDeClampeoTas~tas)
abline(modeltas)


# 2) Número de Lesiones relacionada con Anatomia Coronaria Compleja
  
  #Grafico de barras comparativo, grouped bar chart, entre los que tienen y no ACC en cada numero de lesiones (1-5) 


subsetNoACC <- dataset[which(dataset$ANATOMIA.CORONARIA.COMPLEJA == 0 & dataset$NUMERO.DE.LESIONES != "#N/A"),]
frecNumLesionesNoACC <- c(as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 0)))), as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 1)))), as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 2)))), as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 3)))),  as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 4)))), as.numeric(length(factor(which(subsetNoACC$NUMERO.DE.LESIONES == 5))))) 

subsetACC <- dataset[which(dataset$ANATOMIA.CORONARIA.COMPLEJA == 1 & dataset$NUMERO.DE.LESIONES != "#N/A"),]
frecNumLesionesACC <- c(as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 0)))), as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 1)))), as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 2)))), as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 3)))),  as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 4)))), as.numeric(length(factor(which(subsetACC$NUMERO.DE.LESIONES == 5))))) 


cols1<- c("0 Injuries", "1 Injury", "2 Injuries", "3 Injuries", "4 Injuries", "5 Injuries")
rows1<- c("ACC", "No ACC")

procedACC_data <- data.frame(frecNumLesionesNoACC,frecNumLesionesACC, row.names = cols1)

colnames(procedRisksData)[1]<-"ACC"
colnames(procedRisksData)[2]<-"No ACC"

barplot(t(as.matrix(procedACC_data)),beside=TRUE,main="Injuries according to ACC",xlab="Injuries", ylab="Frequency", col=c("lightblue", "red"),
        legend.text = colnames(procedRisksData),args.legend=list(cex=0.75,x = "top"))


# 3) Distriubucion de procedimientos en funcion de los vasos afectados

subdatasetVasosAfectados = dataset[which(dataset$Resumen.Coronariopatia !="#N/A" & dataset$Resumen.Coronariopatia !="TCI solo"),]
displayVasosProced <- table(subdatasetVasosAfectados$PROCEDIMIENTO,subdatasetVasosAfectados$Resumen.Coronariopatia)
displayVasosProced

subdatasetVasosAfectados
vasosCirugiaAngioplastia <- subdatasetVasosAfectados[which(subdatasetVasosAfectados$PROCEDIMIENTO == "CIRUGIA" | subdatasetVasosAfectados$PROCEDIMIENTO == "ANGIOPLASTIA"),]
unVaso<-vasosCirugiaAngioplastia[which(vasosCirugiaAngioplastia$Resumen.Coronariopatia == "1 vaso"),]
dosVasos<-vasosCirugiaAngioplastia[which(vasosCirugiaAngioplastia$Resumen.Coronariopatia == "2 vasos"),]
tresVasos<-vasosCirugiaAngioplastia[which(vasosCirugiaAngioplastia$Resumen.Coronariopatia == "3 vasos"),]

coronopatia<- c(c("Un vaso", "Un vaso"),c("Dos vasos", "Dos vasos"), c("Tres vasos", "Tres vasos"))
procedimiento<- c("Cirugia", "Angioplastia","Cirugia", "Angioplastia","Cirugia", "Angioplastia")
valores<- c(nrow(unVaso[which(unVaso$PROCEDIMIENTO == "CIRUGIA"),]),nrow(unVaso[which(unVaso$PROCEDIMIENTO == "ANGIOPLASTIA"),]),nrow(dosVasos[which(dosVasos$PROCEDIMIENTO == "CIRUGIA"),]),nrow(dosVasos[which(dosVasos$PROCEDIMIENTO == "ANGIOPLASTIA"),]),nrow(tresVasos[which(tresVasos$PROCEDIMIENTO == "CIRUGIA"),]),nrow(tresVasos[which(tresVasos$PROCEDIMIENTO == "ANGIOPLASTIA"),]))
dataVasosProced <- data.frame(procedimiento,coronopatia,valores)

ggplot(dataVasosProced, aes(fill=procedimiento, y=valores ,x=procedimiento)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Procedimiento segun numero de vasos") +
  facet_wrap(~coronopatia,nrow = 1) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

