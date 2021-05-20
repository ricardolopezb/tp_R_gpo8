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

