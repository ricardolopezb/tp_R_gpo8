dataset <- read.csv("dataset.csv", sep=";")
library(plotrix)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(hrbrthemes)
library(Hmisc)
library(plyr)

# 1) Procedimientos en pacientes segun la edad

datasetSinNA = dataset[which(dataset$PROCEDIMIENTO != "CIRUGIA, ENDOVALVULA"),]

datasetSinNA %>%
  ggplot(aes(x= EDAD, fill = PROCEDIMIENTO)) +
  geom_density(alpha = 0.8) +
  theme_ridges() +
  scale_fill_discrete(name = "Procedimiento", labels = c("Cirugia","Angioplastia","Endovalvula")) +
  theme(legend.position = "top")
ggtitle("Densidad de diferentes procedimientos en pacientes segun la edad")


# 2) Porcentaje de personas con complicaciones

pacientesATC <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]
tablaAtc = table(pacientesATC$COMPLICACIONES.ANGIOPLASTIA)

prop.table(tablaAtc)

porcentageNoCompAng <- paste(round((tablaAtc[1]/217)*100), "%", sep="")
porcentageCompAng <- paste(round((tablaAtc[2]/217)*100), "%", sep="")



RegionLabels<-c(porcentageNoCompAng,porcentageCompAng)
pie(tablaAtc, RegionLabels, col = rainbow(length(table(tablaAtc))),main = "Complicaciones en pacientes de Angioplastia")
legendLabels = c("Sin Complicaciones","Complicaciones")
legend("topright", legendLabels, cex = 0.8, fill = rainbow(length(table(tablaAtc))))

# Falta el resto de los procedimientos


# 3) Motivos de ingreso segun la edad

displayIngresoSegunEdad<- table(dataset$MOTIVO.DE.INGRESO,dataset$EDAD)
motivoIngreso<- factor(dataset$MOTIVO.DE.INGRESO)
motivoIngreso


x <- cut2(dataset$EDAD, seq(10,90,15))

yes <- dataset$MOTIVO.DE.INGRESO
displayIngresoSegunEdad<- table(dataset$MOTIVO.DE.INGRESO,dataset$EDAD)
displayIngresoSegunEdad
textIntervals <- as.character(x)
as.data.frame(table(x))

df <- data.frame(textIntervals, yes)

counts <- ddply(df, .(df$textIntervals, df$yes), nrow)
names(counts) <- c("ageInterval", "Motivo", "Freq")
counts

plot <-ggplot(counts, aes(fill=counts$ageInterval, y=counts$Freq, x=counts$Motivo)) + 
  geom_bar(position="stack", stat="identity")

plot


# 4) Distribucion de los diferentes tipos de cirugias
  
subdatasetCirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA" & dataset$TIPO.DE.CIRUGIA != "#N/A"),]
tiposDeCirugias<- factor(subdatasetCirugia$TIPO.DE.CIRUGIA)
tablaTiposDeCirugias <- table(tiposDeCirugias)
tablaTiposDeCirugias
tiposDeCirugias

subdatasetCRM = dataset[which(dataset$CRM == "true"),]
numCRM = as.numeric(nrow(subdatasetCRM))
numCRMPura = as.numeric(length(factor(which(subdatasetCRM$TIPO.DE.CIRUGIA == "CRM PURA"))))
percentage<- paste(round((numCRMPura/numCRM)*100), "%", sep="") #Porcentaje de CRMS que son CRM puras
percentage
numCirugia = as.numeric(nrow(subdatasetCirugia))
percentage2<- paste(round((numCRM/numCirugia)*100), "%", sep="") #Porcentaje de Cirugias que son CRM
percentage2

subdatasetCirugia %>% 
  ggplot(aes(y=TIPO.DE.CIRUGIA), position=position_dodge()) + 
  geom_bar( width = 0.85, color = "darkblue", fill = "blue") +
  scale_fill_brewer() +
  theme(legend.position="top")+
  ylab("Tipo de Cirugia") 




# 5) Distribucion de edad y sexo en el dataset

  # Grafico de piramide poblacional
