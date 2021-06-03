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
groupColors <- c("#00ff00", "#ff0000", "#0000ff")
datasetSinNA %>%
  ggplot(aes(x= EDAD, fill = PROCEDIMIENTO)) +
  geom_density(alpha = 0.5, fillcolor="#00ff00") +
  theme_ridges() +
  scale_fill_discrete(name = "Procedure:", labels = c("Surgery","Angioplasty","Mitral Valve Rep.")) +
  theme(legend.position = "top")+
  labs(title="Density of different procedures according to Age",x="Age", y = "Density")
ggtitle("")

#Cambie la opacidad
#Cambie las labels






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

pacientesCRM <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"),]

totalDeCirugia = nrow(pacientesCRM)

cantCompCRM <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 | pacientesCRM$COMPLICACIONES.TARDIAS == 1),])

cantCompCRM_I <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 & pacientesCRM$COMPLICACIONES.TARDIAS == 0 ),])
cantCompCRM_T <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 0 & pacientesCRM$COMPLICACIONES.TARDIAS == 1),])
cantCompCRM_IyT<- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 & pacientesCRM$COMPLICACIONES.TARDIAS == 1),])

cantidadTotal = cantCompCRM_I + cantCompCRM_IyT + cantCompCRM_T
porComplicaciones = cantidadTotal/totalDeCirugia;
sinComplicaciones = totalDeCirugia - cantidadTotal

tablaCRM = cbind(c(cantidadTotal),c(sinComplicaciones))
tablaCRM
prop.table(tablaCRM)
A <- matrix(nrow=1,ncol=2, c(cantidadTotal,sinComplicaciones), byrow=TRUE)

porcNoCompCrm <- paste(round((as.numeric(A[1,2]/161)*100)), "%", sep="")
porcCompCrm <- paste(round((as.numeric(A[1,1]/161)*100)), "%", sep="")



RegionLabels<-c(porcCompCrm,porcNoCompCrm)
pie(tablaCRM, RegionLabels, col = rainbow(length(table(tablaCRM))),main = "Complicaciones en pacientes de Cirugía")
legendLabels = c("Complicaciones","Sin Complicaciones")
legend("topright", legendLabels, cex = 0.8, fill = rainbow(length(table(tablaCRM))))

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
f <- as.data.frame(table(x))

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
  geom_bar( width = 0.85, color = "darkblue", fill = "lightblue") +
  scale_fill_brewer() +
  theme(legend.position="top")+
  ylab("Tipo de Cirugia") 




# 5) Distribucion de edad y sexo en el dataset
x <- cut2(dataset$EDAD, seq(10,90,5))

yes <- dataset$SEXO
displayIngresoSegunEdad<- table(dataset$SEXO,dataset$EDAD)
displayIngresoSegunEdad
textIntervals <- as.character(x)
f <- as.data.frame(table(x))

df <- data.frame(textIntervals, yes)

counts <- ddply(df, .(df$textIntervals, df$yes), nrow)
names(counts) <- c("ageInterval", "Sexo", "Freq")
counts

## pyramid charts are two barcharts with axes flipped

counts$Freq <- ifelse(counts$Sexo == "MASC", -1*counts$Freq, counts$Freq)

pyramid <- ggplot(counts, aes(x = counts$ageInterval, y = counts$Freq, fill = counts$Sexo)) + 
  geom_bar(data = subset(counts, counts$Sexo == "Female"), stat = "identity") +
  geom_bar(data = subset(counts, counts$Sexo == "Male"), stat = "identity") +
  scale_y_continuous(breaks = seq(-60, 60, 10), 
                     labels = paste0(as.character(c(seq(60, 0, -10), seq(10, 60, 10))),'')) + 
  coord_flip() + 
  ggtitle("Age distribution by sex") +
  xlab("Age") + ylab("Number Of Patients")+
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male"))+
  theme_bw()

pyramid

