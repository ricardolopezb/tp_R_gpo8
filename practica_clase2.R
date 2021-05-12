dataset <- read.csv("C:/Users/54112/Desktop/TpR/dataset.csv", sep=";")

library(plotrix)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(hrbrthemes)

#Factores de riesgo: edad mayor, diabetes...

#QUERYS: 
#1 Relacion entre procedimiento y complicaciones tardias e inmediatas (comparar entre estas dos)
#2 Relacion entre valvula con numero de lesiones
#3 Diabetes con extubacion por ahi mejor meter mortalidad o algo de muertes
#4 Epoc,swan,dialisis,diabetes,obesidad morbida, hipertension pulmonar, edad, (todos los factores de riesgo) con motivo de ingreso.(mejor con procedimiento)
#5 swan con circulacion extracorporea
#6 Tiempo de clampeo con numero de lesiones
#descartar 5 y 3, ver si es buena la 6, si aporta info, no pensar tanto en el objetivo a resolver, si no en dar un analisis puro del dataset, poner alguno de la edad con tipo de procedimiento y complicaciones

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



women <- percentageVector[1]
men <- percentageVector[2]

fem
masc
perc
View(femePlus50)
View(masc20to40)
mostFreqCirugia
promedioLesiones

#1
pie3D(c(women,men),main = "Sex Distribution", labels = c(paste(women,"%"),paste(men,"%")),col = c("turquoise","lightblue"),labelcex = 1, explode = 0.1,start = 3 ,theta = 1.2, mar = c(6,6,6,6))

legend(-0.9,-0.8,c("Women","Men"),fill = c("turquoise","lightblue"))

#2
diabPerc <- round((nrow(diab)/nrow(dataset))*100)
notDiabPerc <- 100 - diabPerc

pie3D(c(diabPerc,notDiabPerc),main = "Diabetics Distribution", labels = c(paste(diabPerc,"%"),paste(notDiabPerc,"%")),col = c("red","lightgreen"),labelcex = 1, explode = 0.1,start = 2.9 ,theta = 1.1, mar = c(6,6,6,6))
legend(-1.1,1.15,c("Diabetic","Non Diabetic"),fill = c("red","lightgreen"))

#3 -- Women more than 50 years old
womenMoreThan50 <- dataset[which(dataset$EDAD > 50 & dataset$SEXO == "FEME"), ] 
womenMoreThan50 %>% 
  ggplot(aes(x=EDAD)) + 
  geom_density(fill = "#b19cd9", color = "#e1ecef", alpha = 0.8) + 
  ggtitle("Densidad de Mujeres Mayores de 50")
#Density of male and female age distribution  

ggplot(dataset, aes(x = EDAD, fill = SEXO)) +
  geom_density(alpha = 0.4) +
  theme_ridges() +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top")

#4
diab <- dataset[which(dataset$DIABETES == "1"),] #agregarle transparencia al color de las areas
diab %>% 
  ggplot(aes(x= EDAD, fill = PROCEDIMIENTO)) +
  geom_density(alpha = 0.8) +
  theme_ridges() +
  scale_fill_discrete(name = "Procedimiento", labels = c("Cirugia","Angioplastia","Endovalvula")) +
  theme(legend.position = "top")
  ggtitle("Densidad de diferentes procedimientos en pacientes diabeticos segun la edad")

diabFiltered <- diab[which(diab$TIPO.DE.CIRUGIA != "#N/A"),]
diabFiltered %>% 
  ggplot(aes(x=TIPO.DE.CIRUGIA)) + 
  geom_bar( ) +
  scale_fill_brewer() 
  theme(legend.position="top")


mortalidad <- dataset[which(dataset$Mortalidad != "NO"),]
mortalidad <- dataset[which(dataset$EPOC == "1"),]


#Procedimiento de pacientes con fraccion de eyeccion menor a 50
feyminus50 <- dataset[which(dataset$FEY < 50),]
xVar<- c("CIRUGIA","ANGIOPLASTIA","ENDOVALVULA")
procedimientosFEY <- factor(feyminus50$PROCEDIMIENTO)
table(procedimientosFEY)
freqTable1 <- as.vector(table(procedimientosFEY))
freqAngioFEY <- as.numeric(freqTable1[1])
freqCirugiaFEY <- as.numeric(freqTable1[2])
freqEndovalvFEY <- as.numeric(freqTable1[3])
plot_ly(
  y = c(freqCirugiaFEY,freqAngioFEY,freqEndovalvFEY),
  x = xVar,
  name = "PROCEDIMIENTO DE FEY < 50 ",
  type = "bar",
  marker = list(color = c("#83DC4D","#0C9B21","#7BBB84","#9DFBAB"))) %>%
  layout(title = "Procedimientos de pacientes con fraccion de eyeccion menor a 50")


#Procedimientos de pacientes con edad menor a 55
edadMenorA55 <- dataset[which(dataset$EDAD < 55),]
procedimientosAge55 <- factor(edadMenorA55$PROCEDIMIENTO)
table(procedimientosAge55)
freqTable2 <- as.vector(table(procedimientosAge55))
freqAngio55 <- as.numeric(freqTable2[1])
freqCirugia55 <- as.numeric(freqTable2[2])
freqEndovalv55 <- 0

plot_ly(
  y = c(freqCirugia55,freqAngio55,freqEndovalv55),
  x = xVar,
  name = "Procedimientos de pacientes con edad menor a 55",
  type = "bar",
  marker = list(color = c("E9ADF9","F9ADBD","F9ADE3","F9C3AD"))) %>%
  layout(title = "Procedimientos de pacientes con edad menor a 55")

#Procedimientos de pacientes con edad mayor a 70
edadMayorA70 <- dataset[which(dataset$EDAD > 70),]
procedimientos <- factor(edadMayorA70$PROCEDIMIENTO)
table(procedimientos)
freqTable3 <- as.vector(table(procedimientos))
freqAngio <- as.numeric(freqTable3[1])
freqCirugia <- as.numeric(freqTable3[2])
freqEndovalv <- as.numeric(freqTable3[4])

plot_ly(
  y = c(freqCirugia,freqAngio,freqEndovalv),
  x = xVar,
  name = "Procedimientos de pacientes con edad mayor a 70",
  type = "bar",
  marker = list(color = c("E9ADF9","F9ADBD","F9ADE3","F9C3AD"))) %>%
  layout(title = "Procedimientos de pacientes con edad mayor a 70") 

datasetFilteredPocedimiento <- dataset[which(dataset$PROCEDIMIENTO != "CIRUGIA, ENDOVALVULA"),]
datasetFilteredPocedimiento %>% 
  ggplot(aes(x= EDAD, fill = PROCEDIMIENTO)) +
  geom_density(alpha = 0.8) +
  theme_ridges() +
  scale_fill_discrete(name = "Procedimiento", labels = c("Cirugia","Angioplastia","Endovalvula")) +
  theme(legend.position = "top")+
  ggtitle("Densidad de diferentes procedimientos segun la edad")
  

