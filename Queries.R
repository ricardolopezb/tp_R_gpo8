dataset <- read.csv("dataset.csv", sep=";")

#Queries:
#Simples: 
# 1 - Procedimientos en pacientes segun la edad 
# 2 - Porcentaje de pacientes con complicaciones (Cuantos tienen complicaciones, y la distribucion de estas entre inmediatas y tardias, de cirugia, y angioplastia)
# 3 - Motivo de ingreso segun la edad
# 4 - Distribucion de tipos de cirugias

# Numero de vasos angioplastiados y vasos exitosos --> otra opcion

#Cruzadas:
# 1 - Relacion entre procedimiento(CRM y ATC) y complicaciones  ---> La rompe
# 2 - Epoc ,dialisis, diabetes, obesidad morbida, hipertension pulmonar, 
# edad, (todos los factores de riesgo) con procedimiento.
# 3 - Distriubucion de procedimientos en funcion de los vasos afectados
# 4 - Relacion entre Tiempo de Clampeo con la presion inicial con la que ingresa, TAS y TAD
# 5 - Numero de Lesiones relacionada con Anatomia Coronaria Compleja


#Analisis para checkear a que procedimiento corresponden las complicaciones.
complicaciones <- dataset[which(dataset$COMPLICACIONES.INMEDIATAS == 1 | dataset$COMPLICACIONES.TARDIAS == 1),] 
procedimientos<-  factor(complicaciones$PROCEDIMIENTO)
table(procedimientos)
as.numeric(table(procedimientos)) == nrow(complicaciones)

complicacionesAngio <- dataset[which(dataset$COMPLICACIONES.ANGIOPLASTIA == 1),]
complicacionesAngioProced <- factor(complicacionesAngio$PROCEDIMIENTO)
table(complicacionesAngioProced)

#Analizar el procedimiento de los pacientes con 3 vasos (deberia ser mitad CRM y mitad ATC) y ver numero de complicaciones segun procedimiento.

motivoIngreso <- factor(dataset$MOTIVO.DE.INGRESO)

#1 PROCEDIMIENTO EN PACIENTES SEGUN LA EDAD

procedimientoSegunEdad <- table(dataset$PROCEDIMIENTO,dataset$EDAD)
procedimientoSegunEdad

#2 PORCENTAJE DE PACIENTES CON COMPLICACIONES Y DISTRIBUCION DE LAS DIFERENTES COMPLICACIONES 

ComplicacionesTardias <- factor(dataset$COMPLICACIONES.TARDIAS, labels = c("No", "Si"))
displayCompTardias <- prop.table(table(ComplicacionesTardias))

ComplicacionesInmediatas <- factor(dataset$COMPLICACIONES.INMEDIATAS, labels = c("No", "Si"))
displayCompInmediatas<- prop.table(table(ComplicacionesInmediatas))

ComplicacionesAngioplastia <- factor(dataset$COMPLICACIONES.ANGIOPLASTIA, labels = c("No", "Si"))
displayCompAngio<- prop.table(table(ComplicacionesAngioplastia))

dataset_Complicaciones <- dataset[which(dataset$COMPLICACIONES.INMEDIATAS == 1 | dataset$COMPLICACIONES.TARDIAS == 1 | dataset$COMPLICACIONES.ANGIOPLASTIA == 1),] 
numNoComp <- as.numeric(nrow(dataset) - nrow(dataset_Complicaciones))
porcNoComp <- paste(round((numNoComp/nrow(dataset))*100), "%", sep="")
porcComp<- paste(round((nrow(dataset_Complicaciones)/nrow(dataset))*100), "%", sep="")

displayCompTardias
displayCompInmediatas
displayCompAngio

#Porcentaje de pacientes sin complicaciones y con complicaciones
porcNoComp
porcComp

#3 MOTIVO DE INGRESO SEGUN LA EDAD

motivoIngreso<- factor(dataset$MOTIVO.DE.INGRESO)
motivoIngreso

displayIngresoSegunEdad<- table(dataset$MOTIVO.DE.INGRESO,dataset$EDAD)

#4 DISTRIBUCION DE LOS DIFERENTES TIPOS DE CIRUGIAS

subdatasetCirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA" & dataset$TIPO.DE.CIRUGIA != "#N/A"),]
tiposDeCirugias<- factor(subdatasetCirugia$TIPO.DE.CIRUGIA)
tablaTiposDeCirugias <- table(tiposDeCirugias)
tablaTiposDeCirugias


subdatasetCRM = dataset[which(dataset$CRM == "true"),]
numCRM = as.numeric(nrow(subdatasetCRM))
numCRMPura = as.numeric(length(factor(which(subdatasetCRM$TIPO.DE.CIRUGIA == "CRM PURA"))))
percentage<- paste(round((numCRMPura/numCRM)*100), "%", sep="") #Porcentaje de CRMS que son CRM puras

subdatasetCirugia = dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"),]
numCirugia = as.numeric(nrow(subdatasetCirugia))
percentage<- paste(round((numCRM/numCirugia)*100), "%", sep="") #Porcentaje de Cirugias que son CRM

#CRUZADAS:

#1 Relación entre procedimiento (CRM y ATC) y complicaciones
pacientesCRM <- dataset[which(dataset$CRM == "true"),]
pacientesATC <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]

cantCompATC <- nrow(pacientesATC[which(pacientesATC$COMPLICACIONES.ANGIOPLASTIA == 1),])
cantCompCRM <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 | pacientesCRM$COMPLICACIONES.TARDIAS == 1),])

cantCompCRM_I <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 & pacientesCRM$COMPLICACIONES.TARDIAS == 0 ),])
cantCompCRM_T <- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 0 & pacientesCRM$COMPLICACIONES.TARDIAS == 1),])
cantCompCRM_IyT<- nrow(pacientesCRM[which(pacientesCRM$COMPLICACIONES.INMEDIATAS == 1 & pacientesCRM$COMPLICACIONES.TARDIAS == 1),])
percCompATC <- (cantCompATC/nrow(pacientesATC))*100
percCompCRM <- (cantCompCRM/nrow(pacientesCRM))*100

percCompCRM_I <- (cantCompCRM_I/cantCompCRM)*100
percCompCRM_T <- (cantCompCRM_T/cantCompCRM)*100
percCompCRM_IyT <-(cantCompCRM_IyT/cantCompCRM)*100


porcentajesDeComplicaciones <- c(paste(round(percCompATC), "%", sep=""),paste(round(percCompCRM), "%", sep=""))
names <- c("Comp ATC", "Comp CRM")
display <- data.frame(porcentajesDeComplicaciones, row.names=names)
colnames(display)[1]<-"Porcentaje de Complicaciones"

porcCRM_Comps <- c(paste(round(percCompCRM_I), "%", sep=""), paste(round(percCompCRM_T), "%", sep=""),paste(round(percCompCRM_IyT), "%", sep=""))
names2 <- c("Comp. Inmediatas", "Comp. Tardias", "Comp. tardias e inmediatas")
displayCRM_Comp <- data.frame(porcCRM_Comps, row.names=names2)
colnames(displayCRM_Comp)[1]<-"Porcentaje de Tipo de Comp."


display
displayCRM_Comp

#2 Diferentes factores de riesgo en cada procedimiento

#Factores de riesgo: Dialisis, diabetes, obesidad morbida, FEY< 50, edad>65, hipertension pulmonar

riskCirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"),] 
frecRiesgoCirugia<- c(as.numeric(length(factor(which(riskCirugia$EPOC == 1)))), as.numeric(length(factor(which(riskCirugia$DIABETES == 1)))), as.numeric(length(factor(which(riskCirugia$HIPERTENSION.PULMONAR == 1)))), as.numeric(length(factor(which(riskCirugia$OBESIDAD.MORBIDA == 1)))),  as.numeric(length(factor(which(riskCirugia$EDAD >65)))), as.numeric(length(factor(which(riskCirugia$FEY <50))))) 
cols<- c("EPOC", "Diabetes", "Hipertension Pulmonar", "Obesidad Morbida", "Edad mayor a 65", "FEY menor a 50")
rows<- c("Cirugia", "Angioplastia")
riskATC <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]
frecRiesgoATC<- c(as.numeric(length(factor(which(riskATC$EPOC == 1)))), as.numeric(length(factor(which(riskATC$DIABETES == 1)))), as.numeric(length(factor(which(riskATC$HIPERTENSION.PULMONAR == 1)))), as.numeric(length(factor(which(riskATC$OBESIDAD.MORBIDA == 1)))),  as.numeric(length(factor(which(riskATC$EDAD >65)))), as.numeric(length(factor(which(riskATC$FEY <50))))) 

procedRisksData<-data.frame(frecRiesgoCirugia,frecRiesgoATC, row.names = cols)
colnames(procedRisksData)[1]<-"Cirugia"
colnames(procedRisksData)[2]<-"Angioplastia"

procedRisksData

#3 Distriubucion de procedimientos en funcion de los vasos afectados
subdatasetVasosAfectados = dataset[which(dataset$Resumen.Coronariopatia !="#N/A" & dataset$Resumen.Coronariopatia !="TCI solo"),]
displayVasosProced <- table(subdatasetVasosAfectados$PROCEDIMIENTO,subdatasetVasosAfectados$Resumen.Coronariopatia)

displayVasosProced

#4 Relacion entre Tiempo de Clampeo con la presion inicial con la que ingresa (TAS y TAD)
subdatasetNoAngioplastia = dataset[which(dataset$PROCEDIMIENTO != "ANGIOPLASTIA"),]

subdatasetTiempoBomba = subdatasetNoAngioplastia[which(subdatasetNoAngioplastia$TIEMPO.DE.BOMBA != "#N/A"),]
subdatasetTiempoBombaTAD = subdatasetTiempoBomba[which(subdatasetTiempoBomba$TAD.INGRESO.RCV != "#N/A"),]
TADxbomba = table(subdatasetTiempoBombaTAD$TAD.INGRESO.RCV,subdatasetTiempoBombaTAD$TIEMPO.DE.BOMBA)


subdatasetTiempoClampeo = subdatasetNoAngioplastia[which(subdatasetNoAngioplastia$TIEMPO.DE.CLAMPEO != "#N/A"),]
subdatasetTiempoClampeoTAD = subdatasetTiempoClampeo[which(subdatasetTiempoClampeo$TAD.INGRESO.RCV != "#N/A"),]
TADxclampeo = table(subdatasetTiempoClampeoTAD$TAD.INGRESO.RCV,subdatasetTiempoClampeoTAD$TIEMPO.DE.CLAMPEO)


subdatasetTiempoBombaTAS = subdatasetTiempoBomba[which(subdatasetTiempoBomba$TAS.INGRESO.RCV != "#N/A"),]
TASxbomba = table(subdatasetTiempoBombaTAS$TAS.INGRESO.RCV,subdatasetTiempoBombaTAS$TIEMPO.DE.BOMBA)

subdatasetTiempoClampeoTAS = subdatasetTiempoClampeo[which(subdatasetTiempoClampeo$TAS.INGRESO.RCV != "#N/A"),]
TASxclampeo = table(subdatasetTiempoClampeoTAS$TAS.INGRESO.RCV,subdatasetTiempoClampeoTAS$TIEMPO.DE.CLAMPEO)

TADxclampeo
TASxclampeo

#5 Numero de Lesiones relacionadas con Anatomia Coronaria Compleja

subdatasetAnatomiaCompleja = dataset[which(dataset$ANATOMIA.CORONARIA.COMPLEJA>0 & dataset$NUMERO.DE.LESIONES !="#N/A"),]
anatomiaCoronariaCompleja <- factor(subdatasetAnatomiaCompleja$ANATOMIA.CORONARIA.COMPLEJA,levels=c("1"),labels=c("Frecuencia"))
numeroDeLesiones = factor(subdatasetAnatomiaCompleja$NUMERO.DE.LESIONES)
lesionesXanatomia = table(anatomiaCoronariaCompleja,numeroDeLesiones)
lesionesXanatomia

#TABLAS:

#1 Procedimiento en pacientes segun la edad
procedimientoSegunEdad

#2 Porcentaje de pacientes con complicaciones y distribucion de las complicaciones
displayCompTardias
displayCompInmediatas
displayCompAngio

porcNoComp
porcComp

#3 Motivo de Ingreso segun la edad
displayIngresoSegunEdad

#4 Distribucion de tipos de cirugias
tablaTiposDeCirugias

#5 Diferentes factores de riesgo segun el procedimiento.
procedRisksData

#6 Relación entre Tiempo de Clampeo con la presión inicial con la que ingresa (TAS y TAD)

TADxclampeo
TASxclampeo

#7 Número de Lesiones relacionada con Anatomía Coronaria Compleja
lesionesXanatomia

#8 Relación entre procedimiento (CRM y ATC) y complicaciones
display
displayCRM_Comp

#9 Distribución de procedimientos en función de los vasos afectados
displayVasosProced




