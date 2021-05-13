dataset <- read.csv("dataset.csv", sep=";")

#Queries:
#Simples: 
# 1 - Procedimientos en pacientes segun la edad 
# 2 - Porcentaje de pacientes con complicaciones (Cuantos tienen complicaciones, y la distribucion de estas entre inmediatas y tardias, de cirugia, y angioplastia)
# 3 - Motivo de ingreso segun la edad
# 4 - Porcentajes de cirugias que son crm y valvulopatia 

# Numero de vasos angioplastiados y vasos exitosos --> otra opcion

#Cruzadas:
# 1 - Relacion entre procedimiento(CRM y ATC) y complicaciones  ---> La rompe
# 2 - Epoc,swan,dialisis,diabetes,obesidad morbida, hipertension pulmonar, 
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
subdatasetComplicaciones <- dataset[which(dataset$COMPLICACIONES.INMEDIATAS == 1 | dataset$COMPLICACIONES.TARDIAS == 1 | dataset$COMPLICACIONES.ANGIOPLASTIA == 1),] 
numNoComp <- as.numeric(nrow(dataset) - nrow(subdatasetComplicaciones))
porcNoComp <- paste(round((numNoComp/nrow(dataset))*100), "%", sep="")
porcComp<- paste(round((nrow(subdatasetComplicaciones)/nrow(dataset))*100), "%", sep="")
perc<- c(porcComp,porcNoComp)
levels<- c("Complicaciones", "No complicaciones")
factor(perc,levels = perc, labels = levels)

#3 MOTIVO DE INGRESO SEGUN LA EDAD

motivoIngreso<- factor(dataset$MOTIVO.DE.INGRESO)
motivoIngreso

table(dataset$MOTIVO.DE.INGRESO,dataset$EDAD)

#4 DISTRIBUCION DE LOS DIFERENTES TIPOS DE CIRUGIAS
subdatasetCRM = dataset[which(dataset$CRM == "true"),]

subdatasetCirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA" & dataset$TIPO.DE.CIRUGIA != "#N/A"),]
tiposDeCirugias<- factor(subdatasetCirugia$TIPO.DE.CIRUGIA)
tablaTiposDeCirugias <- table(tiposDeCirugias)

crmFactor = factor(subdatasetCRM$CRM)
long = length(crmFactor)
crmFactor2 = factor(which(subdatasetCRM$TIPO.DE.CIRUGIA == "CRM PURA"))
long2 = length(crmFactor2)
long2
long2/long

cirugia = factor(which(dataset$PROCEDIMIENTO == "CIRUGIA"))
lon3 = length(cirugia)
long2/lon3
