dataset <- read.csv("C:/Users/54112/Desktop/TpR/dataset.csv", sep=";")

#Queries:
#Simples: 
# 1 - Procedimientos en pacientes segun la edad 
# 2 - Porcentaje de pacientes con complicaciones (Cuantos tienen complicaciones, y la distribucion de estas entre inmediatas y tardias, de cirugia, y angioplastia)
# 3 - Motivo de ingreso segun la edad
# 4 - Porcentajes de cirugias que son crm 

# Numero de vasos angioplastiados y vasos exitosos --> otra opcion

#Cruzadas:
# 1 - Relacion entre procedimiento(CRM y ATC) y complicaciones  ---> La rompe
# 2 - Epoc,swan,dialisis,diabetes,obesidad morbida, hipertension pulmonar, 
# edad, (todos los factores de riesgo) con procedimiento.
# 3 - Distriubucion de procedimientos en funcion de los vasos afectados
# 4 - 
# 5 - 




#Analisis para checkear a que procedimiento corresponden las complicaciones.
complicaciones <- dataset[which(dataset$COMPLICACIONES.INMEDIATAS == 1 | dataset$COMPLICACIONES.TARDIAS == 1),] 
procedimientos<-  factor(complicaciones$PROCEDIMIENTO)
table(procedimientos)
as.numeric(table(procedimientos)) == nrow(complicaciones)

complicacionesAngio <- dataset[which(dataset$COMPLICACIONES.ANGIOPLASTIA == 1),]
complicacionesAngioProced <- factor(complicacionesAngio$PROCEDIMIENTO)
table(complicacionesAngioProced)

#Analizar el procedimiento de los pacientes con 3 vasos (deberia ser miti miti) y ver numero de complicaciones segun procedimiento.

motivoIngreso<- factor(dataset$MOTIVO.DE.INGRESO)


