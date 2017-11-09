
presupuesto <- readLines("data/sample.txt")

## extracting nombre de cliente
dataExtracted <- c("cliente"=presupuesto[10])
## extracting domicilio
dataExtracted <- c(dataExtracted,"Domicilio-Localidad"=presupuesto[14])
## extracting job_id
dataExtracted <- c(dataExtracted,"job_id"=presupuesto[18])
## extracting fecha
dataExtracted <- c(dataExtracted,"fecha"=gsub("/","-",presupuesto[22]))
## extracting materiales
materiales <- presupuesto[grep("Materiales \\(estimados\\)",presupuesto)+4]
materiales <- gsub(",","",materiales)
dataExtracted <- c(dataExtracted,"Materiales"=materiales)
## extracting mano de obra
manoDeObra <- presupuesto[grep("Mano de obra",presupuesto)+4]
manoDeObra <- gsub(",","",manoDeObra)
dataExtracted <- c(dataExtracted,"Mano_de_obra"=manoDeObra)
## extracting total
total <- presupuesto[grep("Materiales \\(estimados\\)",presupuesto)+8]
total <- gsub(",","",total)
dataExtracted <- c(dataExtracted,"Total"=total)

dataExtracted
