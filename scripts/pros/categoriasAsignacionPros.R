## Asignaci?n de categor?as a pros


library(dplyr)

## reading local files
## Fuente: https://docs.google.com/a/marbventures.com/spreadsheets/d/1bV5lk96CAp1cmcH1CKtbxwt-cDaEHOYEJSsfQfUKk6Y/edit?usp=sheets_home&ths=true
pros <- read.csv("~/data/pros/ACTIVEPROS.csv")
## categor?as obtenidas del siguiente dashboard 
## https://app.periscopedata.com/app/iguanafix/181941/pros-mx-analysis/Categor%C3%ADas-pros?widget=2520482&udv=0&redesign=1
cat <- read.csv("~/data/pros/Categoriaspros_2017-10-25_1341.csv")

## seleccionando solo los campos necesarios
cat.u <- unique(cat[,2:3])
prosAsignados <- select(pros,COMPANY_ID:LOCALIDAD) 

## definicion de un dataframe para guardar resultados
asignaciones <- as.data.frame(matrix("NO",
                                     length(prosAsignados$COMPANY_ID),
                                     length(cat.u$name)),stringsAsFactors = FALSE )
names(asignaciones) <- cat.u$name
asignaciones <- mutate(asignaciones,COMPANY_ID=pros$COMPANY_ID)

## Asignaci?n de categor?as
for (i in 1:length(cat$company_id)) {
        nameIndex <- which(colnames(asignaciones)==cat$name[i])
        compIndex <- which(asignaciones$COMPANY_ID==cat$company_id[i])
        asignaciones[compIndex,nameIndex] <- "YES"  
}

## filtrando y guardando datos
pros <- select(pros,COMPANY_ID:LOCALIDAD)
prosCat <- cbind(pros,asignaciones)
prosCat <- prosCat[,1:(length(prosCat)-1)]
write.csv(prosCat,file = "~/data/pros/prosCategorias.csv",row.names = FALSE)
