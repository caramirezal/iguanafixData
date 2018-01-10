library(dplyr)

localidades <- read.csv("/Users/carlos/Downloads/CATALOGO_DE_COLONIAS_ Y_LOCALIDADES_POR_DTTO_LOCAL_2017.csv",
                         skip=5, header = TRUE)

names(localidades) <- c("no","entidad","entidad_nombre","clave_distrito_electoral",
                        "Cabecera_distrital","seccion","municipio","nombre_municipio",
                        "tipo_colonia","nombre_colonia","codigo_postal")

localidades <- localidades[2:(nrow(localidades)-2),]
localidades <- localidades[,2:(ncol(localidades)-2)]

puebla <- localidades[,c("entidad_nombre",
                         "nombre_municipio",
                         "nombre_colonia")]

puebla <- mutate(puebla,filteredColonia=paste(entidad_nombre,
                                              nombre_municipio,
                                              nombre_colonia,sep=""))

puebla <- puebla[!duplicated(puebla),]

## getting first part of the name
puebla$firstName <- substr(puebla$nombre_colonia,1,3)

## obtaining second part of the name
puebla$secName <- gsub("^LA","",puebla$filteredColonia)
puebla$secName <- gsub("^EL","",puebla$secName)
puebla$secName <- gsub("^SANTO","",puebla$secName)
puebla$secName <- gsub("^SANTA","",puebla$secName)
puebla$secName <- gsub("^SAN","",puebla$secName)




## convert string to upper characters in ASCII format, with no
## numeric or symbol characters
filterStrings <- function(string){
        filteredString <- gsub(" ","",string)
        filteredString <- gsub("\\(","",filteredString)
        filteredString <- gsub("\\)","",filteredString)
        filteredString <- iconv(filteredString, to="ASCII//TRANSLIT")
        filteredString <- toupper(filteredString)
        filteredString <- gsub("[^[:alpha:] ]","",filteredString)
        filteredString
} 




