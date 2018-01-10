library(dplyr)

localidades <- read.csv("/Users/carlos/Downloads/CATALOGO_DE_COLONIAS_ Y_LOCALIDADES_POR_DTTO_LOCAL_2017.csv",
                         skip=5, header = TRUE)

names(localidades) <- c("no","entidad","entidad_nombre","clave_distrito_electoral",
                        "Cabecera_distrital","seccion","municipio","nombre_municipio",
                        "tipo_colonia","nombre_colonia","codigo_postal")

localidades <- localidades[2:(nrow(localidades)-2),]
localidades <- localidades[,2:(ncol(localidades)-2)]

localidades.data <- localidades[,c("entidad_nombre",
                                   "nombre_municipio",
                                   "nombre_colonia")]

localidades.data <- mutate(localidades.data,
                           preprocessed=paste(entidad_nombre,
                                              nombre_municipio,
                                              nombre_colonia,sep=""))

localidades.data <- localidades.data[!duplicated(localidades.data),]


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

localidades.data <- mutate(localidades.data,
                           preprocessed=filterStrings(preprocessed))



