library(dplyr)



## convert string to upper characters in ASCII format, with no
## numeric or symbol characters
filterStrings <- function(string){
        filteredString <- gsub(" ","",string)
        filteredString <- gsub("\\(","",filteredString)
        filteredString <- gsub("\\)","",filteredString)
        filteredString <- iconv(filteredString, to="ASCII//TRANSLIT")
        filteredString <- toupper(filteredString)
        filteredString <- gsub("[^[:alpha:]]","",filteredString)
        filteredString
} 

localidades <- read.csv("/Users/carlos/Downloads/CATALOGO_DE_COLONIAS_ Y_LOCALIDADES_POR_DTTO_LOCAL_2017.csv",
                        skip=5, header = TRUE)

localidades <- localidades[!duplicated(localidades),]


names(localidades) <- c("no","entidad","entidad_nombre","clave_distrito_electoral",
                        "Cabecera_distrital","seccion","municipio","nombre_municipio",
                        "tipo_colonia","nombre_colonia","codigo_postal")

localidades <- localidades[2:(nrow(localidades)-2),]
localidades <- localidades[,2:(ncol(localidades)-2)]

puebla <- localidades[,c("entidad_nombre",
                         "nombre_municipio",
                         "nombre_colonia")]

names(puebla) <- c("Estado","Municipio","Colonia")

puebla <- mutate(puebla,Estado = as.character(Estado),
                 Municipio=as.character(Municipio),
                 Colonia=as.character(Colonia)) 

puebla <- mutate(puebla,Estado=filterStrings(Estado),
                 Municipio=filterStrings(Municipio),
                 Colonia=filterStrings(Colonia))


getInitials <- function(string){
        ## obtaining second part of the name
        res <- gsub("^LA","",string)
        res <- gsub("^EL","",res)
        res <- gsub("^SANTO","",res)
        res <- gsub("^SANTA","",res)
        res <- gsub("^SAN","",res)
        paste(substr(string,1,1),
              substr(res,1,1),
              sep="")
}

puebla <- mutate(puebla,estadoId=substr(Estado,1,2))
puebla <- mutate(puebla,municipioId=substr(Municipio,1,2))
puebla <- mutate(puebla,coloniaId=getInitials(Colonia))

puebla <- mutate(puebla,preprocessed=paste(estadoId,municipioId,coloniaId,sep=""))
puebla <- mutate(puebla, concatenate=paste(Estado,Municipio,Colonia,sep=""))


bag <- puebla$preprocessed[1]
for (i in 2:nrow(puebla)) {
        allreadyInBag <- puebla$preprocessed[i] %in% bag
        if ( ! allreadyInBag ) {
                bag <- c(bag,puebla$preprocessed[i])
        }
}

refineIds <- function(preprocessedString) {
        for ( r in 1:length(preprocessedString)) {
                for (i in 0:5) {
                        splitStr <- unlist(strsplit(preprocessedString,""))
                        for (j in 1:length(LETTERS)) {
                                splitStr[]
                                
                        }
                }
        }
}

res <- combn(LETTERS,2,simplify = TRUE)
res <- sapply(res, function(x) x)
