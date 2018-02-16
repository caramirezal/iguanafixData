library(dplyr)

## data is obtained from:
## http://www.correosdemexico.gob.mx/lservicios/servicios/CodigoPostal_Exportar.aspx
##
##
## data by estado can be easily obtained from this tool:
## http://www.correosdemexico.gob.mx/lservicios/servicios/
estado <- read.csv("/Users/carlos/Downloads/Querétaro.txt",
                   sep='|',
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   skip = 1)

## convert string to upper characters in ASCII format, with no
## numeric or symbol characters
filterStrings <- function(string){
        filteredString <- gsub(" ","",string)
        filteredString <- gsub("\\(","",filteredString)
        filteredString <- gsub("\\)","",filteredString)
        filteredString <- gsub("<e1>loc","",filteredString)
        filteredString <- iconv(filteredString, to="ASCII//TRANSLIT")
        filteredString <- gsub("[^[:alpha:]]","",filteredString)
        filteredString <- toupper(filteredString)
        filteredString
} 

zoneIds <- function(zones) {
        bag <- character(0)
        for (i in 1:length(zones)) {
                newId <- substring(zones[i],1,2)
                if ( ! newId %in% bag ) {
                        bag[i] <- newId
                } else {
                        for (j in 3:nchar(zones[i])) {
                                newId <- paste(substring(zones[i],1,1),
                                               substring(zones[i],j,j),
                                               sep='')
                                if ( ! newId %in% bag ) {
                                        bag[i] <- newId
                                }
                                if ( newId %in% bag ) break 
                        }
                }
        }
        cat("check no of zones = no of ids: ",
            length(zones)==length(unique(bag)),
            "\n")
        return(bag)
}


municipios <- unique(estado$D_mnpio)
municipios <- filterStrings(municipios)
municipios <- data.frame(municipios,
                         stringsAsFactors = FALSE)
municipios$"id" <- zoneIds(municipios$municipios)


localidades <- estado[!duplicated(estado),]
localidades <- localidades[,c("d_estado",
                         "D_mnpio",
                         "d_asenta")]
localidades <- localidades[!duplicated(localidades),]
localidades <- mutate(localidades,entidad_nombre=filterStrings(d_estado),
                 nombre_municipio=filterStrings(D_mnpio),
                 nombre_colonia=filterStrings(d_asenta))
localidades <- mutate(localidades,filteredColonia=filterStrings(nombre_colonia))

## getting first part of the name
localidades$firstName <- substr(localidades$nombre_colonia,1,3)

## obtaining second part of the name
localidades$secName <- gsub("^LA","",localidades$filteredColonia)
localidades$secName <- gsub("^EL","",localidades$secName)
localidades$secName <- gsub("^SANTO","",localidades$secName)
localidades$secName <- gsub("^SANTA","",localidades$secName)
localidades$secName <- gsub("^SAN","",localidades$secName)

localidades$"region" <- 'NA'
localidades$region <- sapply(1:nrow(localidades), 
                             function(i) which(municipios$municipios==localidades$nombre_municipio[i])) 
localidades$region <- sapply(localidades$region, 
                             function(i) municipios$id[i] )


localidades <- mutate(localidades, region=paste("QT",
                                                region,
                                                sep = ".")) 





localidades <- mutate(localidades, Estado=entidad_nombre,Municipio=nombre_municipio,
                 Colonia=nombre_colonia)

alpnum <- c(as.character(0:9),LETTERS,"(",")")


bag <- NULL
localidades$id <- "--.--.--"
for (i in 1:length(localidades$entidad_nombre)) {
        ## try to assign first and second initial
        ## to .CO part of the ID
        firstInitial <- substr(localidades$firstName[i],1,1)
        secondInitial <- substr(localidades$secName[i],1,1)
        newId <- paste(firstInitial,secondInitial,sep="")
        newId <- paste(localidades$region[i],newId,sep=".")
        allreadyInBag <- newId %in% bag
        if ( ! allreadyInBag ) {
                bag <- c(bag,newId)
                localidades$id[i] <- newId 
                cat(i, newId,"\n")
        }
        if ( allreadyInBag ) {
                for (j in 1:nchar(localidades$secName[i])) {
                        newId <- paste(firstInitial,
                                       substr(localidades$secName[i],j,j),sep="")
                        newId <- paste(localidades$region[i],newId,sep=".")
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                localidades$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }
                }
        }
        ## using any alpnum
        if ( allreadyInBag ) {
                for (j in 1:length(alpnum)) {
                        for (k in 1:length(alpnum)) {
                                newId <- paste(alpnum[j],
                                               alpnum[k],sep="")
                                newId <- paste(localidades$region[i],newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        localidades$id[i] <- newId 
                                        #cat(i, newId,"\n")
                                        break 
                                }
                        }
                        if ( allreadyInBag ) break
                }
        }
        
}

###############################################################################################################


check <- filter(localidades,id=="--.--.--")
nrow(check)
check
length(alpnum)**2
