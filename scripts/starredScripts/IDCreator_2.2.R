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

estadoId <- "QT"

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
        ids <- character(length(zones))
        for (i in 1:length(zones)) {
                newId <- substring(zones[i],1,2)
                cat("newid",newId,"\n")
                if ( ! newId %in% bag ) {
                        bag <- c(newId,bag)
                        ids[i] <- newId
                        cat("added to bag",bag[1],"\n")
                } else (
                        for ( j in 1:nchar(zones[i])) {
                                newId <- paste(substring(zones[i],1,1),
                                               substring(zones[i],j,j),
                                               sep="")
                                if ( ! newId %in% bag ) {
                                        bag <- c(newId,bag)
                                        ids[i] <- newId
                                        cat("added to bag",bag[1],"\n")
                                        break
                                }
                                cat("newnewid:",newId,"\n")
                        }
                )
                
        }
                
        cat("check no of zones = no of ids: ",
            length(zones)==length(unique(bag)),
            "\n")
        return(ids)
}

getLocations <- function(estadoFromCSV,estadoId) {
        localidades <- estado[!duplicated(estado),]
        localidades <- localidades[,c("d_estado",
                                      "D_mnpio",
                                      "d_asenta")]
        localidades <- localidades[!duplicated(localidades),]
        localidades <- mutate(localidades,nombre_municipio=filterStrings(D_mnpio))
        
        municipios <- unique(estado$D_mnpio)
        municipios2 <- unique(estado$D_mnpio)
        municipios <- filterStrings(municipios)
        municipios <- data.frame(municipios,
                                 stringsAsFactors = FALSE)
        municipios$"id" <- zoneIds(municipios$municipios)
        municipios$"localidad" <- municipios2
        
        localidades$"region" <- 'NA'
        localidades$region <- sapply(1:nrow(localidades), 
                                     function(i) which(municipios$municipios==localidades$nombre_municipio[i])) 
        localidades$region <- sapply(localidades$region, 
                                     function(i) municipios$id[i] )
        
        localidades <- mutate(localidades, region=paste(estadoId,
                                                        region,
                                                        sep = ".")) 
        
        return(localidades)
}



getallids <- function() {
        alpnum <- c(LETTERS,as.character(0:9))
        ids <- character(0)
        for (i in 1:length(alpnum)) {
                for (j in 1:length(alpnum)) {
                        ids <- c(ids,paste(alpnum[i],alpnum[j],sep=""))
                }
        }
        ids
}

getIds <- function(localidades) {
        localidades$id <- "--.--.--"
        ids <- getallids() 
        uniqueRegions <- unique(localidades$region)
        slide <- filter(localidades,region==uniqueRegions[1])
        slide$id <- ids[1:nrow(slide)]
        slide <- mutate(slide,id=paste(region,id,sep="."))
        result <- slide
        for (i in 2:length(uniqueRegions)) {
                slide <- filter(localidades,region==uniqueRegions[i])
                slide$id <- ids[1:nrow(slide)]
                slide <- mutate(slide,id=paste(region,id,sep=".")) 
                result <- rbind(result,slide)
        }
        if ( nrow(filter(result,id=='--.--.--')) == 0 ) {
                check <- TRUE
        } else { check <- FALSE }
        
        cat('Non unassigned rows check: ',check,"\n")

        result <- result[,c('d_estado','D_mnpio','d_asenta','id')]
        names(result) <- c('Estado','Municipio','Colonia','id')
        result
}


writeIds <- function(estadosFileCSV,localPath,estadoName,estadoId) {
        localidades <- getLocations(estadosFileCSV,estadoId)
        result <- getIds(localidades)
        coloniasFileName <- paste(localPath,
                          estadoName,
                          "Colonias.csv",
                          sep="")
        write.csv(result,
                  coloniasFileName,
                  row.names = FALSE)
        
        municipios <- mutate(result, id=substring(id,1,5))
        municipios <- unique(result[,c("Estado","Municipio","id")])
        municipiosFileName <- paste(localPath,
                                  estadoName,
                                  "Municipios.csv",
                                  sep="")
        write.csv(municipios,
                  municipiosFileName,
                  row.names = FALSE)
}


writeIds(estado,
         "/Users/carlos/Downloads/",
         "Querétaro",
         "QT")






