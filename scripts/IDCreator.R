
## script to create ids in the following format:
## "ST.MU.CO" 
## ST - Estado
## MU - Municipio
## CO - Colonia

library(dplyr)

puebla <- read.csv("/Users/carlos/Documents/GitHub/iguanafixData/data/DataGeograficaPuebla.csv",stringsAsFactors = FALSE)

## removing duplicated rows 
puebla <- puebla[!duplicated(puebla),]


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


puebla$filteredColonia <- filterStrings(puebla$Colonia)

## getting first part of the name
puebla$firstName <- substr(puebla$filteredColonia,1,3)

## obtaining second part of the name
puebla$secName <- gsub("^LA","",puebla$filteredColonia)
puebla$secName <- gsub("^EL","",puebla$secName)
puebla$secName <- gsub("^SANTO","",puebla$secName)
puebla$secName <- gsub("^SANTA","",puebla$secName)
puebla$secName <- gsub("^SAN","",puebla$secName)

## getting the following 
puebla$region <- paste(substr(puebla$Estado,1,2),
                       substr(puebla$Municipio,1,2),
                       sep=".")
puebla$region <- toupper(puebla$region)

bag <- NULL
puebla$id <- "--.--.--"
for (i in 1:length(puebla$Estado)) {
        ## try to assign first and second initial
        ## to .CO part of the ID
        firstInitial <- substr(puebla$firstName[i],1,1)
        secondInitial <- substr(puebla$secName[i],1,1)
        newId <- paste(firstInitial,secondInitial,sep="")
        newId <- paste(puebla$region[i],newId,sep=".")
        allreadyInBag <- newId %in% bag
        if ( ! allreadyInBag ) {
                bag <- c(bag,newId)
                puebla$id[i] <- newId 
                cat(i, newId,"\n")
        }
        if ( allreadyInBag ) {
                for (j in 1:nchar(puebla$secName[i])) {
                        newId <- paste(firstInitial,
                                       substr(puebla$secName[i],j,j),sep="")
                        newId <- paste(puebla$region[i],newId,sep=".")
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }
                }
        }
        ## try to assign first initial and any letter
        if ( allreadyInBag ) {
                for (j in 1:length(LETTERS)) {
                        newId <- paste(firstInitial,
                                       LETTERS[j],sep="")
                        newId <- paste(puebla$region[i],newId,sep=".")
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }
                }
        }
        ## using any first letter and the second initial
        if ( allreadyInBag ) {
                for (j in 1:length(LETTERS)) {
                        newId <- paste(LETTERS[j],
                                       secondInitial,sep="")
                        newId <- paste(puebla$region[i],newId,sep=".")
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }
                }
        }
        ## using any letters
        if ( allreadyInBag ) {
                for (j in 1:length(LETTERS)) {
                        for (k in 1:length(LETTERS)) {
                                newId <- paste(LETTERS[j],
                                               LETTERS[k],sep="")
                                newId <- paste(puebla$region[i],newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                        if ( allreadyInBag ) break
                }
        }
        ## using the third letter for municipio
        if ( allreadyInBag ) {
                region <- paste(substr(puebla$region[i],1,4),
                               toupper(substr(puebla$Municipio[i],3,3)),
                               sep="")
                firstInitial <- substr(puebla$firstName[i],1,1)
                secondInitial <- substr(puebla$secName[i],1,1)
                newId <- paste(firstInitial,secondInitial,sep="")
                newId <- paste(region,newId,sep=".")
                allreadyInBag <- newId %in% bag
                if ( ! allreadyInBag ) {
                        bag <- c(bag,newId)
                        puebla$id[i] <- newId 
                        cat(i, newId,"\n")
                }
                ## using first initial and any letter in the 
                ## municipio second name
                if ( allreadyInBag ) {
                        for (j in 1:nchar(puebla$secName[i])) {
                                newId <- paste(firstInitial,
                                               substr(puebla$secName[i],j,j),sep="")
                                newId <- paste(region,newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using first initial of municipio
                ## and any letter
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                newId <- paste(firstInitial,
                                               LETTERS[j],sep="")
                                newId <- paste(region,newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using initial of the second name 
                ## and any letter
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                newId <- paste(LETTERS[j],
                                               secondInitial,sep="")
                                newId <- paste(region,newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using any letter
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                for (k in 1:length(LETTERS)) {
                                        newId <- paste(LETTERS[j],
                                                       LETTERS[k],sep="")
                                        newId <- paste(region,newId,sep=".")
                                        allreadyInBag <- newId %in% bag
                                        if ( ! allreadyInBag ) {
                                                bag <- c(bag,newId)
                                                puebla$id[i] <- newId 
                                                cat(i, newId,"\n")
                                                break 
                                        }
                                }
                                if (  allreadyInBag ) break
                        }
                }
                ## using PB.PB for state and municipio
                ## and initial of first and second name
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:nchar(puebla$secName[i])) {
                                newId <- paste(firstInitial,
                                               substr(puebla$secName[i],j,j),sep="")
                                newId <- paste("PB.PB",newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using any letter after PB.PB
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                for (k in 1:length(LETTERS)) {
                                        newId <- paste(LETTERS[j],
                                                       LETTERS[k],sep="")
                                        newId <- paste("PB.PB",newId,sep=".")
                                        allreadyInBag <- newId %in% bag
                                        if ( ! allreadyInBag ) {
                                                bag <- c(bag,newId)
                                                puebla$id[i] <- newId 
                                                cat(i, newId,"\n")
                                                break 
                                        }
                                }
                                if (  ! allreadyInBag ) break
                        }
                }

        }
}

pueblaIds <- puebla[,c("Estado","Municipio","Colonia","id")]
write.csv(pueblaIds,file = "PUEBLAIDS.csv")

##################################################################################################################################

## definition of the ids para estados y municipios
##


## reading data
ids <- read.csv("pueblaIDs.csv")
ids <- select(ids, Estado:Municipio)
idsRegion <- unique(ids)



## getting Estado part of the name
idsRegion <- mutate(idsRegion,EstadoFormatted=filterStrings(Estado) )

## getting municipio
idsRegion <- mutate(idsRegion,MunicipioFormatted=filterStrings(Municipio) )



## id assignment
bag <- NULL
idsRegion$"id" <- rep("--.--",length(idsRegion$Estado))
for (i in 1:length(idsRegion$Estado)) {
        ## try to assign first and second initial
        ## to .CO part of the ID
        estadoID <- substr(idsRegion$EstadoFormatted[i],1,2)
        municipioID <- substr(idsRegion$MunicipioFormatted[i],1,2)
        newId <- paste(estadoID,municipioID,sep=".")
        allreadyInBag <- newId %in% bag
        if ( ! allreadyInBag ) {
                bag <- c(bag,newId)
                idsRegion$id[i] <- newId 
                cat(i, newId,"\n")
        }
        if ( allreadyInBag ) {
                for (j in 1:nchar(idsRegion$MunicipioFormatted[i])) {
                        newId <- paste(substr(idsRegion$MunicipioFormatted[i],1,1),
                                       substr(idsRegion$MunicipioFormatted[i],j,j),sep="")
                        newId <- paste("PU",newId,sep=".")
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                idsRegion$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }

                }
        }
        if ( allreadyInBag ) {
                for (j in 1:nchar(idsRegion$MunicipioFormatted[i])) {
                        for (k in 1:nchar(idsRegion$MunicipioFormatted[i])) {
                                newId <- paste(substr(idsRegion$MunicipioFormatted[i],j,j),
                                               substr(idsRegion$MunicipioFormatted[i],k,k),sep="")
                                newId <- paste("PU",newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        idsRegion$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                        if (  ! allreadyInBag ) break
                }
        }
}


## writting results
ids <- select(idsRegion, -(EstadoFormatted:MunicipioFormatted))
write.csv(ids,file = "idsRegion.csv",row.names = FALSE)
