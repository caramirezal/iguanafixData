library(dplyr)



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

localidades <- read.csv("~/Downloads/Tlaxcala.txt",
                        sep = "|",
                        skip = 1)

localidades <- localidades[!duplicated(localidades),]

#names(localidades) <- c("no","entidad","entidad_nombre","clave_distrito_electoral",
#                        "Cabecera_distrital","seccion","municipio","nombre_municipio",
#                        "tipo_colonia","nombre_colonia","codigo_postal")

#localidades <- localidades[2:(nrow(localidades)-2),]
#localidades <- localidades[,2:(ncol(localidades)-2)]

puebla <- localidades[,c("d_estado",
                         "D_mnpio",
                         "d_asenta")]

names(puebla) <- c("entidad_nombre","nombre_municipio","nombre_colonia")

puebla <- puebla[!duplicated(puebla),]

puebla <- mutate(puebla,filteredColonia=filterStrings(nombre_colonia))



## getting first part of the name
puebla$firstName <- substr(puebla$nombre_colonia,1,3)

## obtaining second part of the name
puebla$secName <- gsub("^LA","",puebla$filteredColonia)
puebla$secName <- gsub("^EL","",puebla$secName)
puebla$secName <- gsub("^SANTO","",puebla$secName)
puebla$secName <- gsub("^SANTA","",puebla$secName)
puebla$secName <- gsub("^SAN","",puebla$secName)


puebla <- mutate(puebla, region=paste(substring(entidad_nombre,1,2),
                                    substring(nombre_municipio,1,2),
                                    sep = "."))

puebla <- rename(puebla, Estado=entidad_nombre,Municipio=nombre_municipio,
                 Colonia=nombre_colonia)



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
                ## using PB.PU for state and municipio
                ## and initial of first and second name
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:nchar(puebla$secName[i])) {
                                newId <- paste(firstInitial,
                                               substr(puebla$secName[i],j,j),sep="")
                                newId <- paste("PB.PU",newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using any letter after PB.PU
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                for (k in 1:length(LETTERS)) {
                                        newId <- paste(LETTERS[j],
                                                       LETTERS[k],sep="")
                                        newId <- paste("PB.PU",newId,sep=".")
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
                ## using PU.PA for state and municipio
                ## and initial of first and second name
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:nchar(puebla$secName[i])) {
                                newId <- paste(firstInitial,
                                               substr(puebla$secName[i],j,j),sep="")
                                newId <- paste("PU.PA",newId,sep=".")
                                allreadyInBag <- newId %in% bag
                                if ( ! allreadyInBag ) {
                                        bag <- c(bag,newId)
                                        puebla$id[i] <- newId 
                                        cat(i, newId,"\n")
                                        break 
                                }
                        }
                }
                ## using any letter after PU.PA
                allreadyInBag <- newId %in% bag
                if ( allreadyInBag ) {
                        for (j in 1:length(LETTERS)) {
                                for (k in 1:length(LETTERS)) {
                                        newId <- paste(LETTERS[j],
                                                       LETTERS[k],sep="")
                                        newId <- paste("PU.PA",newId,sep=".")
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


combinations <- combn(LETTERS,4)
combinations <- sapply(1:ncol(combinations),function(x) paste(combinations[,x],collapse = ""))
combinations <- sapply(1:length(combinations), function(x) paste("PU",
                                                                 substr(combinations[x],1,2),
                                                                 substr(combinations[x],3,4),
                                                                 sep=".") )

for (i in 1:nrow(puebla)){
        if ( ( puebla$id[i] == '--.--.--' ) | ( nchar(puebla$id[i]) != nchar("PU.AB.CD") ) ) {
                for ( j in 1:length(combinations)) {
                        newId <- combinations[j]
                        allreadyInBag <- newId %in% bag
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                                break 
                        }
                }
        }
}

pueblaIds <- puebla[,c("Estado","Municipio","Colonia","id")]
write.csv(pueblaIds,file = "PUEBLAIDSv2.csv")


ids <- read.csv("PUEBLAIDSv2.csv")
length(unique(ids$id)) == length(ids$id)

ids <- read.csv("idsRegionv2.csv")
length(unique(ids$id)) == length(ids$id)
