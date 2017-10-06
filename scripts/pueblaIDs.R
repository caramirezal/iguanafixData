
puebla <- read.csv("DataGeograficaPuebla.csv",stringsAsFactors = FALSE)
puebla <- puebla[!duplicated(puebla),]



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

puebla$firstName <- substr(puebla$filteredColonia,1,3)

##
puebla$secName <- gsub("^LA","",puebla$filteredColonia)
puebla$secName <- gsub("^EL","",puebla$secName)
puebla$secName <- gsub("^SANTO","",puebla$secName)
puebla$secName <- gsub("^SANTA","",puebla$secName)
puebla$secName <- gsub("^SAN","",puebla$secName)

puebla$region <- paste(substr(puebla$Estado,1,2),
                       substr(puebla$Municipio,1,2),
                       sep=".")
puebla$region <- toupper(puebla$region)

bag <- NULL
puebla$id <- "--.--.--"
for (i in 1:length(puebla$Estado)) {
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
                }
        }
        if ( allreadyInBag ) {
                region <- paste(substr(puebla$region[i],1,4),
                               toupper(substr(puebla$Municipio[i],3,3)),
                               sep="")
                firstInitial <- substr(puebla$firstName[i],1,1)
                secondInitial <- substr(puebla$secName[i],1,1)
                newId <- paste(firstInitial,secondInitial,sep="")
                newId <- paste(region,newId,sep=".")
                if ( ! allreadyInBag ) {
                        bag <- c(bag,newId)
                        puebla$id[i] <- newId 
                        cat(i, newId,"\n")
                }
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
                        }
                }
                if ( allreadyInBag ) {
                        region <- paste(substr(puebla$region[i],1,4),
                                        toupper(substr(puebla$Municipio[i],4,4)),
                                        sep="")
                        firstInitial <- substr(puebla$firstName[i],1,1)
                        secondInitial <- substr(puebla$secName[i],1,1)
                        newId <- paste(firstInitial,secondInitial,sep="")
                        newId <- paste(region,newId,sep=".")
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                        }
                }
                if ( allreadyInBag ) {
                        region <- paste(substr(puebla$region[i],1,4),
                                        toupper(substr(puebla$Municipio[i],5,5)),
                                        sep="")
                        firstInitial <- substr(puebla$firstName[i],1,1)
                        secondInitial <- substr(puebla$secName[i],1,1)
                        newId <- paste(firstInitial,secondInitial,sep="")
                        newId <- paste(region,newId,sep=".")
                        if ( ! allreadyInBag ) {
                                bag <- c(bag,newId)
                                puebla$id[i] <- newId 
                                cat(i, newId,"\n")
                        }
                }
        }
}

pueblaIds <- puebla[,c("Estado","Municipio","Colonia","id")]
write.csv(pueblaIds,file = "PUEBLAIDS.csv")