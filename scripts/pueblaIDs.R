
puebla <- read.csv("DataGeograficaPuebla.csv",stringsAsFactors = FALSE)
puebla <- puebla[!duplicated(puebla),]

puebla$index <- 1:length(puebla$Estado)

puebla$filteredColonia <- gsub("[^[:alpha:]]","",puebla$Colonia)
puebla$filteredColonia <- gsub("_","",puebla$filteredColonia)
puebla$filteredColonia <- iconv(puebla$filteredColonia, to='ASCII//TRANSLIT')
letters <- with(puebla, lapply(1:length(filteredColonia), 
                               function(x) strsplit(filteredColonia[x],"") ) )
sortedLetters <- lapply(1:length(letters), 
                            function(x) toupper(letters[[x]][[1]]) )
#sortedLetters <- lapply(1:length(letters), 
#                            function(x) sort(unique(sortedLetters[[x]])) )
sortedChars <- sapply(1:length(sortedLetters), 
                        function(x) paste(sortedLetters[[x]],collapse = ""))
puebla$sortedLetters <- sortedChars

puebla$ids <- with(puebla, paste(substr(Estado,1,2),substr(Municipio,1,2),sep="."))
puebla$ids <- toupper(puebla$ids)
#puebla$ids <- sapply(1:length(puebla[,1]), 
#                     function(x) paste(puebla$ids[x],
#                                       puebla$sortedLetters[x],
#                                       #toupper(substr(sortedLetters[x],1,1)),
#                                       sep="."))
#
firstLetter <- iconv(substring(puebla$Colonia,1,1),to="ASCII//TRANSLIT")
secLetter <- sub(".* ","",puebla$Colonia)
secLetter <- iconv(substr(secLetter,1,1),to="ASCII//TRANSLIT")
idCol <- sapply(1:length(secLetter),function(x) paste(firstLetter[x],secLetter[x],sep=""))
puebla$idCol <- idCol
puebla$ID <- sapply(1:length(secLetter),function(x) paste(puebla$ids[x],puebla$idCol[x],sep="."))


puebla$finalID <- rep("--.--.--",length(puebla$Estado))
bag <- puebla$finalID[1]
for (i in 1:length(puebla$ids)) {
      if ( ! ( puebla$ID[i] %in% bag ) ){
           puebla$finalID[i] <- puebla$ID[i]
           bag <- c(bag,puebla$ID[i])
      }
}

for (i in 1:length(puebla$ids)) {
        if (puebla$finalID[i] == "--.--.--") {
              colonia1 <- strsplit(puebla$sortedLetters[i-1],"")
              colonia2 <- strsplit(puebla$sortedLetters[i],"")
              difLetters <- intersect(colonia1,colonia2)
              if ( length(difLetters) > 0 ) {
                    counter <- 1
                    newID <- paste(substr(puebla$ID[i],1,7),difLetters[counter],sep="")
                    while ( counter < length(difLetters) && newID %in% bag ) {
                          newID <- paste(substr(puebla$ID[i],1,7),difLetters[counter],sep="")
                          counter <- counter + 1
                    }
                    if ( ! ( newID %in% bag ) ) {
                          puebla$finalID[i] <- newID
                    }
              }


        } 
}

#puebla$ids <- sapply(1:length(puebla[,1]), 
#                     function(x) paste(puebla$ids[x],puebla$sortedLetters[x],sep="."))



