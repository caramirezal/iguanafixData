
puebla <- read.csv("DataGeograficaPuebla.csv",stringsAsFactors = FALSE)


estado <- substr(puebla$Estado,1,2)
municipio <- substr(puebla$Municipio,1,2)
puebla$Colonia <- gsub("\\(","",puebla$Colonia)
puebla$Colonia <- gsub("\\)","",puebla$Colonia)
puebla$Colonia <- gsub("\\.","",puebla$Colonia)
first <- substr(puebla$Colonia,1,2)
second <- substr(gsub(".* ","",puebla$Colonia),1,1)

paste(estado[1],municipio[1],paste(first[1],second[1],sep=""),sep=".")

ids <- sapply(1:length(puebla[,1]), function(x) paste(estado[x],
                                                      municipio[x],
                                                      first[x],
                                                      sep="."))
ids <- toupper(ids)
puebla$ids <- ids

##puebla <- puebla[order(puebla$ids),]

usedIDs <- puebla$ids[1]
for (i in 1:length(puebla[,1])) {
        newID <- puebla$ids[i]
     while ( ( newID %in% usedIDs  ) || 
             substr(newID,8,8) == " "
             || grepl(substr(newID,8,8),"0123456789" ) )  {
             randomNum <- sample(1:nchar(puebla$Colonia[i]),1)
             randomchr <- substr(puebla$Colonia[i],randomNum,randomNum)
             newID <- paste(substr(newID,1,7),randomchr,sep="")
             newID <- toupper(newID)
     }
        usedIDs <- c(usedIDs,newID)
        cat(i,": ",usedIDs[i],"\n")
}


