

fileNames <- readLines("/home/carlos_ramirez/MEX_IVA_TEST_list.csv")
fileNames <- gsub(" .*","",fileNames)

#write.csv(fileNames,"/home/carlos_ramirez/MEX_IVA_TEST_list.csv") 


## downloading all files in MX IVA TEST dir
for (i in 2:length(fileNames)) {
       command <- paste("sudo gdrive export -f ",fileNames[i],sep="")
       system(command)
}

fileNames <- list.files()

extractData <- function(file,pattern1,pattern2,pattern3){
     quoteLines <- readLines(file)
     adressIndex <- grep(pattern1,quoteLines)
     adressLine <- quoteLines[adressIndex]
     extractedData <- gsub(pattern2,"",adressLine)
     extractedData <- gsub(pattern3,"",extractedData)
     extractedData <- gsub('\\"','',extractedData)
     extractedData <- gsub(",","",extractedData)
     extractedData <- gsub("\\.","",extractedData)
     extractedData <- gsub(".*(estimados)","",extractedData)
     extractedData <- gsub(".*)","",extractedData)
     return(extractedData)
}

## extract data from quotes
extractQuote <- function(file) {
                
        client <- extractData(file,"Cliente",".*Cliente","N° de Trabajo.*")
        adress <- extractData(file,"Domicilio",".*Localidad","Fecha.*")
        date <- extractData(file,"Domicilio",".*Fecha","XXXX")
        jobID <- extractData(file,"N° de Trabajo",".*N° de Trabajo","XXXX")
        jobPrice <- extractData(file,"Mano de obra",".*Mano de obra","XXXX")
        material <- extractData(file,"Materiales",".*Materiales","XXXX")
        adds <- extractData(file,"Adicionales especiales",".*especiales","XXXX")
        visitDiscount <- extractData(file,"Descuento de visita",".*visita","XXXX")
        total <- extractData(file,"IMPORTE TOTAL",".*TOTAL","XXXX")

         res <- c("client"=client,
                  "adress"=adress,
                  "date"=date,
                  "job_id"=jobID,
                  "jobPrice"=jobPrice,
                  "material"=material,
                  "adds"=adds,
                  "visitDiscount"=visitDiscount,
                  "total"=total)

         return(res)
}

quotes <- lapply(fileNames, function(x) extractQuote(x) )

write.csv("hola quote","quotes_summary.csv")
write.csv(as.data.frame(quotes),"output.csv")

