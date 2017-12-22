
dir <- "/Users/carlos/Dropbox/facturas_txt/"
facturas <- list.files(dir)

total <- rep('0',length(facturas))
fecha <- rep('',length(facturas))
jobIds <- rep('0',length(facturas))
orderIds <- rep('0',length(facturas))
cliente <- rep('NA',length(facturas))

for (i in 1:length(facturas)) {
        docLines <- readLines(paste(dir,facturas[i],sep=""))
        fecha[i] <- grep("201",docLines,value=TRUE)[1]
        total[i] <- grep("TOTAL",docLines,value=TRUE)[2]
        jobIds[i] <- ifelse(length(grep("job|no trabajo",tolower(docLines)))>0
                            ,grep("job|no trabajo",tolower(docLines),value=TRUE)
                            ,'0')
        orderIds[i] <- ifelse( length(grep("orde",tolower(docLines))) > 0,
                               grep("orde",tolower(docLines),value=TRUE),
                              '0')
        cliente[i] <- ifelse(length(grep("client",tolower(docLines)))>0,
                             grep("client",tolower(docLines),value = TRUE),
                              'NA')
        
}

fecha <- gsub("FECHA: ","",fecha)
fecha <- strptime(fecha,format = "%d/%m/%Y")
fecha <- as.character(fecha)
fecha[is.na(fecha)] <- ''
res <- data.frame(date=fecha)
total <- gsub("TOTAL: ","",total)
total <- as.numeric(gsub(",","",total))
total[is.na(total)] <- 0
res$"total" <- total
res$"job_id" <- gsub("id","",jobIds)
res$job_id <- gsub("job","",res$job_id)
res$job_id <- gsub("no trabajo","",res$job_id)
res$job_id <- gsub(":","",res$job_id)
res$job_id <- gsub(" ","",res$job_id)
res$job_id <- gsub("cotizacionww","",res$job_id)
res$"order_id" <- gsub(".*order","",orderIds)
res$order_id <- gsub(" ","",res$order_id)
res$order_id <- gsub(".*:","",res$order_id)
res$order_id <- gsub("noorden","",res$order_id)
res$"client" <- gsub("cliente: ","",cliente)
res$date[is.na(res$date)] <- ''
res$"file_name" <- gsub(".txt","",facturas)

write.csv(res,
          "~/GitHub/iguanafixData/data/facturacionMX.csv",
          row.names = FALSE)

