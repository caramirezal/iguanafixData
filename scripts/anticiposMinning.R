library(dplyr) 
setwd("~/scripts/iguanafixData/data/Sent/")
fileNames <- list.files()
csvs <- gsub(".xlsx",".csv",fileNames)
head(csvs)

for (i in 1:length(fileNames)) {
    command <- paste("ssconvert '",fileNames[i],"' '",csvs[i],"'",sep = "")
    system(command)
}

command <- "mv *.csv ~/scripts/iguanafixData/data/anticipos_txt"
system(command)


setwd("~/scripts/iguanafixData/data/anticipos_txt/")
fileNames <- list.files("~/scripts/iguanafixData/data/anticipos_txt/")

lines <- readLines(fileNames[1])

extractJobId <- function(csv) {
        lines <- readLines(csv)
        jobId <- lines[grepl("de Trabajo",lines)][1]
        jobId <- gsub(".* de Trabajo","",jobId)
        jobId <- gsub("*.,","",jobId)
        jobId <- gsub(" ","",jobId)
        jobId        
}

extractClientId <- function(csv){
        csv <- readLines(csv)
        clienteId <- csv[grepl("Cliente",csv)][1]
        clienteId <- gsub('.*Cliente',"",clienteId)
        clienteId <- gsub('N°.*',"",clienteId)
        clienteId <- gsub(",","",clienteId)
        clienteId <- gsub('"','',clienteId)
        clienteId        
}

extractTotal <- function(csv){
        lines <- readLines(csv)
        total <- lines[grepl("IMPORTE TOTAL",lines)][1]
        total <- gsub("IMPORTE TOTAL","",total)
        total <- gsub(",","",total)
        total <- gsub('\"','',total)
        total
}


jobIds <- sapply(fileNames,function(i) extractJobId(i))
jobIds <- as.numeric(as.character(jobIds))
jobIds[is.na(jobIds)] <- 0 
clientIds <- sapply(fileNames,function(i) extractClientId(i))
totals <- sapply(fileNames, function(i) extractTotal(i))
totals <- as.numeric(as.character(totals))
totals[is.na(totals)] <- 0
res <- data.frame("job_id"=jobIds,
                  "client"=clientIds,
                  "total"=totals,
                  "file"=fileNames)

write.csv(res,
          file="~/scripts/iguanafixData/data/anticipos.csv",
          row.names = FALSE)


length(fileNames)
nrow(res)
