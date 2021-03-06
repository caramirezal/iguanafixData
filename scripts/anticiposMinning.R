library(dplyr)

## extracting names from xlsx files
setwd("~/scripts/iguanafixData/data/Sent/")
fileNames <- list.files()
csvs <- gsub(".xlsx",".csv",fileNames)

## Applying ssconvert to convert xlsx to csv
for (i in 1:length(fileNames)) {
    command <- paste("ssconvert '",fileNames[i],"' '",csvs[i],"'",sep = "")
    system(command)
}

## moving csvs to a single file for convinience
command <- "mv *.csv ~/scripts/iguanafixData/data/anticipos_txt"
system(command)

## moving to the file containing the csvs
setwd("~/scripts/iguanafixData/data/anticipos_txt/")
fileNames <- list.files("~/scripts/iguanafixData/data/anticipos_txt/")

lines <- readLines(fileNames[1])

## extract id job
extractJobId <- function(csv) {
        lines <- readLines(csv)
        jobId <- lines[grepl("de Trabajo",lines)][1]
        jobId <- gsub(".* de Trabajo","",jobId)
        jobId <- gsub("*.,","",jobId)
        jobId <- gsub(" ","",jobId)
        jobId        
}

## extract client name
extractClientId <- function(csv){
        csv <- readLines(csv)
        clienteId <- csv[grepl("Cliente",csv)][1]
        clienteId <- gsub('.*Cliente',"",clienteId)
        clienteId <- gsub('N°.*',"",clienteId)
        clienteId <- gsub(",","",clienteId)
        clienteId <- gsub('"','',clienteId)
        clienteId        
}

## extract total price
extractTotal <- function(csv){
        lines <- readLines(csv)
        total <- lines[grepl("IMPORTE TOTAL",lines)][1]
        total <- gsub("IMPORTE TOTAL","",total)
        total <- gsub(",","",total)
        total <- gsub('\"','',total)
        total
}

## extract pro price
extractMontoPro <- function(csv){
        lines <- readLines(csv)
        montoPro <- lines[grepl("Mano de obra",lines)][1]
        montoPro <- gsub("Mano de obra","",montoPro)
        montoPro <- gsub(",","",montoPro)
        montoPro <- gsub('\"',"",montoPro)
        montoPro <- as.character(montoPro)
        montoPro <- as.numeric(montoPro)
        montoPro[is.na(montoPro)] <- 0
        montoPro
}

## extract date
extractDate <- function(csv){
        lines <- readLines(csv)
        date <- lines[grepl("Domicilio",lines)][1]
        date <- gsub(".*Fecha","",date)
        date <- gsub('\"','',date)
        date <- gsub(',','',date)
        date <- gsub(' ','',date)
        date <- strptime(date,format="%Y/%m/%d")
        date <- as.character(date)
        date
}


## formatting extracted data
date <- sapply(fileNames,function(i) extractDate(i))
jobIds <- sapply(fileNames,function(i) extractJobId(i))
jobIds <- as.numeric(as.character(jobIds))
jobIds[is.na(jobIds)] <- 0 
clientIds <- sapply(fileNames,function(i) extractClientId(i))
totals <- sapply(fileNames, function(i) extractTotal(i))
totals <- as.numeric(as.character(totals))
totals[is.na(totals)] <- 0
montoPro <- sapply(fileNames, function(i) extractMontoPro(i))
        
## definition of the resume table
res <- data.frame("date"=date,
                  "job_id"=jobIds,
                  "client"=clientIds,
                  "total"=totals,
                  "montoPro"=montoPro,
                  "file"=fileNames)

## storing the result
## This table is called quotes_mx in periscopedata
write.csv(res,
          file="~/scripts/iguanafixData/data/anticipos.csv",
          row.names = FALSE)


