dir <- "/home/carlos_ramirez/Dropbox/Collections/B2B"

facturasPaths <- list.files(dir, pattern="FA00.*.pdf",recursive=TRUE)

facturasNames <- gsub(".pdf",".txt",facturasPaths)
facturasNames <- gsub(".*FA00","FA00",facturasNames)
facturasLoc <- paste("~/Dropbox/facturas/",facturasNames,sep="")


#for (i in 1:length(facturasPaths)) {
#    system(paste("pdftotext '",dir,"/", facturasPaths[i], "' ",facturasLoc[i],"",sep=""))
#}

files <- list.files("~/Dropbox/facturas")
cat("No de files",length(files),"\n")
cat("No de pdfs ",length(facturasPaths),"\n")

total <- rep("NA",length(files))

for (i in 1:length(files)) {
    lines <- readLines(paste("~/Dropbox/facturas/",files[i],sep="")) 
    total[i] <- grep("TOTAL:",lines,value=TRUE)[2]
}

cat(total[1:50],"\n")

