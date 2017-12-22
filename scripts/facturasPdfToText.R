dir <- "/home/carlos_ramirez/Dropbox/facturas_pdf"

facturasPaths <- list.files(dir)

facturasNames <- gsub(".pdf",".txt",facturasPaths)
facturasLoc <- paste("~/Dropbox/facturas_txt/",facturasNames,sep="")


for (i in 1:length(facturasPaths)) {
    system(paste("pdftotext ",dir,"/", facturasPaths[i], " ",facturasLoc[i],"",sep=""))
}


