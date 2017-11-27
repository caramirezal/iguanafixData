
#library(dplyr)


###################################################################################################################
## Automating cash advance payment check




## reading anticipos del sistema almacenados en periscope
## This table is quickly updated
url <- "https://app.periscopedata.com/api/iguanafix/chart/csv/f86d75ad-d8af-8aba-f414-ec9fa88e7970"
check <- read.csv(url)



check$"margen" <- check$facturacion - check$monto_pro


## payment advance checks:
## check facturacion > monto pro
check$"automatic_check" <- check$facturacion >= 1.20*check$monto_pro
## formatting check
formattedCheck <- character(length(check$automatic_check))
for ( i in 1:length(check$automatic_check)) {
        if ( is.na(check$automatic_check[i] ) ) {
                formattedCheck[i] <- "NO GO"
        } else {
                if ( check$automatic_check[i] == TRUE ) {
                        formattedCheck[i] <- "GO" 
                } else { formattedCheck[i] <- "NO GO" }
        }

}
check$automatic_check <- formattedCheck

write(paste("Check before tags ",date(),sep=" ::: "),file="/home/carlos_ramirez/tracking_r.txt")

check$tags <- sapply(check$tags, function(x) ifelse(grepl("CID",x),
                                gsub(",.*","",gsub(".*CID","",x)),
                                NA) )


fileName <- paste("/home/carlos_ramirez/data/pagos_",Sys.time(),".csv",sep="")
fileName <- gsub(" ","_",fileName)
write.csv(check,fileName)
system(paste("/home/carlos_ramirez/gdrive upload -p 0B2I1tf9BPlOddGNpSEFaQmZkRU0  ",fileName,sep=""))




write(paste("Check after system ",date(),sep=" ::: "),file="/home/carlos_ramirez/tracking_r.txt")




#######################################################################################################################

