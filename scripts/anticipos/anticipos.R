library(dplyr)

###################################################################################################################
## Automating cash advance payment check

cat("Starting R script cash advance payment check\n")

## reading anticipos de ventas
anticiposDeVentas <- read.csv("Solicitud de Anticipos MX.csv")

## reading anticipos del sistema almacenados en periscope
## This table is quickly updated
url <- "https://app.periscopedata.com/api/iguanafix/chart/csv/f86d75ad-d8af-8aba-f414-ec9fa88e7970"
anticiposDePeriscope <- read.csv(url)



## filtering anticiposDeVentas
anticipos <- anticiposDeVentas
anticipos$Costo.Cliente.Final <- as.numeric(gsub(",","",anticipos$Costo.Cliente.Final))
anticipos$Costo.PRO.Final <- as.numeric(gsub(",","",anticipos$Costo.PRO.Final))
anticipos$Anticipo.Solicitado <- as.numeric(gsub(",","",anticipos$Anticipo.Solicitado))

check <- anticipos
## relating ventas and system data
#anticipos <- rename(anticipos, job_id = ID.JOB)
#check <- merge(x=anticipos,y=anticiposDePeriscope,by="job_id")
#check$"anticipoClienteSugerido" <-  ( check$Costo.Cliente.Final + check$Costo.PRO.Final ) / 2


## payment advance checks:
##
## check facturacion > monto pro
check$"fact_vs_montoPro" <- check$Costo.Cliente.Final >= 1.20*check$Costo.PRO.Final
## check margen > 10%
#check$"margen_check" <- with(check, ( Costo.Cliente.Final - Costo.PRO.Final ) / Costo.Cliente.Final ) > 0.1  
## formatting check
#check$"automatic_check" <- with(check, fact_vs_montoPro & margen_check )
check$automatic_check <- check$fact_vs_montoPro
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
##
##



## formatting and exporting table
check$"margen_previsto" <- with(check, ( Costo.Cliente.Final - Costo.PRO.Final ) )
check <- select(check,-(X..a.re.negociar:fact_vs_montoPro))

write.csv(check,"checkAnticipos.csv")

cat("Automated cash payment pay check Finished \n")

#######################################################################################################################

