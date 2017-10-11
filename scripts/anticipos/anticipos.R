
## Automating cash advance payment check

cat("Starting R script cash advance payment check\n")

## reading anticipos de ventas
anticiposDeVentas <- read.csv("Solicitud de Anticipos MX.csv")

## reading anticipos del sistema almacenados en periscope
## This table is quickly updated
url <- "https://app.periscopedata.com/api/iguanafix/chart/csv/f86d75ad-d8af-8aba-f414-ec9fa88e7970"
anticiposDePeriscope <- read.csv(url)

colnames(anticiposDeVentas)[1] <- "job_id"

## filter anticiposDeVentas
anticiposDeVentas.filtered <- anticiposDeVentas[,c("job_id",
                                                   "Costo.Cliente.Final",
                                                   "Costo.PRO.Final",
                                                   "Anticipo.Solicitado",
                                                   "Ok.Pablo")]
anticiposDeVentas.filtered$Costo.Cliente.Final <- as.numeric(gsub(",",
                                                                  "",
                                                                  anticiposDeVentas.filtered$Costo.Cliente.Final))
anticiposDeVentas.filtered$Costo.PRO.Final <- as.numeric(gsub(",",
                                         "",
                                         anticiposDeVentas.filtered$Costo.PRO.Final))
anticiposDeVentas.filtered$Anticipo.Solicitado <- as.numeric(gsub(",",
                                                              "",
                                                              anticiposDeVentas.filtered$Anticipo.Solicitado))


## relating ventas and system data
check <- merge(x=anticiposDeVentas.filtered,y=anticiposDePeriscope,by="job_id")
#check$"anticipoClienteSugerido" <-  ( check$Costo.Cliente.Final + check$Costo.PRO.Final ) / 2


check$"fact_vs_montoPro" <- check$monto_pro < check$facturacion
check$"margen_check" <- with(check, ( Costo.Cliente.Final - Costo.PRO.Final ) / Costo.Cliente.Final ) > 0.1  
check$"margen_previsto" <- with(check, ( Costo.Cliente.Final - Costo.PRO.Final ) )
check$"automatic_check" <- with(check, fact_vs_montoPro & margen_check )
formattedCheck <- character(length(check$automatic_check))
for ( i in 1:length(check$automatic_check)) {
  if ( check$automatic_check[i] == TRUE ) {
    formattedCheck[i] <- "GO" 
  } else { formattedCheck[i] <- "NO GO" }
}
check$automatic_check <- formattedCheck

check <- check[,c("job_id",
                  "Costo.Cliente.Final",
                  "Costo.PRO.Final",
                  "facturacion",
                  "margen_previsto",
                  "Anticipo.Solicitado",
                  "Ok.Pablo",
                  "automatic_check")]


write.csv(check,"checkAnticipos.csv")

cat("Automated cash payment pay check Finished \n")


