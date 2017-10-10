
## reading anticipos de ventas
anticiposDeVentas <- read.csv("Solicitud de Anticipos MX.csv")

## reading acticipos del sistema almacenados en periscope
url <- "https://app.periscopedata.com/api/iguanafix/chart/csv/f86d75ad-d8af-8aba-f414-ec9fa88e7970"
anticiposDePeriscope <- read.csv(url)

check.df <- merge(x=anticiposDePeriscope,y=anticiposDeVentas,by="job_id")

check.df$Costo.Cliente.Final <- as.numeric(gsub(",","",check.df$Costo.Cliente.Final))
check.df$Costo.PRO.Final <- as.numeric(gsub(",","",check.df$Costo.PRO.Final))

checkMontoPro <- abs(check.df$monto_pro - check.df$Costo.PRO.Final) 
checkJobPrice <- abs(check.df$Costo.Cliente.Final - check.df$facturacion)

anticiposGO <- data.frame(costo_cliente_final=numeric(0),
                          costo_PRO_final=numeric(0))

write.csv(check.df,"checkAnticipos.csv")

