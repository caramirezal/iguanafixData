## Lista de pros activos e inactivos en el último mes y medio

## https://app.periscopedata.com/app/iguanafix/181941/pros-mx-analysis
prosActivos <- read.csv("~/data/pros/Prosactivos45das_2017-10-13_1017.csv")

## tomado de https://app.periscopedata.com/app/iguanafix/181941/pros-mx-analysis?widget=2477147&udv=0&redesign=1
totalPros <- read.csv("~/data/pros/Listadeprosmx_2017-10-13_1148.csv")

inactivePros <- totalPros[ ! totalPros$company_id %in% prosActivos$company_id, ]

write.csv(inactivePros, file="~/data/pros/inactivePros.csv",row.names = FALSE)
