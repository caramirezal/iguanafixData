library(dplyr)

anticipos <- read.csv("~/GitHub/iguanafixData/data/Solicitud de Anticipos MX - Anticipos Octubre.csv")

fecha_pago <- anticipos$Fecha.AplicaciÃ³n.Anticipo
fecha_solicitud <- anticipos$Fecha.Solicitud
okPablo <- anticipos$Ok.Pablo
okandres <- anticipos$Ok.AndrÃ.s

anticipos <- select(anticipos,id_job:Anticipo.Solicitado)
anticipos <- select(anticipos,-(Fecha.Solicitud:X..Margen))
anticipos$"request_date" <- fecha_solicitud
anticipos$"pay_date" <- fecha_pago
anticipos$"okpablo" <- okPablo
anticipos$"okandres" <- okandres
anticipos$request_date <- as.character(strptime(anticipos$request_date,format = '%d-%b-%Y'))
anticipos$pay_date <- as.character(strptime(anticipos$pay_date,format = '%d-%b-%Y'))
anticipos <- mutate(anticipos,id_job=as.numeric(as.character(id_job)))
anticipos <- mutate(anticipos,CID.PRO=as.numeric(as.character(CID.PRO)))
anticipos <- mutate(anticipos,Anticipo.Solicitado=as.numeric(
                                                         gsub(',',
                                                              '',
                                                              as.character(Anticipo.Solicitado))))

names(anticipos) <- tolower(names(anticipos))
names(anticipos) <- gsub('\\.','',names(anticipos))

anticipos$request_date[is.na(anticipos$request_date)] <- ''
anticipos$pay_date[is.na(anticipos$pay_date)] <- ''
anticipos$anticiposolicitado[is.na(anticipos$anticiposolicitado)] <- 0

write.csv(anticipos,'~/GitHub/iguanafixData/data/anticipos.csv',row.names = FALSE)
