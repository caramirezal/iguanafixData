library(dplyr)

## Data retreived from ifix model and timming dashboard
url <- 'https://app.periscopedata.com/api/iguanafix/chart/csv/a41b019f-1b10-d714-5e08-27f9a52d39c2'
quotes <- read.csv(url)

quotes.vars <- quotes[,c('trabajo_relacionado_complete',
                         'visitas',
                         'categoria',
                         'reseller',
                         'pro_asignado',
                         'company_id',
                         'dias_envio_pdf',
                         'horas_envio_pro',
                         'horas_envio_pdf',
                         'precio_trabajo_relacionado',
                         'descuento_ofrecido',
                         'estado_trabajo_relacionado',
                         'horas_realizaion_visita',
                         'horas_total_envio')]

qLinMod <- lm(trabajo_relacionado_complete~categoria,data = as.data.frame(quotes.vars))
              