library(dplyr)

## data taken from https://docs.google.com/spreadsheets/d/19JFc3WtB7tPJ-F8kH4edEhjBd7eaY3wIPE3eN31vsEc/edit?usp=sharing
## as provided and edited from Joaquin Copy_of_VTAS_DIC_17_ENE_18.xlsx file
data <- read.csv("/Users/carlos/Documents/GitHub/iguanafixData/data/Copy_of_VTAS_DIC_17_ENE_18.csv",
                 stringsAsFactors = FALSE)

## renaming columns
names(data) <- c("fecha","no_factura","cliente","subtotal","iva","total")

## 2018 has "m/d/yyyy" but 2017 is "dd/mm/yyyyy" format
data$fecha <- sapply(data$fecha, function(x) ifelse(grepl('2018',x),
                                                    as.character(strptime(x, format="%m/%d/%Y")),
                                                    as.character(as.Date(x,format = "%d/%m/%Y"))))
## removing commas from montos
data <- mutate(data,
               subtotal=gsub(",","",subtotal),
               iva=gsub(",","",iva),
               total=gsub(",","",total))

## removing white spaces from montos
data <- mutate(data,
               subtotal=gsub(" ","",subtotal),
               iva=gsub(" ","",iva),
               total=gsub(" ","",total))

## cast montos to numeric
data <- mutate(data,
               subtotal=as.numeric(subtotal),
               iva=as.numeric(iva),
               total=as.numeric(total))

## replacing NAs values to 0 (Important for periscope uploading)
data$subtotal[is.na(data$subtotal)] <- 0
data$iva[is.na(data$iva)] <- 0
data$total[is.na(data$total)] <- 0

##  saving the data to local file GitHub/iguanafixData/data
write.csv(data, 
      "/Users/carlos/Documents/GitHub/iguanafixData/data/ventas_dic_enero_2018.csv",
      row.names = FALSE)
