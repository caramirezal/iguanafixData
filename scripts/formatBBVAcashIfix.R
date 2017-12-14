library(dplyr)

cashIfix <- read.csv("~/GitHub/iguanafixData/data/Cash_Ifix_Bank_BBVA.csv",
                     skip = 4,
                     header=TRUE)

names(cashIfix) <- tolower(names(cashIfix))
names(cashIfix) <- gsub('\\.','',names(cashIfix))
cashIfix <- rename(cashIfix,cargo=nâcargo)
cashIfix <- select(cashIfix,date:detail)
cashIfix <- mutate(cashIfix,amount=as.numeric(gsub(',','',amount)))
cashIfix <- mutate(cashIfix,balance=as.numeric(gsub(',','',balance)))
dateFormat1 <- grep("/",cashIfix$date)
dateFormat1 <- as.character(strptime(cashIfix$date[dateFormat1],format = '%d/%m/%Y'))

dateFormat2 <- as.character(cashIfix$date[grep("-",cashIfix$date)])
dateFormat2 <- sapply(dateFormat2, function(i) ifelse(nchar(i)==10,
                                                      paste('0',i,sep = ''),
                                                      i)) 
dateFormat2 <- as.character(strptime(dateFormat2,format = '%d-%b-%Y'))

cashIfix <- cashIfix[grepl('-',cashIfix$date) | grepl('/',cashIfix$date),]
cashIfix$date <- c(dateFormat1,dateFormat2)

cashIfix$amount[is.na(cashIfix$amount)] <- 0
cashIfix$balance[is.na(cashIfix$balance)] <- 0

write.csv(x = cashIfix,
          file = '~/GitHub/iguanafixData/data/BBVAtransIfix.csv',
          row.names = FALSE)



