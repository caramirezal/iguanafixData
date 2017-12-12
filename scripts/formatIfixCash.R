
library(dplyr)

cashIfix <- read.csv('/Users/carlos/Downloads/Cash Position_IguanaFix MX_2017.xlsx - Daily Cashflow.csv',
                     skip=7,
                     header = FALSE,
                     nrows = 19)

cashIfix <- t(cashIfix)
cashIfix <- cashIfix[2:nrow(cashIfix),]
cashIfix <- cashIfix[,1:(ncol(cashIfix)-1)]

## 
rnames <- read.csv('/Users/carlos/Downloads/Cash Position_IguanaFix MX_2017.xlsx - Daily Cashflow.csv',
                     skip=5,
                     header = FALSE,
                     nrows = 1)
rnames <- rnames[1,2:length(rnames)]
rnames <- as.character(unlist(rnames))
#cashIfix <- as.data.frame(cashIfix)
row.names(cashIfix) <- rnames

cnames <- cashIfix[1,]
cashIfix <- cashIfix[2:nrow(cashIfix),]
colnames(cashIfix) <- cnames

cashIfix <- as.data.frame(cashIfix)
head(cashIfix)

dates <- sapply(row.names(cashIfix),function(i) paste(i,'2017',sep='-'))
dates <- strptime(dates,format = '%d-%b-%y')

cashIfix$date <- dates

for ( i in 1:(ncol(cashIfix)-1) )  {
        cashIfix[,i] <- as.numeric(gsub(',','',cashIfix[,i]))
} 

cashIfix[is.na(cashIfix)]  <- 0
                      
colnames(cashIfix) <- gsub(' ','',colnames(cashIfix))
colnames(cashIfix) <- gsub(',','',colnames(cashIfix)) 
colnames(cashIfix) <- gsub('\\-','',colnames(cashIfix))  
colnames(cashIfix) <- gsub('\\&','',colnames(cashIfix)) 
colnames(cashIfix) <- gsub('\\(','',colnames(cashIfix))
colnames(cashIfix) <- gsub('\\)','',colnames(cashIfix)) 

write.csv(cashIfix,'cashIfix2017.csv',row.names = FALSE)
