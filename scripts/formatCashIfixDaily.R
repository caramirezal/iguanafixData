library(dplyr)
## setting english time names
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## reading local files
cashIfix <- read.csv("~/scripts/iguanafixData/data/cashIfixDaily.csv",
                     skip = 5,header = FALSE,nrows = 20)

## formatting data to table like for uploading
## to periscope
cashIfix <- cashIfix[,2:ncol(cashIfix)]
cnames <- cashIfix[,1]
dateTrans <- cashIfix[1,]
dateTrans <- sapply(dateTrans, function(i) paste(i,'2017',sep='-'))
dateTrans <- sapply(dateTrans, function(i) ifelse(nchar(i)==10,
                                            paste('0',i,sep=''),
                                            i))
dateTrans <- strptime(dateTrans,format = "%d-%b-%Y")
head(dateTrans)

cashIfix <- cashIfix[2:nrow(cashIfix),2:ncol(cashIfix)]
cashIfix <- t(cashIfix)
cashIfix <- as.data.frame(cashIfix)
colnames(cashIfix) <- cnames[2:length(cnames)]
cashIfix <- cashIfix[,-16]

for (i in 1:ncol(cashIfix)) {
        cashIfix[,i] <- as.numeric(gsub(",","",cashIfix[,i]))
}

## adding transaction date
cashIfix$"date_trans" <- dateTrans[2:length(dateTrans)]

## removing symbols from column names
newNames <- tolower(colnames(cashIfix))
newNames <- gsub("-","",newNames)
newNames <- gsub("  ","_",newNames)
newNames <- gsub(" ","_",newNames)
newNames <- gsub("\\(","",newNames)
newNames <- gsub("\\)","",newNames)
newNames <- gsub("&","",newNames)
newNames <- gsub(",","",newNames)
newNames <- gsub("_","",newNames)
colnames(cashIfix) <- newNames
colnames(cashIfix)

cashIfix[is.na(cashIfix)] <- 0
cashIfix$date_trans <- as.character(cashIfix$date)
cashIfix <- select(cashIfix,-datetrans)
colnames(cashIfix) <- gsub("_","",colnames(cashIfix))
head(cashIfix)

write.csv(cashIfix,"collectData.csv",row.names = FALSE)