## Bank transfers from Arturo Report 
cashIfix <- read.csv("~/GitHub/iguanafixData/data/BBVAtransIfix2018.csv")

detail <- as.character(cashIfix$detail)
date <- as.character(cashIfix$date)

res <- list()

## extract ids from detail column by spliting space separated the string, casting 
## and taking the non NA values
for (i in 1:length(detail)) {
        extracted <- gsub("#",",",detail[i])
        extracted <- gsub(" ",",",detail[i])
        extracted <- unlist(strsplit(extracted,","))
        extracted <- as.numeric(extracted)
        extracted <- extracted[!is.na(extracted)]
        if ( length(extracted) > 0 ) {
                for (j in 1:length(extracted)) {
                        res <- c(res,list(c(date[i],extracted[j]))) 
                }
        }
}

## formatting and writting the result
res <- as.data.frame(do.call(rbind, res))
names(res) <- c("payment_date","job_id")
res$job_id <- as.numeric(as.character(res$job_id))
write.csv(res, "~/GitHub/iguanafixData/data/gmv_corrido_payments_dates_2018.csv",row.names = FALSE)
