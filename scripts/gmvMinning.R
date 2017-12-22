
cashIfix <- read.csv("~/GitHub/iguanafixData/data/BBVAtransIfix.csv")

detail <- as.character(cashIfix$detail)
date <- as.character(cashIfix$date)

res <- list()

for (i in 1:length(detail)) {
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

res <- as.data.frame(do.call(rbind, res))
names(res) <- c("payment_date","job_id")
res$job_id <- as.numeric(res$job_id)

write.csv(res, "~/GitHub/iguanafixData/data/gmv_corrido_payments_dates.csv",row.names = FALSE)
