library(dplyr)
library(googlesheets)

## reading data from periscope
url <- "https://app.periscopedata.com/api/iguanafix/chart/csv/5efe034e-efd6-8a0a-bc69-f0eebd5ea9c3/299646"
data <- read.csv(url,stringsAsFactors = FALSE)

months <- unique(data$month[order(data$month)])

## definition of matrix cohort to store data from revenues per month per cohort
cohort <- matrix(0,
                 nrow = length(months),
                 ncol = length(months))


rownames(cohort) <- months
colnames(cohort) <- months

## filling cohort matrix with the values from periscope
for (i in 1:nrow(data)) {
        cohort[data$cohort_month[i],data$month[i]] <- data$facturacion[i] 
}

## diagonal values are the first_month_revenues
first_revenues <- diag(cohort)

## formatting results. Adding cohort tags and first month values
cohort.df <- as.data.frame(cohort)
cohort.df <- mutate(cohort.df,cohort_month=colnames(cohort))
cohort.df <- mutate(cohort.df,first_month_revenues=diag(cohort))
rownames(cohort.df) <- colnames(cohort.df)[1:13]

## normalization of data by dividing by the first month cohort value
for (i in 1:13) {
        if ( cohort.df$first_month_revenues[i] != 0) {
                cohort.df[i,1:13] <- cohort.df[i,1:13] / cohort.df$first_month_revenues[i]
        } 
}


head(cohort.df)

## uploading data to google Spreadsheet
boring_ss <- gs_new("CohortTable", ws_title = "CohortChart", input = cohort.df,
                    trim = TRUE, verbose = FALSE)
boring_ss %>% 
        gs_read()
