 
setwd("~/scripts/iguanafixData/data/Sent/")
fileNames <- list.files()
csvs <- gsub(".xlsx",".csv",fileNames)
head(csvs)

for (i in 1:length(fileNames)) {
    command <- paste("ssconvert '",fileNames[i],"' '",csvs[i],"'",sep = "")
    system(command)
}

command <- "mv *.csv ~/scripts/iguanafixData/data/anticipos_txt"
system(command)

fileNames <- list.files("~/scripts/iguanafixData/data/anticipos_txt/")
head(fileNames)
