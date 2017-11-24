

fileNames <- readLines("/Users/carlos/Dropbox/MEX_IVA_TEST_list.csv")
fileNames <- gsub(" .*","",fileNames)
#write.csv(fileNames,"/home/carlos_ramirez/MEX_IVA_TEST_list.csv")


for (i in 2:length(fileNames)) { 
        command <- paste("sudo gdrive export -f ",fileNames[i],sep="")
        system(command)
}

fileNames <- list.files()

firstLine <- sapply(1:length(fileNames), function(x) readLines(fileNames[x])[4])

write.csv(fileNames,"quotes_summary.csv")


