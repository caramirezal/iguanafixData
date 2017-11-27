## quote minning

quote <- read.xlsx("~/data/17-10-06_373077_OscarOjeda_JuanBarrera51.xlsx",
                   sheetIndex = 1,colClasses = "character")


getClient <- function(data) {
        for (i in 1:length(data)) {
                if (  length(grep("Client",as.character(data[,i])) ) > 0 ) {
                        res <- c("Client" = as.character(
                                quote[grep("Client",
                                           as.character(data[,i])),
                                      i+2]))
                }
        }
        res
}

res <- getClient(quote)
cat(res,"\n")

data <- quote
for (i in 1:length(data)) {
        if (  length(grep("Mano",as.character(data[,i])) ) > 0 ) {
                res <- c("Mano de obra" = as.character(
                        quote[grep("Mano",
                                   as.character(data[,i])),
                              i+2]))
        }
}

getMontoPro <- function(data) {
        for (j in 1:length(data)) {
                for (i in 1:length(data[,1])) {
                        if ( length(grep("Mano de obra",data[i,j])) > 0 ) {
                                #cat(as.character(quote[i,j]),"\n")
                                noNA <- which(data[i,]!="NA")
                                res <- as.character(data[i,noNA[2]])
                                break       
                        }
                }
                if ( length(grep("Mano de obra",data[i,j])) > 0 ) break
        }     
        res <- as.numeric(res)
        c("ManoDeObra"=res)
}

getMontoPro(quote)


