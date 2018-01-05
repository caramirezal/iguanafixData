library(ggplot2)
library(devtools)
install_github("easyGgplot2","kassambara")
library(easyGgplot2)
library()


leads <- read.csv("https://app.periscopedata.com/api/iguanafix/chart/csv/a50739aa-9f2a-9c1d-adf2-8612451f47f6/328445")


g <- ggplot(leads[leads$trk_source=='www.google.com',], 
            aes(x=trk_source,y=sucess)) + geom_violin()
g <- g + scale_x_discrete(limits=c("0","1"))
g
