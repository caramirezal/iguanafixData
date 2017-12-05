library(devtools)
library(plotFun)
library(ggplot2)
library(dplyr)
library(reshape2)


###################################################################################
## Timmings explorative analysis


timmings <- read.csv("https://app.periscopedata.com/api/iguanafix/chart/csv/00eb5d3c-6c3d-1433-8397-c5e5c4343828")



g <- ggplot(timmings, aes(as.numeric(visit2quote),
                        fill = as.factor(currency)))
g <- g + geom_density(alpha = 0.5)
g <- g + labs(x = 'Time',
              y = 'Frequency',
              title = 'Quote pending time',
              fill = 'Country')

g <- g + geom_vline(xintercept = c(mean(timmings[timmings$currency=='ARS',"visit2quote"],na.rm = TRUE),
                                   mean(timmings[timmings$currency=='MXN',"visit2quote"],na.rm = TRUE),
                                   mean(timmings[timmings$currency=='BRL',"visit2quote"],na.rm = TRUE)),
                    color = c("pink","steelblue","green"))


sumary.countries <- cbind(summary(timmings[timmings$currency=='ARS',"visit2quote"]),
                          summary(timmings[timmings$currency=='BRL',"visit2quote"]),
                          summary(timmings[timmings$currency=='MXN',"visit2quote"]))
colnames(sumary.countries) <- c('ARS','BRL','MXN')

###################################################################################
## Correction by number of quotes
url <- 'https://app.periscopedata.com/api/iguanafix/chart/csv/3468efc3-89d9-fcdf-14f7-5db32d32a845'
timmings.cor <- read.csv(url)



g <- ggplot(timmings.cor, aes(as.numeric(mean_corrected),
                          fill = as.factor(currency)))
g <- g + geom_density(alpha = 0.5)
g <- g + labs(x = 'Time',
              y = 'Frequency',
              title = 'Quote pending time',
              fill = 'Country')

g <- g + geom_vline(xintercept = c(mean(timmings.cor[timmings.cor$currency=='ARS',"mean_corrected"],na.rm = TRUE),
                                   mean(timmings.cor[timmings.cor$currency=='MXN',"mean_corrected"],na.rm = TRUE),
                                   mean(timmings.cor[timmings.cor$currency=='BRL',"mean_corrected"],na.rm = TRUE)),
                    color = c("pink","steelblue","green"))
g <- g + xlim(-20,250)
plot(g)

###################################################################################
mex <- filter(timmings,currency=='MXN')

ordvisit2ord.sum <- summary(mex$ordvisit2ord)
ord2project.sum <- summary(mex$ord2project)

timmings.mx <- mutate(mex,
                      ordvisit2ord_desp=ordvisit2ord,
                      ord2project_desp=ord2project + ordvisit2ord.sum['Median'])

timmings.mx <- select(timmings.mx,
                      ordvisit2ord_desp:ord2project_desp)
colnames(timmings.mx) <- c("orderVisit2ordProject",
                           "ord2Project")

g <- mhist(timmings.mx)
g <- g + xlim(-600,1000)
g


##################################################################################
## plotting timmings as boxplots

timmings.mx <- filter(timmings,currency=='MXN')
timmings.mx <- filter(timmings.mx,0 < visit2quote)
timmings.mx <- filter(timmings.mx,0 < ordvisit2ord)
timmings.mx <- filter(timmings.mx,0 < ord2project)
timmings.mx <- filter(timmings.mx,0 < paid2project)
timmings.mx <- select(timmings.mx, visit2quote:paid2project)
timmings.melt <- melt(timmings.mx)

theme_set(theme_classic())
g <- ggplot(timmings.melt, aes(x=variable,y=log(value+1))) 
g <- g + geom_boxplot(aes(fill=factor(variable)))
g <- g + labs(title='Timmins per ifix step',
              x='Ifix Step',
              y='Log(Pending time)')
plot(g)
