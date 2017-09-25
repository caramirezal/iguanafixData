
tiemposEspera <- read.csv("~/GitHub/iguanafixData/data/tiempos_espera.csv")

jpeg("~/GitHub/iguanafixData/figures/minsToFirstCall.png")
with(tiemposEspera, 
     hist(sales_to_contact[sales_to_contact<200],
                         breaks = 1000,
                         col="black",
                          main="Mins to first call",
                          xlab="Time in minutes"))
abline(v=mean(tiemposEspera$sales_to_contact),col="red")
mtext(text = "mean = 118.85",at = c(mean(tiemposEspera$sales_to_contact),3500))
dev.off()

mean(tiemposEspera$sales_to_contact)
