
library(shiny)

ui <- fluidPage("Hola, Mundo")

server <- function(input,output) {}

shinyApp(ui=ui,server = server)


library(googleComputeEngineR)
vm <- gce_vm(template = "rstudio",
             name = "my-rstudio",
             username = "carlos_ramirez", password = "mark1234",
             predefined_type = "n1-highmem-2")