#install.packages("shiny")

library("shiny")

source("ui.R")

source("server.R")

shinyApp(ui = my_ui, server = my_server)