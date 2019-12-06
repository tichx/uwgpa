library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

source("ui.R")
source("server.R")
source("global.R")

shinyApp(ui = my_ui, server = my_server)
