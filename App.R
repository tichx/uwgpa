library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

source("UI.R")
source("Server.R")

shinyApp(ui = my_ui, server = my_server)
