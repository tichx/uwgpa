library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

source("UI.R")
source("Server.R")
source("global.R")

shinyApp(ui = my_ui, server = my_server)
