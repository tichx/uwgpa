ui <- navbarPage('Test App',id = "inTabset",
                 tabPanel(title = "Panel 1", value = "panel1", 
                          actionButton('jumpToP2', 'Jump to Second Tab')),
                 tabPanel(title = "Panel 2", value = "panel2", 
                          actionButton('jumpToP1', 'Jump to First Tab'))
)

server <- function(input, output, session) {
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
}

shinyApp(ui, server)
