library("dplyr")
library("ggplot2")
library(shiny)
library(plotly)

# import the data frame
df <- read.csv(file = "../data/uw_courses.csv", stringsAsFactors = FALSE)


# filter and return the tageting data frame 
filter_data <- function(data, depart) {
  fixed_data <- data %>%
    replace(. == "NULL", 0) %>%      
    select(dept_abbrev, course_no, Fail, W, student_count)
  
  if (depart != "ALL") {
    fixed_data <- fixed_data %>%
      filter(dept_abbrev == depart)
  } 
  
  fixed_data$W <- as.numeric(as.character(fixed_data$W))
  fixed_data$Fail <- as.numeric(as.character(fixed_data$Fail))
  fixed_data$student_count <- as.numeric(as.character(fixed_data$student_count))
  
  return(fixed_data)
}




# caculate the Withdraw and Fail rate for the course
caculate <- function(info_data, sam_num) {
  courses <- info_data %>%
    group_by(dept_abbrev, course_no) %>%
    summarise(
      student_num = sum(student_count),
      total_fail_num = sum(Fail),
      total_w_num = sum(W)
    ) %>%
    ungroup() %>%
    mutate(course = paste(dept_abbrev, course_no)) %>%
    group_by(course) %>%
    mutate(total_w_fail = sum(total_fail_num, total_w_num, na.rm = T)) %>%
    select(course, total_w_fail, student_num) %>%
    summarise(
      fail_rate = round(total_w_fail / student_num, digits = 3) * 100
    ) %>%
    arrange(-fail_rate) %>%
    head(sam_num)
  return(courses)
}


# Setting server
server <- function(input, output) {
  
  # plot the chart
  output$fail_plot <- renderPlot({
    info_data <- filter_data(df, input$department)
    final_data <- caculate(info_data, input$sample_num)
    ggplot(final_data) +
      geom_col(mapping = aes(
        x = reorder(course, fail_rate),
        y = fail_rate, fill = fail_rate
      )) +
      coord_flip() +
      labs(
        title = "Course fails / withdraw rate",
        x = "Course name",
        y = "Percentage of fails/withdraw students %",
        fill = "Fail / Drop rate"
      )
  })
}


# additional function pulling the course titles 
courses_titles <- function(data) {
  titles <- data %>%
    distinct(dept_abbrev) %>%
    pull(dept_abbrev)
}


# Setting ui 
page_one <- tabPanel(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "department",
        label = "Course title",
        choices = c(courses_titles(df))
      ),
      numericInput(
        inputId = "sample_num",
        label = "Numer of samples",
        value = 10,
        min = 1,
        max = NA
      )
    ), 
    mainPanel(
      plotOutput(outputId = "fail_plot")
    )
  )
)

ui <- navbarPage(
  "Function2",
  page_one
)


# Running shiny
shinyApp(ui = ui, server = server)