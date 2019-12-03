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

filter_data_for_a <- function(data, name) {
  myclass <- data %>%
    replace(. == "NULL", 0) %>%
    select(dept_abbrev, course_no, A, student_count)
  
  if (name != "ALL") {
    myclass <- myclass %>%
      filter(dept_abbrev == name)
  } 
  
  myclass$A <- as.numeric(as.character(myclass$A))
  myclass$student_count <- as.numeric(as.character(myclass$student_count))
  myclass

}


filter_data_for_a(df, "ALL")


final_data_a <- function(data, num) {
  data1 <-  data %>%
    group_by(dept_abbrev, course_no) %>%
    summarise(
      total_student = sum(student_count), 
      a_student = sum(A),
    ) %>%
    ungroup() %>%
    mutate(className = paste(dept_abbrev, course_no)) %>%
    group_by(className) %>%
    select(className, total_student, a_student) %>%
    summarize(a_rate = round(a_student / total_student, digit = 3) * 100) %>%
    arrange(-a_rate) %>%
    head (num)
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
my_server <- function(input, output) {
  
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
  
  output$a_plot <- renderPlot({
    filtered_data <- filter_data_for_a(df, input$department)
    final_data <- final_data_a(filtered_data, input$sample_num)
    ggplot(final_data, aes(x=`className`, y=a_rate, label=paste0(a_rate, "%"))) + 
      geom_point(stat='identity', aes(col=className), size=6) +
      geom_text(color="white", size=2) + 
      labs(
          title="Percentage of students receiving A (4.0/3.9)", 
           x = "Course name",
           y = "4.0 percentage",
          color = "Class name"
        ) +
      ylim(0, 100) +
      coord_flip()
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
        choices = c("ALL", courses_titles(df))
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
      plotOutput(outputId = "fail_plot"),
      plotOutput(outputId = "a_plot")
    )
  )
)

my_ui <- navbarPage(
  "Function2",
  page_one
)

# Running shiny
shinyApp(ui = my_ui, server = my_server)