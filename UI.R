library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)



page_one <- tabPanel(
  "Course offering by schools",
  titlePanel("Controls"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons("radio",
                   label = h3("View by schools"),
                   choices = list(
                     "All" = "UW",
                     "College of Arts and Sciences" = "College of Arts and Sciences",
                     "College of Built Environments" = "College of Built Environments",
                     "College of Education" = "College of Education",
                     "College of Engineering" = "College of Engineering",
                     "College of the Environment" = "College of the Environment",
                     "Evans Sch of Public Policy/Gov" = "Evans Sch of Public Policy/Gov",
                     "Foster School of Business" = "Foster School of Business",
                     "Information School" = "Information School",
                     "Interdisciplinary Graduate Prg" = "Interdisciplinary Graduate Prg",
                     "Interschool/Intercollege Prog." = "Interschool/Intercollege Prog.",
                     "School of Dentistry" = "School of Dentistry",
                     "School of Law" = "School of Law",
                     "School of Medicine" = "School of Medicine",
                     "School of Nursing" = "School of Nursing",
                     "School of Public Health" = "School of Public Health",
                     "School of Social Work" = "School of Social Work",
                     "Undergraduate Academic Affairs" = "Undergraduate Academic Affairs",
                     "Others" = "NA"
                   ),
                   selected = "UW"
      ),
    ),
    mainPanel(
      h3("UW course visualization - by schools"),
      plotlyOutput(outputId = "gg", height = "700px", width = "1100px")
    )
  )
)

page_two <- tabPanel(
  "Grade Distribution",
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput("text", label = h3("Course"), value = "INFO 201"),
      p("Enter course code + course number, i.e. \"cse 154\". Please put white space in between.")
    ),
    mainPanel(
      h3("Grade Distribution Lookup"),
      plotlyOutput(outputId = "chart"),
      plotlyOutput(outputId = "course"),
      strong(textOutput(outputId = "message")),
      br(),
      br(),
      br()
    )
  )
)

# Setting ui 
page_three <- tabPanel(
  "The Best and the Worst",
  sidebarLayout(
    sidebarPanel(
      width = 2,
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

page_four <- tabPanel(
  "Course offering by departments/majors",
  titlePanel("Average GPA and class size for each course in the selected department/major"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "dept",
        label = "Choose department:",
        choices = dept_list
      ),
      selectInput(
        inputId = "level",
        label = "Choose course level:",
        choices = c("All", "100 level", "200 level", "300 level", "400 level",
                    "500 level", "600 level", "700 level")
      )
    ),
    mainPanel(
      plotlyOutput("scatter"), width = 10
    )
  )
)


my_ui <- navbarPage(
  "UW Courses & GPA",
  page_one,
  page_two,
  page_three,
  page_four
)