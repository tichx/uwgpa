library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(shiny)


df <- read.csv("../data/uw_courses.csv", stringsAsFactors = F)


get_chart_text <- function(df, class) {
  new_df <- df %>%
    mutate(course = paste(dept_abbrev, course_no)) %>%
    filter(course == toupper(class)) %>%
    group_by(course) %>%
    summarize(
      sections = n(),
      title = course_title[1],
      total_enrolled = sum(as.numeric(student_count), na.rm = T),
      avg_gpa = round(sum(as.numeric(avg_gpa) * as.numeric(student_count), na.rm = T) / total_enrolled, 2),
      A_perc = round(sum(as.numeric(A) / total_enrolled) * 100, 1)
    )
  text <- paste0(new_df$course, ": ", new_df$title, " had ", new_df$total_enrolled, " students enrolled to ", new_df$sections, " section(s) in the past eight years. The class had an average GPA of ", new_df$avg_gpa, ", and ", new_df$A_perc, "% received a grade of 3.9/4.0.")
  text
}

plot_chart <- function(df, class) {
  new_df <- df %>%
    mutate(course = paste(dept_abbrev, course_no)) %>%
    filter(course == toupper(class)) %>%
    group_by(course) %>%
    summarize(
      A = sum(as.numeric(A), na.rm = T),
      Aminus = sum(as.numeric(Aminus), na.rm = T),
      Bplus = sum(as.numeric(Bplus), na.rm = T),
      B = sum(as.numeric(B), na.rm = T),
      Bminus = sum(as.numeric(Bminus), na.rm = T),
      Cplus = sum(as.numeric(Cplus), na.rm = T),
      C = sum(as.numeric(C), na.rm = T),
      Cminus = sum(as.numeric(Cminus), na.rm = T),
      Dplus = sum(as.numeric(Dplus), na.rm = T),
      D = sum(as.numeric(D), na.rm = T),
      Dminus = sum(as.numeric(Dminus), na.rm = T),
      Fail = sum(as.numeric(Fail), na.rm = T),
      W = sum(as.numeric(W), na.rm = T)
    )
  new_df <- within(new_df, rm("course"))
  label <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F", "Withdrawl")
  grade <- as.numeric(new_df[1, ])
  perc <- round(grade / sum(grade) * 100, 1)
  p <- plot_ly(
    x = label,
    y = grade,
    type = "bar",
    hovertemplate = paste0(perc, "% students received ", label)
  ) %>%
    layout(
      title = paste("Grade distribution for", class),
      xaxis = list(title = "Grade"),
      yaxis = list(title = "# of students")
    )
  p
}



plot_graph <- function(df, school) {
  df <- df %>%
    mutate(code = paste(dept_abbrev, course_no)) %>%
    mutate(gp = as.numeric(avg_gpa) * as.numeric(student_count))
  if (school != "UW") {
    df <- df %>%
      filter(college_text == school)
  }
  df <- df %>%
    group_by(code) %>%
    summarize(
      sections = n(),
      college = college_text[1],
      sub_college = sub_college_text[1],
      dept = department_text[1],
      abbr = dept_abbrev[1],
      no = course_no[1],
      title = course_title[1],
      student = sum(as.numeric(student_count), na.rm = T),
      A = sum(as.numeric(A), na.rm = T),
      tgp = sum(gp, na.rm = T),
      avg = round(tgp / student, 2),
      A_perc = round(A / student, 3) * 100
    )


  plot_ly(
    data = df,
    x = ~A_perc,
    y = ~avg,
    type = "scatter",
    mode = "markers",
    hovertemplate = paste(
      df$sub_college,
      "<br>", df$code, df$title,
      "<br>", df$sections, " section(s),", df$student, "students",
      "<br>", df$A_perc, "%", "received 4.0", "average GPA", df$avg
    ),
    marker = list(size = sqrt(df$student)),
    color = ~college
  ) %>%
    layout(
      title = paste0("Every course at ", school, ", by GPA"),
      xaxis = list(title = "Percentage % of 4.0 given (A)"),
      yaxis = list(title = "Average GPA")
    )
}


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
      plotlyOutput(outputId = "gg")
    )
  )
)

page_two <- tabPanel(
  "Grade Distribution",

  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput("text", label = h3("Course"), value = "INFO 201")
    ),
    mainPanel(
      h3("Grade Distribution Lookup"),
      p("This chart shows grade distribution for every course offered at the UW for the past 8 years. Enter the course code on the left to search."),
      plotlyOutput(outputId = "chart"),
      textOutput(outputId = "message")
    )
  )
)

my_ui <- navbarPage(
  "UW Courses & GPA",
  page_one,
  page_two
)
my_server <- function(input, output) {
  output$gg <- renderPlotly(plot_graph(df, input$radio))
  output$chart <- renderPlotly(plot_chart(df, input$text))
  output$message <- renderText(get_chart_text(df, input$text))
}

shinyApp(ui = my_ui, server = my_server)
