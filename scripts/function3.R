library(dplyr)
library(plotly)
library(shiny)

df <- read.csv("../data/uw_courses.csv", stringsAsFactors = F)

# Compute the vector of departments for ui
dept_list <- unique(df %>%
  select(department_text) %>%
    arrange(department_text))
# Remove NA values
dept_list <- dept_list[!is.na(dept_list)]

# function
avg_gpa_chart <- function(df, input_dept, input_level) {
  # Construct the general big dataframe of the selected major courses.
  dept_df <- df %>%
    filter(department_text == input_dept) %>%
    arrange(course_no) %>%
    mutate(
      course_id = paste0(dept_abbrev, " ", course_no),
      course_level = floor(course_no / 100) * 100,
      term = factor(term, levels = c("Autumn", "Winter", "Spring", "Summer")),
      term = as.character(term)
    )

  # The next two functions construct a specific dataframe of the dept
  # courses, with the first and last offering quarters included for each
  # course. The first and last offering quarters are information needed
  # for the interactive components of my chart.
  course_period_df <- unique(dept_df %>%
    group_by(course_id, course_title) %>%
    filter(row_number() == 1 | row_number() == n()) %>%
    mutate(term_year = paste0(term, " ", year)) %>%
    select(course_level, dept_abbrev, course_id, course_title, term_year))

  course_period_df <- unique(course_period_df %>%
    group_by(course_id, course_title) %>%
    mutate(
      first_offered = term_year[1],
      last_offered = term_year[2],
      course_level = paste0(as.character(course_level), " ", "level")
    ) %>%
    select(course_level, dept_abbrev, course_id, course_title,
           first_offered, last_offered))

  # This dataframe contains the average course grade and the average number of
  # enrolled students for each department course.
  course_grade_df <- dept_df %>%
    select(
      course_level, dept_abbrev, course_id, course_title,
      section_id, year, term, student_count, avg_gpa
    ) %>%
    mutate(course_level = paste0(as.character(course_level), " ", "level")) %>%
    group_by(course_level, dept_abbrev, course_id, course_title) %>%
    summarise(
      avg_gpa = round(mean(avg_gpa), 1),
      avg_student = round(mean(as.numeric(student_count)), 0)
    )

  # Combine course_grade_df and course_period_df by the columns "course_level",
  # "course_id", "course_title". Finally filter out specified level of courses.
  combined_dept_df <- left_join(course_grade_df, course_period_df,
    by = c("course_level", "dept_abbrev", "course_id", "course_title")
  )
  if (input_level != "All") {
    combined_dept_df <- filter(combined_dept_df, course_level == input_level)
  }
  
  # Due to the limited space of main panel, we only consider 
  # the 5 most popular programs from this department! 6 if
  # the 5th and 6th have the same popularity i.e. total num
  # of quarters offered are the same. e.g. Jackson School
  # of Interational Studies.
  course_title <- pull(combined_dept_df %>%
    ungroup() %>%
    group_by(dept_abbrev) %>%
    summarise(count = length(dept_abbrev)) %>%
    arrange(-count) %>%
    top_n(5, wt = count) %>%
    select(dept_abbrev))

  # Build an interactive chart that displays the average GPA for each
  # dept course. The radius of each circle represents the average size
  # of each course.
  plots <- lapply(course_title, function(course) {
    combined_dept_df <- filter(combined_dept_df, dept_abbrev == course)
    plot_ly(
      data = combined_dept_df,
      x = ~course_id,
      y = ~avg_gpa,
      type = "scatter",
      mode = "markers",
      name = ~dept_abbrev,
      hovertemplate = ifelse(is.na(combined_dept_df[["last_offered"]]),
        paste(
          "course id:", combined_dept_df[["course_id"]],
          "<br>course title:", combined_dept_df[["course_title"]],
          "<br>avg student:", combined_dept_df[["avg_student"]],
          "<br>avg gpa:", combined_dept_df[["avg_gpa"]],
          "<br>first offered:", combined_dept_df[["first_offered"]]
        ),
        paste(
          "course id:", combined_dept_df[["course_id"]],
          "<br>course title:", combined_dept_df[["course_title"]],
          "<br>avg student:", combined_dept_df[["avg_student"]],
          "<br>avg gpa:", combined_dept_df[["avg_gpa"]],
          "<br>first offered:", combined_dept_df[["first_offered"]],
          "<br>last offered:", combined_dept_df[["last_offered"]]
        )
      ),
      marker = list(size = combined_dept_df[["avg_student"]] / 3),
      color = ~course_level
    ) %>%
      layout(
        title = paste0("Average GPAs for ", input_dept, " Courses"),
        xaxis = list(title = "Course ID"),
        yaxis = list(title = "Average GPA"),
        autosize = F, width = 1200, height = 825, margin = list(l = 50, r = 50, b = 125, t = 100, pad = 4),
        showlegend = F,
        plot_bgcolor = "rgb(246, 246, 246)"
        #annotations = list(
        #  x = 0.0075, y = 1.55,
        #  text = paste0(combined_dept_df[["dept_abbrev"]], " courses"),
        #  showarrow = F, xref = "paper", yref = "paper",
        #  font = list(size = 13), xanchor = "middle",
        #  yanchor = "top"
        #)
      )
  })
  ### In some cases plots is NULL! Better solution?
  if (length(plots) > 0) {
    subplot(plots, nrows = length(plots), shareX = F, titleY = F,
            margin = c(0.001, 0.001, 0.08, 0.08))
  }
}

# chart2 <- avg_gpa_chart(df, "Information School", "200 level")




# ui
scatter_panel <- tabPanel(
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
  "UW avg GPA",
  scatter_panel
)




# server
my_server <- function(input, output) {
  output$scatter <- renderPlotly(avg_gpa_chart(df, input$dept, input$level))
}


shinyApp(ui = my_ui, server = my_server)
