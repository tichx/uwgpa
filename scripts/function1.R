library(dplyr)
library(tidyr)
library(plotly)


df <- read.csv("../data/uw_courses.csv", stringsAsFactors = F)

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

plot_graph(df, "College of the Environment")
