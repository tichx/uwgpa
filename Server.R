library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)




df <- read.csv("data/uw_courses.csv", stringsAsFactors = F)




simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
}


big_search <- function(data, last, first, class) {
  new_df <- df %>%
    select(year, term, dept_abbrev, course_no, course_title, student_count, A, Aminus, avg_gpa, professor_rating, lastname, firstname) %>%
    mutate(class_code = paste(dept_abbrev, course_no))
  if (last != "") {
    new_df <- new_df %>%
      filter(lastname == simpleCap(tolower(last)))
  }
  if (first != "") {
    new_df <- new_df %>%
      filter(firstname == simpleCap(tolower(first)))
  }
  if (class != "") {
    new_df <- new_df %>%
      filter(class_code == toupper(class))
  }
  new_df <- new_df %>%
    mutate(
      A_perc = round(as.numeric(A) / as.numeric(student_count) * 100, 1),
      Am_perc = round(as.numeric(Aminus) / as.numeric(student_count) * 100, 1)
    )
  new_df
}


t <- big_search(df, "reges", "stuart", "cse 143")


plot_course <- function(df, class) {
  new_df <- df %>%
    mutate(course_code = gsub(" ", "", paste(dept_abbrev, course_no), fixed = TRUE)) %>%
    mutate(title = paste0(course_code, ": ", course_title)) %>%
    mutate(quarter = paste0(year, term)) %>%
    filter(course_code == toupper(gsub(" ", "", class, fixed = TRUE)))
  new_df$firstname[is.na(new_df$firstname)] <- "Prof."
  new_df$lastname[is.na(new_df$lastname)] <- "Unknown"
  new_df <- new_df %>%
    mutate(name = paste0(lastname, ", ", firstname)) %>%
    mutate(course_code = paste(dept_abbrev, course_no)) %>%
    group_by(name, course_code, quarter) %>%
    summarise(
      enrolled = sum(as.numeric(student_count), na.rm = T),
      grade = round(sum(as.numeric(avg_gpa) * as.numeric(student_count), na.rm = T) / enrolled, 2),
      course_title = title[1]
    )
  p <- plot_ly(
    data = new_df,
    x = ~enrolled,
    y = ~grade,
    type = "scatter",
    mode = "markers",
    hovertemplate = paste(
      new_df$quarter,
      "<br>", "Enrolled: ", new_df$enrolled,
      "<br>", "GPA: ", new_df$grade,
      "<br>", "Professor: ", new_df$name
    ),
    marker = list(size = sqrt(new_df$enrolled) * 4, line = list(color = "rgba(220,220,220, .8)", width = 2)),
    color = ~name,
    text = ~quarter,
    textposition = "middle center",
    textfont = list(color = "#fffff", size = 8)
  ) %>%
    layout(
      xaxis = list(title = "Students Enrolled"),
      yaxis = list(title = "Average GPA")
    )
  p
}

plot_course(df, "e e477")

get_chart_text <- function(df, class) {
  new_df <- df %>%
    mutate(course = paste(dept_abbrev, course_no)) %>%
    mutate(course_text_match = gsub(" ", "", paste(dept_abbrev, course_no), fixed = TRUE)) %>% 
    filter(course_text_match == gsub(" ", "", toupper(class), fixed = TRUE)) %>%
    group_by(course) %>%
    summarize(
      sections = n(),
      title = course_title[1],
      total_enrolled = sum(as.numeric(student_count), na.rm = T),
      avg_gpa = round(sum(as.numeric(avg_gpa) * as.numeric(student_count), na.rm = T) / total_enrolled, 2),
      A_perc = round(sum(as.numeric(A) / total_enrolled, na.rm = T) * 100, 1)
    )
  # text <- paste0(new_df$course, ": ", new_df$title, " had ", new_df$total_enrolled, " students enrolled to ", new_df$sections, " section(s) in the past eight years. The class had an average GPA of ", new_df$avg_gpa, ", and ", new_df$A_perc, "% received a grade of 3.9/4.0.")
  new_df
}

plot_chart <- function(df, class) {
  new_df <- df %>%
    mutate(course = paste(dept_abbrev, course_no)) %>%
    mutate(course_search_match = gsub(" ", "", paste(dept_abbrev, course_no), fixed = TRUE)) %>% 
    filter(course_search_match == gsub(" ", "", toupper(class), fixed = TRUE)) %>%
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
  xform <- list(
    categoryorder = "array",
    categoryarray = label
  )
  grade <- as.numeric(new_df[1, ])
  perc <- round(grade / sum(grade) * 100, 1)
  p <- plot_ly(
    x = label,
    y = grade,
    type = "bar",
    text = paste0(perc, "%"),
    textposition = "auto",
    hovertemplate = paste0(perc, "% students received ", label)
  ) %>%
    layout(
      title = paste("Grade distribution for", class),
      xaxis = xform,
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
      "<b>",
      df$sub_college,
      "<br>", df$code, df$title,
      "</b><br><br>", df$sections, "section(s),", df$student, "students",
      "<br>", df$A_perc, "%", "received 4.0<br>", "GPA average:", df$avg
    ),
    marker = list(size = sqrt(df$student) / 2),
    color = ~college
  ) %>%
    layout(
      title = paste0("Every course at ", school, ", by GPA"),
      xaxis = list(title = "Percentage % of 4.0 given (A)"),
      yaxis = list(title = "Average GPA"),
      showlegend = FALSE
    )
}


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


final_data_a <- function(data, num) {
  data1 <- data %>%
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
    head(num)
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
    select(
      course_level, dept_abbrev, course_id, course_title,
      first_offered, last_offered
    ))

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
    top_n(3, wt = count) %>%
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
          "<br><b>", combined_dept_df[["course_id"]],
          ": ", combined_dept_df[["course_title"]],
          "</b><br><br>Class size:", combined_dept_df[["avg_student"]], "students on average",
          "<br>GPA:", combined_dept_df[["avg_gpa"]], "on average",
          "<br>First offered:", combined_dept_df[["first_offered"]]
        ),
        paste(
          "<br><b>", combined_dept_df[["course_id"]],
          ": ", combined_dept_df[["course_title"]],
          "</b><br><br>Class size:", combined_dept_df[["avg_student"]], "students on average",
          "<br>GPA:", combined_dept_df[["avg_gpa"]], "on average",
          "<br>First offered:", combined_dept_df[["first_offered"]],
          "<br>Last offered:", combined_dept_df[["last_offered"]]
        )
      ),
      marker = list(size = combined_dept_df[["avg_student"]] / 2),
      color = ~course_level
    ) %>%
      layout(
        title = paste0("Average GPAs for ", input_dept, " Courses"),
        xaxis = list(title = "Course ID"),
        yaxis = list(title = "Average GPA"),
        autosize = F, height = 825, width = 1200, margin = list(l = 50, r = 50, b = 125, t = 100, pad = 4),
        showlegend = F,
        plot_bgcolor = "rgb(246, 246, 246)",
        annotations = list(
          x = 0.0075, y = 1.05,
          text = paste0(combined_dept_df[["dept_abbrev"]], " course"),
          showarrow = F, xref = "paper", yref = "paper",
          font = list(size = 13), xanchor = "middle",
          yanchor = "top"
        )
      )
  })
  ### In some cases plots is NULL! Better solution?
  if (length(plots) > 0) {
    subplot(plots,
      nrows = length(plots), shareX = F, titleY = F,
      margin = c(0.001, 0.001, 0.08, 0.08)
    )
  }
}


get_ui <- function(dataframe) {
  tagList(
    h6(paste0(dataframe$course, ": ", dataframe$title)),
    p("Average GPA", h2(style = "color:#E95420; padding-top:0px;margin-top:0px", dataframe$avg_gpa)),
    p("Received 4.0", h2(style = "color:#E95420; padding-top:0px;margin-top:0px", paste0(dataframe$A_perc, "%"))),
    h6(em(paste0("from ", dataframe$sections, " section(s), ", dataframe$total_enrolled, " students")))
  )
}


# main server call
my_server <- function(input, output, session) {
  output$gg <- renderPlotly(plot_graph(df, input$radio))
  output$chart <- renderPlotly(plot_chart(df, input$search_course_name))
  # output$message <- renderTable(get_chart_text(df, input$text))

  output$textui <- renderUI(get_ui(get_chart_text(df, input$search_course_name)))
  output$course <- renderPlotly(plot_course(df, input$search_course_name))
  output$scatter <- renderPlotly(avg_gpa_chart(df, input$dept, input$level))
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
    ggplot(final_data, aes(x = `className`, y = a_rate, label = paste0(a_rate, "%"))) +
      geom_point(stat = "identity", aes(col = className), size = 15) +
      geom_text(color = "white", size = 4) +
      labs(
        title = "Percentage of students receiving A (4.0/3.9)",
        x = "Course name",
        y = "4.0 percentage",
        color = "Class name"
      ) +
      ylim(0, 100) +
      coord_flip()
  })
}
