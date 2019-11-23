library(dplyr)
library(tidyr)
library(plotly)
library(pracma)

df <- read.csv("../data/uw_courses.csv", stringsAsFactors = F)


d <- df %>% 
  mutate(code = paste(dept_abbrev, course_no)) %>% 
  mutate(gp = as.numeric(avg_gpa) * as.numeric(student_count))%>% 
  group_by(code)%>% 
  summarize(
    sections=n(),
    college=college_text[1],
    sub_college=sub_college_text[1],
    dept=department_text[1],
    abbr=dept_abbrev[1],
    no=course_no[1],
    title=course_title[1],
    student=sum(as.numeric(student_count), na.rm = T),
    A=sum(as.numeric(A), na.rm = T),
    tgp = sum(gp,na.rm = T),
    avg = round(tgp / student,2),
    A_perc = round(A/student,3) * 100)

plot_ly(
  data = d,
  x = ~A_perc,
  y = ~avg,
  type = "scatter",
  mode = "markers",
  hovertemplate = paste(d$sub_college, 
                        "<br>", d$code, d$title,
                        "<br>",d$sections," section(s),",d$student, "students",
                        "<br>", d$A_perc, "%","received 4.0", "average GPA",d$avg),
  marker = list(size = sqrt(d$student)),
  color = ~college
  ) %>%
  layout(
    title = "Every course at UW, by GPA",
    xaxis = list(title = "Percentage % of 4.0 given (A)"),
    yaxis = list(title = "Average GPA")
  )

sum(as.numeric(df$student_count),na.rm=T)

