library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# import dataset
df <- read.csv(file = "../data/uw_courses.csv", stringsAsFactors = F)


sum_grade <- function(grade) {
  return(sum(as.numeric(grade), na.rm = TRUE))
}

plot_graph <- function(df, code) {
class <- df %>%
  mutate(gp = as.numeric(student_count) * as.numeric(avg_gpa)) %>% 
  mutate(class_name = paste(dept_abbrev,course_no))

class <- class %>% 
  filter(class_name == code) %>% 
  group_by(lastname) %>%
  summarize(
    firstname = firstname[1],
    student_count = sum_grade(student_count),
    section = n(),
    A = sum_grade(A),
    Am = sum_grade(Aminus),
    Bp = sum_grade(Bplus),
    B = sum_grade(B),
    Bm = sum_grade(Bminus),
    Cp = sum_grade(Cplus),
    C = sum_grade(C),
    Cm = sum_grade(Cminus),
    Dp = sum_grade(Dplus),
    D = sum_grade(D),
    Dm = sum_grade(Dminus),
    Failed = sum_grade(Fail),
    Withdrawl = sum_grade(W),
    grade_point_total = sum(gp),
    avg_gpa = round(grade_point_total / student_count, 2),
    a_perc = A / student_count
  )

class <- add_row(class,
  lastname = "All Sections", firstname = "",
  student_count = sum(class$student_count),
  section = sum(class$section),
  A = sum(class$A),
  Am = sum(class$Am),
  Bp = sum(class$Bp),
  B = sum(class$B),
  Bm = sum(class$Bm),
  Cp = sum(class$Cp),
  C = sum(class$C),
  Cm = sum(class$Cm),
  Dp = sum(class$Dp),
  D = sum(class$D),
  Dm = sum(class$Dm),
  Failed = sum(class$Failed),
  Withdrawl = sum(class$Withdrawl),
  grade_point_total = sum(class$grade_point_total),
  avg_gpa = round(grade_point_total / student_count, 2),
  a_perc = sum(class$a_perc)
)

plot_data <- class %>%
  select(
    lastname, A, Am, Bp, B, Bm, Cp, C, Cm, Dp, D, Dm, Failed, Withdrawl
  ) %>%
  gather(key = Grade, value = GPA, -lastname)
ggplot(plot_data) +
  geom_col(
    mapping = aes(x = lastname, y = GPA, fill = Grade),
    position = "fill"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 4)) +
  labs(
    title = paste("Grade disparity between", class_code ,"sections at UW"),
    subtitle = "Average GPA", x = "Prof. last name",
    y = "Grade Percentage", color = "Grade"
  ) +
  coord_flip()
}

plot_graph(df, "INFO 200")


