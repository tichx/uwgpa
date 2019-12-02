library("dplyr")
library("ggplot2")

# import the data frame
df <- read.csv(file = "./data/uw_courses.csv", stringsAsFactors = FALSE)

# filter and return the tageting data frame 
filtered_df <- function(data, depart) {
  fixed_data <- data %>%
    replace(. == "NULL", 0) %>%      
    select(dept_abbrev, course_no, Fail, W, student_count)
  
  if (depart != "ALL") {
    return_data <- fixed_data %>%
      filter(dept_abbrev == depart)
  } else {
    return_data <- fixed_data
  }
  
  return(return_data)
}

info_data <- filtered_df(df, "ALL")

info_data$W <- as.numeric(as.character(info_data$W))
info_data$Fail <- as.numeric(as.character(info_data$Fail))
info_data$student_count <- as.numeric(as.character(info_data$student_count))

# caculate the Withdraw and Fail rate for the course
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
  head(10)

# plot the chart
chart <- ggplot(courses) +
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
