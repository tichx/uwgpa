library(ggplot2)
library(dplyr)
library(tidyr)

#import data set
df <- read.csv("../data/uw_courses.csv", stringsAsFactors = F)
myfun <- function(name){
   myclass <- df %>%
   filter(dept_abbrev == name) %>%
   replace(. == "NULL", 0) %>%
   select(dept_abbrev, course_no, A, student_count)

   myclass$A <- as.numeric(as.character(myclass$A))
   myclass$student_count <- as.numeric(as.character(myclass$student_count))
   

#Calculation of the A Rate of all Psych classes

data1 <- myclass %>%
  group_by(dept_abbrev, course_no) %>%
  summarise(
    total_student = sum(student_count), 
    a_student = sum(A),
  ) %>%
  ungroup()%>%
  mutate(className = paste(dept_abbrev, course_no))%>%
  group_by(className) %>%
  select(className, total_student, a_student) %>%
  summarize(a_rate = round(a_student / total_student, digit = 3) * 100) %>%
  arrange(-a_rate)%>%
  head (20)

#Return a Plot

ggplot(data = data1, aes(x=className, y=a_rate)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
}

  