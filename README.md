# University of Washington GPA Report

A link to the shiny app can be found [here](https://tichx.shinyapps.io/finalproj/)

# Welcome

We are glad to see you here! As fellow UW students, one of the most frustrating situations to find yourself in is a course where all of your friends are in the "easier section". For most of us, it feels like this happens __all of the time__. This data visualization study hopes to dismystify such perception. 

# About our Dataset

In January 2019, the University of Washington responded to a Freedom of Information Act request (FOIA #16-456 and others) for "the grade distributions by percent and/or letter grade, for every class [...] at the University of Washihngton at Seattle". This dataset, now available on github, contains the breakdown of GPA for every course at The University of Washington. As noted in the FOIA response, some data was withheld to maintain FERPA compliance. 

# About our audience

Our target audience would be other University of Washington students who are looking to assess the competitiveness of their classes. By using this tool "GPA by course," students can find open and accurate historical data on the past sections offered by every professor in the past eight years. By selecting colleges from "GPA by college," users can understand overall GPA trend in a specific school, like College of Engineering. We hope to bring transparency to the course planning process and help everyone makes informed decisions.

# Questions we want to answer

With our Shiny Web Application, we want to answer quesitions such as:

* What is the toughest course in the Information School?

* How many students have taken CSE 143 and how did they perform?

* Which class has the highest enrollment?

The tools are presented simply as a interactive scatter plot, with filter and text/plot analysis.


# Disclaimer

* This site is not endorsed by University of Washington Public Record Compliance Office.

* The data was obtained through public record available to everyone per request.

* No private information is used, including names, ratings, reviews that could potentially identify to person. All data was requested to be FERPA-safe.

* Accuracy of the information is relied on University of Washington Public Record Office. Our team is not liable to false information from the dataset.

# Technical Stuff

We used a Shiny App to document our investigation and analysis of the data. In most parts, we used ‘dplyr’, ‘ggplot2’, and ‘plotly’ to produce the outputs we wanted. 

# Team Members

* Nicholas Xu: tichx@uw.edu

* Yuheng Zhong: lzhong13@uw.edu

* Keyan Ding: dingk4@uw.edu

* Chloe Chen: liuk25@uw.edu

# Link to the data set we used
* https://github.com/tichx/uw_gpa_dataset

# Link to the examples we referenced
* http://waf.cs.illinois.edu/discovery/grade_disparity_between_sections_at_uiuc/
* http://waf.cs.illinois.edu/discovery/every_gen_ed_at_uiuc_by_gpa/
* http://waf.cs.illinois.edu/discovery/gpa_of_every_course_at_illinois/
* https://easy-a.net/
