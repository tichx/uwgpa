library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)
library(shinythemes)


front_page <- tabPanel(
  "Overview",
  mainPanel(
    HTML(
      '<div>',
      '<img src="uw.png" align="top" width="291.6" height="196.344">',
    '<h2 class="display-3">About University of Washington GPA Analysis</h2>',
    '<p class="lead">This is a set of tools and data that helps you explore the
    history of student performance of courses offered from Autumn 2010 to
    Autumn 2018 at University of Washington</p>',
    '<hr class="my-4">',
    '<p class="lead">A bit of background</p>',
    '<p>Lots of students go to sites like ratemyprofessor.com or UW’s own
    course evaluation site to learn more about their courses before they commit.
    Often time they also ask recommendations from friends and peers. However,
    the information they can reach is often made up of single data points and
    thus is limited and subjective.</p>',
    '<p class="lead"><br>Objectives</p>',
    '<p>We aim to eliminate the scarcity of course data by providing you with
    visualizations and filters to help you get done your measurement of course
    difficulty.</p>',
    '<p>In addition, we would love to help you make better decisions at
    course registration, so we cleaned, aggregated, and analyzed course data across
    years to expose you to the most accurate student performance history in your
    courses.</p>',
    '<p class="lead"><br>What questions can be answered?</p>',
    '<p>As mentioned above, we hope to help you make an accurate assessment of
    course difficulty. We conjectured that the grade/GPA distribution,
    including the distribution/percentage of failure and 4.0 grades, serves
    as a good reflection of the difficulty level. Therefore, say if you want
    to learn about the difficulty of a specific course, you can go to the
    "GPA by course" and "Best & worst class" tabs.</p>',
    '<p>Despite this, we add and make two more dimensions of our GPA data
    available - the average GPA/4.0 GPA by “school” and “department”
    respectively - to get you informed of the trend or outliers of
    difficulties, in terms of grades,  across courses within your concerned
    school or department. If you have a major, great job! They can help you
    plan ahead and register for class wisely as you continue to take many
    upper-level classes; If you don’t have a major yet, no worries! you can
    utilize both as comparison tools to get insights of student performance
    across different schools/departments and then intend for the one that
    best fits you!</p>',
    '<p class="lead"><br>Our data</p>',
    '<p>We obtained a set of data from UW public record office by email
    request.The office then gave us a total of 9 files. The data from the
    nine files each represents different information regarding student
    course average GPA distribution, enrollments, course evaluations, and etc.
    We put all the data from 9 file into 1 file - “uw_courses.csv” through R.
    After that, we processed the raw data, and extrapolated the new file which
    is going to be the only file that we will use moving forward to reduce
    complexity. We also ensured that all of our data are FERPA safe.</p>',
    '<br><br>',
    '</div>'
    )
  )
)

page_one <- tabPanel(
  theme = shinytheme("united"),
  "GPA by college",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons("radio",
                   label = h3("View college"),
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
      h3("View average GPA by college"),
      plotlyOutput(outputId = "gg", height = "750px", width = "1200px")
    )
  )
)

page_two <- tabPanel(
  "GPA by department",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "dept",
        label = "Choose department",
        choices = c("Accounting", "Aeronautics & Astronautics", "Aerospace Studies", 
                    "American Ethnic Studies", "American Indian Studies", "Anthropology", 
                    "Applied Mathematics", "Architecture", "Art", "Art History", 
                    "Arts and Sciences Dean's Ofc.", "Asian American Studies", "Asian Language and Literature", 
                    "Astronomy", "Atmospheric Sciences", "Biochemistry", "Bioengineering", 
                    "Bioethics & Humanities", "Biology", "Biomedical Informatics and Medical Education", 
                    "Biostatistics", "Built Environments", "Business Administration", 
                    "Chemical Engineering", "Chemistry", "Chicano Studies", "Civil & Environmental Engineering", 
                    "Civil Engineering", "Classics", "College of Education", "College of Environment", 
                    "Communication", "Comp. Literature,Cinema & Media Studies", "Comparative History of Ideas", 
                    "Computer Science & Engineering", "Conjoint", "Construction Management", 
                    "CSE Professional Program", "Ctr for Digital Arts & Experimental Media", 
                    "Ctr. For Quantitative Sciences", "Ctr. Statistics & Social Sciences", 
                    "Dance", "Drama", "Early Childhood & Family Studies", "Earth and Space Sciences", 
                    "Economics", "Educ. Leadership & Policy Studies", "Educational Curriculum & Instruction", 
                    "Educational Psychology", "Electrical Engineering", "Engineering", 
                    "English", "Environmental Health", "Epidemiology", "Evans Sch of Public Policy/Gov", 
                    "Executive Master Bus Admin Group", "Finance & Business Economics", 
                    "French & Italian Studies", "Gender, Women, & Sexuality Studies", 
                    "General Studies", "Genome Sciences", "Geography", "Germanic Languages", 
                    "Global Health", "Health Services", "History - General", "Human Centered Design & Engr.", 
                    "Immunology", "Industrial & Systems Engineering", "Infant and Early Childhood Mental Health", 
                    "Info. Systems & Operations Mgmt.", "Information School", "Integrated Sciences", 
                    "Integrated Social Sciences", "Interdisciplinary Graduate Program", 
                    "Intr Global Innovation Exchange", "Jackson School International Studies", 
                    "Landscape Architecture", "Law, Societies, and Justice", "Linguistics", 
                    "Management & Organization", "Marketing & International Business", 
                    "Materials Science & Engineering", "Mathematics", "Mechanical Engineering", 
                    "Microbiology", "Music", "Naval Science", "Near East Language & Civilization", 
                    "Neurobiology", "Neurobiology & Behavior", "Nursing - NSG", "Nursing - NURS", 
                    "Nutritional Science", "Oral Health Sciences", "Orthodontics", 
                    "Pathobiology", "Pediatric Dentistry", "Periodontics", "Philosophy", 
                    "Physics", "Political Science", "Program on the Environment", 
                    "Psychology", "Public Health Genetics", "Real Estate", "Rehabilitation Medicine", 
                    "Scandinavian Studies", "Sch. of Aquatic & Fishery Sciences", 
                    "School of Environmental and Forest Sciences", "School of Law", 
                    "School of Marine & Envir. Affairs", "School of Nursing - Clinical", 
                    "School of Nursing - Methods", "School of Oceanography", "School of Pharmacy", 
                    "School of Public Health", "Slavic Language & Literature", "Social Work", 
                    "Sociology", "Spanish & Portuguese Studies", "Special Education", 
                    "Speech & Hearing Sciences", "Statistics", "Supply Chain Management", 
                    "Teacher Education", "Undergraduate Interdisciplinary Program", 
                    "University Conjoint", "Urban Design and Planning"),
        selected = "Information School"
      ),
      selectInput(
        inputId = "level",
        label = "Choose course level",
        choices = c("All", "100 level", "200 level", "300 level", "400 level",
                    "500 level", "600 level", "700 level")
      )
    ),
    mainPanel(
      h3("View average GPA by department"),
      plotlyOutput("scatter"), width = 10
    )
  )
)

page_three <- tabPanel(
  "GPA by course",
  sidebarLayout(
    sidebarPanel(
      label = h3("Lookup a course"),
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
page_four <- tabPanel(
  "Best & worst class",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "department",
        label = "From department",
        choices = c("ALL", "A A", "A S", "AAS", "ACADEM", "ACCTG", "AES", "AFRAM", "AIS", "AMATH", "ANTH", "ARAB", 
                    "ARCH", "ARCHY", "ART","ART H","ASIAN", "ASL", "ASTR", "ATM S", "B A", "B BIO", "B BUS",
                    "B CHIN", "B CMU", "B CUSP", "B ECON", "B EDUC", "B EE", "B H", "B HLTH","B NURS", "B SPAN",
                    "BCULST", "BES", "BIO A", "BIOC", "BIOEN", "BIOL", "BIOST", "BIS", "BISAMS", "BISCLA", "BISIA",
                    "BISMCS", "BISSEB", "BISSTS", "BPOLST", "BSE", "BST", "C LIT", "CEE", "CEP", "CFR", "CHEM",
                    "CHEM E", "CHID", "CHIN", "CHSTU","CL AR", "CLAS", "CM", "COM", "CONJ", "CS&SS", "CSE", "CSE P",
                    "CSS", "DANCE", "DENT", "DPHS", "DRAMA", "DXARTS", "E E", "ECFS", "ECON", "EDC&I", "EDLPS", "EDPSY",
                    "EDSPE", "EDTEP", "ELCBUS", "EMBA", "ENDO", "ENGL", "ENGR", "ENTRE", "ENV H", "ENVIR", "EPI", "ESRM",
                    "ESS", "EURO", "FIN", "FISH", "FRENCH", "G H", "GEN ST", "GENOME", "GEOG", "GERMAN", "GIS", "GREEK",
                    "GTTL", "HCDE", "HEBR", "HIHIM", "HINDI", "HIST", "HONORS", "HSERV", "HSMGMT", "HSTAA", "HSTAM", "HSTAS",
                    "HSTEU", "I BUS", "I S", "IMMUN", "IMT", "IND E", "INFO", "INFX", "ITAL", "JAPAN", "KOREAN", "L ARCH",
                    "LAB M", "LATIN", "LAW B", "LAW E", "LAW P", "LAW T", "LING", "LIS", "LSJ", "M E", "M SCI", "MATH", "MCB",
                    "MEDCH", "MEDEX", "MGMT", "MICROM", "MKTG", "MSE", "MUHST", "MUSAP", "MUSEN", "MUSEUM", "MUSIC", "N SCI",
                    "NBIO", "NCLIN", "NEAR E", "NEUBEH", "NME", "NMETH", "NORW", "NSG", "NURS", "NUTR", "O S", "OCEAN", "OPMGT",
                    "ORALB", "ORALM", "ORTHO", "P BIO", "PA EX", "PB AF", "PCEUT", "PEDO", "PERIO", "PHARM", "PHCOL", "PHIL",
                    "PHRMRA", "PHYS", "POL S", "POLSH", "PORT", "PROS", "PRSAN", "PSYCH", "Q SCI", "QMETH", "REHAB", "RELIG",
                    "RES D", "RUSS", "SCAND", "SIS", "SISA", "SISEA", "SISJE", "SISSE", "SLAV", "SMA", "SOC", "SOC W", "SOC WF",
                    "SPAN", "SPHSC", "STAT", "SWA", "SWED", "T ACCT", "T ARTS", "T BGEN", "T BUS", "T CORE", "T CRIM", "T EDSP",
                    "T EDUC", "T FILM", "T FIN", "T GH", "T GIS", "T HISP", "T HIST", "T HLTH", "T INFO", "T INST", "T LIT", "T MGMT",
                    "T MKTG", "T NURS", "T PHIL", "T SOC", "T SOCW", "T URB", "T WOMN", "TAGLG", "TBECON", "TCES", "TCHIN", "TCOM",
                    "TCSS", "TECON", "TEDADM", "TESC", "THAI", "THLEAD", "TIAS", "TKISH", "TMATH", "TMMBA", "TNPRFT", "TPOL S", 
                    "TPSYCH", "TRELIG", "TSOCWF","TWRT","UCONJ", "URBDP", "URDU", "VIET", "WOMEN", "ASTBIO", "B STR", "BISCP",
                    "BISGST", "EDUC", "FINN", "INSC", "LAW A", "MUSED", "PATH", "PHG", "SISLA",  "SISRE", "T EDSS", "T GEOG",
                    "TEST", "TSPAN", "B ART", "LIT", "SISME", "T SUD", "MSIS", "T ANTH", "B CHEM", "B PHYS", "C ENV", "GWSS",
                    "IPM", "PHARBE", "R E", "BISLEP", "SMEA", "HUM", "BCWRIT", "DESIGN", "JSIS", "JSIS A", "JSIS B", "LEDE", "SEFS",
                    "STMATH", "BCS",  "MOLMED", "SPH", "TCULTR", "VALUES", "DIS ST", "GEMBA", "T EDLD", "A E", "B IMD", "BHS", "BJAPAN",
                    "HCID", "JSIS C", "MEBI", "PHARMP",  "SCTL", "T AMST", "CFRM", "ARCTIC", "B ARTS", "B CORE", "B ENGR", "B MATH", "B ME",
                    "B WRIT", "BCONSC", "BIME", "HSTAFM", "HSTCMP", "HSTLAC", "HSTRY", "INTSCI", "ISS", "LAW", "T CSL", "B ACCT", "EGYPT",
                    "NEURO", "T EGL", "FHL", "JSIS E", "T UNIV", "B LEAD", "DENTCL", "B E",   "DENTFN", "DENTPC", "B CLIM", "BISGWS", "SLAVIC",
                    "PABIO", "DENTGP", "SCM", "ARTS", "ARTSCI", "BIBHEB", "CMS", "DATA", "PUBPOL", "T LAW", "TBIOMD", "CESI",
                    "CSE M", "GEEZ", "BISAES", "CESG", "CET", "CEWA", "JEW ST", "MOLENG", "PSYCAP", "SOC WL", "T BIOL", "T CHEM", "T GEOS",
                    "T LAX", "T PHYS", "T UDE", "TBANLT", "TCMP", "TECHIN", "TEE", "B BECN", "OHS", "PSYCLN", "COMMLD", "EE P", "IECMH"),
        selected = "INFO"
      ),
      numericInput(
        inputId = "sample_num",
        label = "Show top N",
        value = 10,
        min = 1,
        max = NA
      )
    ), 
    mainPanel(
      h3("Highest failure/4.0 rate from a department"),
      plotOutput(outputId = "fail_plot"),
      plotOutput(outputId = "a_plot")
    )
  )
)


my_ui <- navbarPage(
  theme = shinytheme("united"),
  "University of Washington GPA Analysis",
  front_page,
  page_one,
  page_two,
  page_three,
  page_four
)