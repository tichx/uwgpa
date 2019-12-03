library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)



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
      plotlyOutput(outputId = "gg", height = "700px", width = "1100px")
    )
  )
)

page_two <- tabPanel(
  "Grade Distribution",
  
  sidebarLayout(
    sidebarPanel(
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
page_three <- tabPanel(
  "The Best and the Worst",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "department",
        label = "Course title",
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
      ),
      numericInput(
        inputId = "sample_num",
        label = "Numer of samples",
        value = 10,
        min = 1,
        max = NA
      )
    ), 
    mainPanel(
      plotOutput(outputId = "fail_plot"),
      plotOutput(outputId = "a_plot")
    )
  )
)

page_four <- tabPanel(
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
  "UW Courses & GPA",
  page_one,
  page_two,
  page_three,
  page_four
)