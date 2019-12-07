library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)




page_one <- tabPanel(
  "GPA for every school",
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
        selected = "College of Engineering"
      ),
    ),
    mainPanel(
      h3("View average GPA by college"),
      plotlyOutput(outputId = "gg", height = "750px")
    )
  )
)

page_two <- tabPanel(
  "GPA for every department",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "dept",
        label = "Choose department",
        choices = c(
          "Accounting", "Aeronautics & Astronautics", "Aerospace Studies",
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
          "University Conjoint", "Urban Design and Planning"
        ),
        selected = "Information School"
      ),
      selectInput(
        inputId = "level",
        label = "Choose course level",
        choices = c(
          "All", "100 level", "200 level", "300 level", "400 level",
          "500 level", "600 level", "700 level"
        )
      )
    ),
    mainPanel(
      h3("View average GPA by department"),
      plotlyOutput("scatter"),
      width = 10
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
        choices = c(
          "ALL", "A A", "A S", "AAS", "ACADEM", "ACCTG", "AES", "AFRAM", "AIS", "AMATH", "ANTH", "ARAB",
          "ARCH", "ARCHY", "ART", "ART H", "ASIAN", "ASL", "ASTR", "ATM S", "B A", "B BIO", "B BUS",
          "B CHIN", "B CMU", "B CUSP", "B ECON", "B EDUC", "B EE", "B H", "B HLTH", "B NURS", "B SPAN",
          "BCULST", "BES", "BIO A", "BIOC", "BIOEN", "BIOL", "BIOST", "BIS", "BISAMS", "BISCLA", "BISIA",
          "BISMCS", "BISSEB", "BISSTS", "BPOLST", "BSE", "BST", "C LIT", "CEE", "CEP", "CFR", "CHEM",
          "CHEM E", "CHID", "CHIN", "CHSTU", "CL AR", "CLAS", "CM", "COM", "CONJ", "CS&SS", "CSE", "CSE P",
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
          "TPSYCH", "TRELIG", "TSOCWF", "TWRT", "UCONJ", "URBDP", "URDU", "VIET", "WOMEN", "ASTBIO", "B STR", "BISCP",
          "BISGST", "EDUC", "FINN", "INSC", "LAW A", "MUSED", "PATH", "PHG", "SISLA", "SISRE", "T EDSS", "T GEOG",
          "TEST", "TSPAN", "B ART", "LIT", "SISME", "T SUD", "MSIS", "T ANTH", "B CHEM", "B PHYS", "C ENV", "GWSS",
          "IPM", "PHARBE", "R E", "BISLEP", "SMEA", "HUM", "BCWRIT", "DESIGN", "JSIS", "JSIS A", "JSIS B", "LEDE", "SEFS",
          "STMATH", "BCS", "MOLMED", "SPH", "TCULTR", "VALUES", "DIS ST", "GEMBA", "T EDLD", "A E", "B IMD", "BHS", "BJAPAN",
          "HCID", "JSIS C", "MEBI", "PHARMP", "SCTL", "T AMST", "CFRM", "ARCTIC", "B ARTS", "B CORE", "B ENGR", "B MATH", "B ME",
          "B WRIT", "BCONSC", "BIME", "HSTAFM", "HSTCMP", "HSTLAC", "HSTRY", "INTSCI", "ISS", "LAW", "T CSL", "B ACCT", "EGYPT",
          "NEURO", "T EGL", "FHL", "JSIS E", "T UNIV", "B LEAD", "DENTCL", "B E", "DENTFN", "DENTPC", "B CLIM", "BISGWS", "SLAVIC",
          "PABIO", "DENTGP", "SCM", "ARTS", "ARTSCI", "BIBHEB", "CMS", "DATA", "PUBPOL", "T LAW", "TBIOMD", "CESI",
          "CSE M", "GEEZ", "BISAES", "CESG", "CET", "CEWA", "JEW ST", "MOLENG", "PSYCAP", "SOC WL", "T BIOL", "T CHEM", "T GEOS",
          "T LAX", "T PHYS", "T UDE", "TBANLT", "TCMP", "TECHIN", "TEE", "B BECN", "OHS", "PSYCLN", "COMMLD", "EE P", "IECMH"
        ),
        selected = "INFO"
      ),
      numericInput(
        inputId = "sample_num",
        label = "Show top N",
        value = 15,
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


summary <- tabPanel(
  "Our findings",
  includeHTML("summary.html")
)

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(
      id = inputId,
      class = "mobile-element",
      type = "hidden"
    )
  )
}


search_page <- tabPanel(
  "Search a course",
  includeHTML("search-header.html"),
  br(),
  sidebarLayout(
    sidebarPanel(
      label = h3("Lookup a course"),
      width = 3,
      textInput("search_course_name", label = h3("Search a course"), value = "MATH 126"),
      helpText("Enter course code + course number, i.e. \"cse 154\". Please put white space in between."),
      hr(),
      # tableOutput(outputId = "message"),
      uiOutput("textui"),
      hr(),
      bookmarkButton(style = "background-color:#E95420;", "Share this page")
    ),
    mainPanel(
      plotlyOutput(outputId = "chart"),
      plotlyOutput(outputId = "course"),
      HTML('<script type="text/javascript">
  
var observer = new MutationObserver(function(mutations, observer) {
    var x = document.querySelectorAll("g.pointtext text");
    var i;
    for (i = 0; i < x.length; i++) {
        x[i].style.visibility = "hidden";
    }
});

observer.observe(document, {
  subtree: true,
  attributes: true
});

mixpanel.track("Remove label on legend");

$(document).on("shiny:inputchanged", function(event) {
    
    mixpanel.track("input changed", {"event name": event.name, "event value": event.value});
    
  });
</script>'),


      br(),
      br(),
      br(),
      br(),
      br()
    )
  )
)

my_ui <- function(request) {
  navbarPage(
    collapsible = TRUE,
    theme = shinytheme("united"),
    "UWGPA Analytica",
    search_page,
    navbarMenu("GPA Data Exploration", page_two, page_one, page_four, summary),
    about_pagge <- tabPanel(
      "About",
      includeHTML("front.html"),
      includeCSS("custom.css"),
      tags$head(includeScript("google.js"))
    )
  )
}
