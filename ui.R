  # READ ME #####################################################################
  # AUTHOR:
  # PURPOSE OF SCRIPT: Code for interactive vis for Household Estimates, 2018.
  # CONTACTS: Sandy Taylor
  # SOURCES:  
  # NOTES: ---
  ##############################################################################
### 000 Required packages ----
require(shiny)
require(shinydashboard) # apparently needed
library(dplyr)
library(ggrepel)
library(ggplot2)
require(stringr)
library (tidyverse)
library(plotly)
## 00 Required Datasets ----
load("household_estimates_2018.RData")
# creating a list of areas
list_of_areas <- as.list(as.character(unique(hh$Area)))
list_of_sdpa <- list_of_areas[c(34, 35, 36, 37)]
list_of_np <- list_of_areas[c(38, 39)]
list_of_council <- list_of_areas[c(1:25, 27:33)]
# selecting the years for SDPAs and NPS ==> (find a better way to do it)
list_of_year_sdpa <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
2016, 2017, 2018)
## 0 Common features ---- 
g2 <- "#ADBD8E"
g1 <- "#5c7b1e"
theme_set(theme_minimal(base_size = 16))
## ui ----
ui <- dashboardPage(
  # ui - background ----
  # Main title - Appears in the bar at the top of the page
title = "Household Estimates for Scotland", 
dashboardHeader(title = tags$a(href = "http://www.nrscotland.gov.uk", 
 tags$img(height = "45", alt = "NRS", src = "logo.png"), 
 tags$script(HTML('
 $(document).ready (function() { 
 $("header").find("nav").append(\'<span class="myClass"> 
 Household Estimates for Scotland </span>\'); 
}) 
')), 
tags$head(tags$style(HTML( 
  '.myClass {
  font-size: 17px;
  line-height: 50px; 
  text-align: left; 
  padding: 0 15px;
  overflow: hidden; 
  color: white;
  font-family: "Roboto", sans-serif !important; font-weight:400;
            }
          ')),
  HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?
  family=Roboto: 100,200,300,400,500,900">')
  ),
  HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?
  family=Open+Sans:400,700,800italic|Roboto: 100,200,300, 400,500,900|Oswald:
  400,300|Lato:400,100">'))
  ), 
  # ui - Side Menu ----
dashboardSidebar( 
sidebarMenu( 
menuItem("Introduction", tabName = "tab0", icon = icon("th")), 
#here add name of the menu items
menuItem("Households trends", tabName = "tab1", icon = icon("line-chart")),
menuItem("Number of households", tabName = "tab2", icon = icon("bar-chart")),
menuItem("Percentage change", tabName = "tab3", icon = icon("line-chart")),
menuItem("Average household size", tabName = "tab4", icon = icon("bar-chart")),
menuItem("More information", tabName = "tab5", icon = icon("info"))
)
), 
  dashboardBody(
    
    HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
    HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
    tags$script(src = "cookie_control_config.js"),
    
    #adding css file
 tags$head( 
  includeCSS("style.css")
 ), 
 tabItems(
## 0. Tab - Small introduction ----
tabItem(tabName = "tab0", 
        fluidPage( 
          titlePanel("Introduction"), 
br(),
h4("This interactive visualisation shows the", (strong("estimated number of 
households in Scotland")), "from 1991 to 2018. The total figures can be 
broken down by single local authority, Strategic Development Plan Areas 
(SDPA) and National Parks and comparisons can be made."), 
br(),
h4("In 2018 there were", (strong("2.48 million households")), "in Scotland, an 
   increase of around 14,500 households (0.6%) compared with the previous year."),
br(),
h4("Since 1991, the number of households in Scotland has increased by just 
     over 434,500 (21.3%)."),
br(),
h4("The household information presented in this interactive visualisation for
National Parks and Strategic Development Plan areas relates to the number
of occupied dwellings.The data series starts from 2008, the year when SDP 
areas were created.")
  )),
## 1. Tab - Households trends ----
tabItem(tabName = "tab1", 
  fluidPage(tags$head(
  tags$style(
  HTML(".shiny-output-error-validation {
  font-family: arial, sans-serif;
  font-size: 1.3em;
  line-height: 1.1;
  color: black;
  }
        "))),
titlePanel("Households trends"),
fluidRow(
 # 1.1 Side menu ----
column(3,
wellPanel(
helpText("Please select options for chart"),
br(),
selectInput(inputId="Area1", label="Area:", 
choices = list(" "= list_of_areas, "Scottish areas"=list_of_council,  
               "SDPA" =list_of_sdpa, "National Park" = list_of_np), 
selected="Scotland",
selectize=TRUE),
h5( "Data for SDPAs and NPS starts from 2008."),
br(),
sliderInput(inputId="end_year1",
label="Year:",
value =  c(min(hh$Year), max(hh$Year)),
min = min(hh$Year),
max = max(hh$Year),
step=1, sep = ""),
h5("If you want to show data on SDPAs and NPs you need to start from 2008.")
)),
 # 1.2 Plot ----
column(9, 
wellPanel(
plotlyOutput("my_plot1"),
# Show the explanation for SDPAs and NPs only if such areas have been selected
conditionalPanel("input.Area1=='SESplan' || input.Area1=='TAYplan' || input.Area1=='Glasgow and the Clyde Valley' || input.Area1=='LLNTP' || input.Area1=='Cairngorms' || input.Area1=='Aberdeen City and Shire'",
br(),
h5( "We have data on SDPAs and NPS starting from 2008. These figures refer to",
(strong("the number of occupied dwellings")),"and not to the number of 
household. Although an occupied dwelling is roughly equivalent to a household,
the number of occupied dwellings can differ from the number of households 
recorded by the census. More information", a("here.",
href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/2018", 
target="_blank")))
)
)
))),
## 2. Tab - Number of households ----
tabItem(tabName = "tab2",
  fluidPage(
  titlePanel("Number of households")
  ),
  # 2.1 Side menu ----
  fluidRow(
column(3,
 wellPanel(
helpText("Please select options for chart"),
br(),
selectizeInput(inputId="Area2", label="Area:",
 choices=list("Scottish areas"=list_of_council, 
"SDPA"=list_of_sdpa, "NP"=list_of_np), 
 selected="NULL", 
 multiple= TRUE
 #selectize=TRUE,
 #options = list(placeholder = 'Enter area name')
 ),
h5( "Data for SDPAs and NPs starts from 2008."),
br(),
sliderInput(inputId="end_year2",
 label="Year for Councils:",
 value = c(min(hh$Year), max(hh$Year)),
 min = min(hh$Year),
 max = max(hh$Year),
 step=1, sep = ""),
br(),
sliderInput(inputId="end_year2b",
 label="Year for SDPAs and NPs:",
 value = c(min(list_of_year_sdpa), max(list_of_year_sdpa)),
 min = min(list_of_year_sdpa),
 max = max(list_of_year_sdpa),
 step=1, sep = "")
 )
),
  # 2.2 Plot ----
column(9, 
 wellPanel(
plotlyOutput("my_plot2"),
br(),
plotlyOutput("my_plot2b"),
br(),
h5( "We have data on SDPAs and NPS starting from 2008. These figures refer to",
(strong("the number of occupied dwellings")),"and not to the number of 
household. Although an occupied dwelling is roughly equivalent to a household,
the number of occupied dwellings can differ from the number of households 
recorded by the census. More information", a("here.",
 href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/2018", 
 target="_blank"))
)
))),
## 3. Tab - Percentage change of the number of households ---- 
tabItem(tabName = "tab3",
  fluidPage(
  titlePanel("Percentage change of the number of households")
    ),
  # 3.1 Side menu ----
  fluidRow(
  column(3,
wellPanel(
helpText("Please select options for chart"),
br(),
selectizeInput(inputId="Area3", label="Area:",
choices=list(" "= list_of_areas,"Scottish areas"=list_of_council, 
             "SDPA"=list_of_sdpa, "NP"=list_of_np), 
selected="Scotland",
multiple=TRUE
),
h5( "Data for SDPAs and NPs starts from 2008."),
br(),
sliderInput(inputId="end_year3",
label="Year:",
value = c(min(hh$Year), max(hh$Year)),
min = min(hh$Year),
max = max(hh$Year),
step=1, sep = ""),
h5("If you want to show data on SDPAs and NPs you need to start from 2008."),
br()
  )),
  # 3.2 Plot ----
  column(9,
wellPanel(
plotlyOutput("my_plot3"),
conditionalPanel("input.Area3 %in% as.list(list_of_sdpa, list_of_np)",
br(),
h5( "We have data on SDPAs and NPS starting from 2008. These figures refer to",
(strong("the number of occupied dwellings")),"and not to the number of 
household. Although an occupied dwelling is roughly equivalent to a household,
the number of occupied dwellings can differ from the number of households 
recorded by the census. More information", a("here.",
href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/2018", 
target="_blank"))
  )
  )
))),
## 4. Tab - Average household size ----
tabItem(tabName = "tab4",
  fluidPage(
 titlePanel("Average household size")
  ),
  fluidRow(
  # 4.1 Side menu ----
  column(3,
wellPanel(
helpText("Please select options for chart"),
selectizeInput(inputId="Area4", label="Area:",
choices=list(" "=list_of_areas[26], "Scottish areas"=list_of_council), 
selected="Scotland",
multiple=TRUE
),

h5("Data on average household size for SDPAs and NPs are not available."),
br(),
sliderInput(inputId="end_year4",
label="Year:",
value = c(2001, max(hh$Year)), # we have average size only starting from 2001
min = 2001,
max = max(hh$Year),
step=1, sep = "")
)
  ),
  # 4.2 Plot ----
  column(9,
wellPanel(
plotlyOutput("my_plot4"))
  )
)),
## 5. Tab - More information ----
  # More information on HH estimates and useful links (to update)
tabItem(tabName = "tab5",
  fluidPage(
  titlePanel("More information"),
 # 5.1 More information text ---- 
  fluidRow(
 column(12, 
 br(),
 h4("A", strong ("household"), "is defined as one person living alone 
  or a group of people (not necessarily related) living at the same address."),
 br(),
 h4("Household estimates are based on the number of total dwellings obtained 
    from Council Tax (Ctaxbase) systems. The occupied dwellings are calculated 
    by subtracting vacant dwellings and second homes from the total dwellings 
    figures. The resulting number of occupied dwellings is then adjusted from 
    September back to June.", 
    strong("Although an occupied dwelling is roughly equivalent to a household,
           the number of occupied dwellings can differ from the number of 
           households recorded by the Census."),
  "A further adjustment is made to account for differences in the number of 
   households estimated from Council Tax data and the number recorded in 
   Scotland's Census 2011. 
  A number of factors can be responsible for the different number of households 
  recorded by each source. They can include the treatment of vacant dwellings, 
  shared dwellings, holiday lets, caravans and some communal establishments 
  (for example student halls of residence)."),
 br(),
 
 h4("The estimates are used for a range of purposes by councils, the Scottish Government, other organisations and researchers. Statistics about dwellings and households are used mainly for informing council decisions about housing need and the provision of services (including housing, waste collection and community care)."), 
 br(),
 # 5.2 Links (to update) ----
 h4("More information about statistics on household estimates, including the 
    methodology used to produce them, can be found on the", a("Household estimates",
 href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates", 
 target="_blank"), "page on the NRS website."),
 br(),
 br(),
  fluidRow(
column(4,  
wellPanel(
 h6(strong("More information")),
 h6("Data: ", span(a("Household estimates by 2011 data zones, 2018  (.xlsx)",
 href="https://www.nrscotland.gov.uk/files//statistics/nrs-visual/he-18/house-est-18-vis-source-table.xlsx", 
 target="_blank")),
 h6("Publication: ", a("Estimates of Households and Dwellings in Scotland, 2018",
 href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/2018", 
 target="_blank"))))),
  column(4, 
  wellPanel(
 br(), span(
 h6("Follow us on Twitter - ", 
 a("@NatRecordsScot", 
 href="https://twitter.com/NatRecordsScot",
 target="_blank")),
 h6("See more ", 
  a("Infographics & Visualisations", 
  href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations", 
  target="_blank"))))),
column(4, 
wellPanel(span(
  h6(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", 
       target="_blank")),
  h6("\U00A9 Crown Copyright 2019 - ",
a("Copyright conditions", 
  href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", 
  target="_blank"))))
  )),
  fluidRow(
 wellPanel(
h4("Any feedback about this visualisation?", 
   a("Get in touch!", 
     href="mailto:Joseph.Adams@nrscotland.gov.uk?subject=Scotland Household Estimates visualisation &cc=statisticscustomerservices@nrscotland.gov.uk", 
     target="_blank")
  
  ))
))# End of fluidRow
  ))#Fluidpage
  )#tabItem

  )#TabItems
  )#dashboard body
 
)#end
