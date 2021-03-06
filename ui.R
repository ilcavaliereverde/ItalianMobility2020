ui = navbarPage(
  title = NULL,
  id = "Welcome",
  theme = shinytheme("yeti"),
  
  # Welcome page tab.
  tabPanel("Introduction",
           includeMarkdown("welcome.Rmd")),
  
  # Plot tab.
  tabPanel("Dashboard",
           sidebarLayout(
             sidebarPanel(
               
               # Date range selector.
               dateRangeInput(
                 "dateRange",
                 label = paste("From - To:"),
                 start = min(dfr$date),
                 end = max(dfr$date),
                 min = min(dfr$date),
                 max = max(dfr$date),
                 separator = " - ",
                 format = "dd/mm/yyyy",
                 startview = "month",
                 language = "en",
                 weekstart = 1
               ),
               
               # Variable selector.
               selectInput("vis",
                           label = "Visits to:",
                           nam$namlab),
               
               
               # Region selector.
               selectizeInput(
                 "reg",
                 label = "Region:",
                 selected = "Abruzzo",
                 sort(regpro$reglab)
               ),
               
               # Province selector.
               selectInput(
                 "pro",
                 label = "Province:",
                 selected = "Chieti",
                 choices = NULL
               ),
               
               br(),
               
               # Region average switch.
               materialSwitch(
                 inputId = "chk",
                 label = "Regional average",
                 right = TRUE,
                 status = "primary"
               ),
               
               # National average switch.
               materialSwitch(
                 inputId = "ita",
                 label = "National average",
                 right = TRUE,
                 status = "success"
               ),
               
               
               hr(),
               
               # Plot description.
               p(plotdescr),
               
               hr(),
               
               # Variable description.
               uiOutput("summ1")
             )
             
             ,
             
             # Plot panel with summary.
             mainPanel(plotOutput("plot"))
           ))
)
