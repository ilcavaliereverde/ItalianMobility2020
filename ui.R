ui = navbarPage(
  title = NULL,
  id = "Welcome",
  theme = shinytheme("yeti"),
  
  #Welcome page tab
  tabPanel("Introduction",
           includeMarkdown("welcome.Rmd")),
  
  #Plot tab
  tabPanel("Dashboard",
           fluidRow(
             column(
               3,
               
               #Date range selector
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
               
               #Variable selector
               selectInput("vis",
                           label = "Visits to:",
                           nam$name),
               
               
               #Region selector
               selectizeInput("reg",
                              label = "Region:",
                              selected = "Abruzzo",
                              reg),
               
               #Province selector
               selectInput("pro",
                           label = "Province:",
                           choices = NULL),
               
               br(),
               
               #Region average selector
               materialSwitch(
                 inputId = "chk",
                 label = "Regional average",
                 right = TRUE,
                 status = "primary"
               )
             ),
             
             #Plot panel with summary
             column(9,
                    plotOutput("plot"),
                    
                    br()),
             
             
             
             column(7, offset = 3,
                    p(plotdescr),
                    
                    uiOutput("summ"))
             
             
             
           ))
)
