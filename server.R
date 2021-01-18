server = function(input, output, session) {
  #Refreshing data every week (milliseconds * seconds * minutes * hours * days to form a week)
  observe({
    invalidateLater(1000 * 60 * 60 * 24 * 7, session)
    
    read_google(path, file)
    
  })
  
  
  #Observer that updates available provinces based on selected region
  observeEvent(input$reg,
               {
                 updateSelectInput(
                   session,
                   "pro",
                   choices = regpro %>%
                     filter(
                       region == regpro %>%
                         filter(reglab == input$reg) %>%
                         select(region) %>%
                         unlist() %>%
                         as.character()
                     ) %>%
                     select(prolab) %>%
                     drop_na() %>%
                     unlist() %>%
                     sort() %>%
                     as.character()
                 )
                 
               })
  
  #Reactive functions from inputs
  #Date range selection
  d = reactive({
    input$dateRange
  })
  
  #Variable selection from the variable relational database
  v = reactive({
    nam %>% filter(namlab == input$vis) %>% select(var) %>% as.character()
  })
  
  #Region selection from the region-province relational database
  r = reactive({
    regpro %>%
      filter(reglab == input$reg) %>%
      select(region) %>%
      unlist() %>%
      as.character()
  })
  
  #Province selection from the region-province relational database
  p = reactive({
    regpro %>%
      filter(prolab == input$pro) %>%
      select(province) %>%
      unlist() %>%
      as.character()
  })
  
  #Creates a temporary database from selected inputs
  df1 = reactive({
    dfr %>%
      select(province, date, v()) %>%
      filter(province == p() |
               province == r() | province == "Italy") %>%
      pivot_wider(names_from = province, values_from = v())
  })
  
  #Plot output
  output$plot = renderPlot({
    ggplot(df1(), aes_string(x = "date")) +
      
      # Regional rolling 7 days mean, only if selected
      {
        if (input$chk == T)
          geom_line(
            aes(y = zoo::rollmean(
              get(r()), 7, na.pad = TRUE, align = "right"
            )),
            colour = "#3B9AB2",
            alpha = 0.9,
            size = 0.75
          )
      } +
      
      #National rolling 7 days mean, only if selected
      {
        if (input$ita == T)
          geom_line(
            aes(y = zoo::rollmean(
              Italy, 7, na.pad = TRUE, align = "right"
            )),
            colour = "#02401B",
            alpha = 0.9,
            size = 0.75
          )
      } +
      
      #Province, daily change from baseline
      geom_line(aes_string(y = p()), alpha = 0.2, colour = "#899DA4") +
      
      
      #Province, rolling mean 7 days
      geom_line(
        aes(y = zoo::rollmean(
          get(p()), 7, na.pad = TRUE, align = "right"
        )),
        colour = "#F21A00",
        alpha = 0.9,
        size = 0.75
      ) +
      geom_area(aes(y = zoo::rollmean(
        get(p()), 7, na.pad = TRUE, align = "right"
      )),
      fill = "#F21A00",
      alpha = 0.1) +
      
      #Plot labels
      labs(
        title = "Italian mobility changes",
        subtitle = paste0(
          nam %>% filter(var == v()) %>% select(namlab) %>% as.character(),
          " 2020-2021"
        ),
        x = NULL,
        y = NULL,
        caption = "Source: egiovannini.shinyapps.io/ItalianMobility/"
      ) +
      
      #Y axis labels
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100)) +
      scale_x_date(
        date_breaks = "1 month",
        limits = d(),
        labels = date_format("%b %y")
      ) +
      
      #General theme
      cowplot::theme_minimal_grid(font_size = 18) +
      
      #Angle on x axis
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, height = 600)
  
  output$summ1 = renderText({
    HTML(paste0("<code>",
                nam %>% filter(var == v()) %>% select(namlab) %>% as.character(),
                "</code>"),
         nam %>% filter(var == v()) %>% select(text) %>% as.character())
  })
  
}
