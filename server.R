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
    regpro %>% filter(reglab == input$reg) %>% select(region) %>% as.character()
  })
  
  #Province selection from the region-province relational database
  p = reactive({
    regpro %>% filter(prolab == input$pro) %>% select(province) %>% as.character()
  })
  
  #Creates a temporary database from selected inputs
  df1 = reactive({
    dfr %>%
      select(province, date, v()) %>%
      filter(province == p() | province == r() | province == "Italy") %>%
      pivot_wider(names_from = province, values_from = v())
  })
  
  #Plot output
  output$plot = renderPlot({
    ggplot(df1(), aes_string(x = "date")) +
      
      # Region, rolling mean 7 days, only if selected
      {
        if (input$chk == T)
          geom_line(
            aes(y = zoo::rollmean(
              get(r()), 7, na.pad = TRUE, align = "right"
            )),
            size = 1,
            colour = "#78B7C5",
            alpha = 0.75
          )} +
      
   {
     if (input$nat == T)
        geom_line(
          aes(y = zoo::rollmean(
            "Italy", 7, na.pad = TRUE, align = "right"
          )),
          size = 1,
          fill = "#78B7C5",
          alpha = 0.2
        )
      } +
      
      #Province, daily change from baseline
      geom_line(aes_string(y = p()), alpha = 0.2, colour = "#899DA4") +
      
      
      #Province, rolling mean 7 days
      geom_line(
        aes(y = zoo::rollmean(
          get(p()), 7, na.pad = TRUE, align = "right"
        )),
        size = 1,
        colour = "#F21A00",
        alpha = 0.9
      ) +
      geom_area(
        aes(y = zoo::rollmean(
          get(p()), 7, na.pad = TRUE, align = "right"
        )),
        size = 1,
        fill = "#F21A00",
        alpha = 0.1
      ) +
      
      #Plot labels
      labs(
        title = nam %>% filter(var == v()) %>% select(namlab) %>% as.character(),
        subtitle = "Italy 2020",
        x = NULL,
        y = "% mobility change",
        caption = "Source: egiovannini.shinyapps.io/ItalianMobility2020/"
      ) +
      
      #Y axis labels
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100)) +
      scale_x_date(
        date_breaks = "1 month",
        limits = d(),
        labels = date_format("%b %y")
      ) +
      
      #General theme
      cowplot::theme_minimal_grid() +
      
      #Angle on x axis
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summ = renderText({
    HTML(nam %>% filter(var == v()) %>% select(text) %>% as.character())
  })
}