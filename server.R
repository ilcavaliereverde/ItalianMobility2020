server = function(input, output, session) {
  #Refreshing data every week (milliseconds * seconds * minutes * hours * days to form a week)
  observe({
    invalidateLater(1000 * 60 * 60 * 24 * 7, session)
    
    readGoogle(path, country)
    
  })
  
  #Observer that updates available provinces based on selected region
  observeEvent(input$reg,
               {
                 updateSelectInput(
                   session,
                   "pro",
                   choices = prre %>% filter(region == input$reg) %>% select(provonly) %>% drop_na() %>% unlist() %>% as.character()
                 )
                 
               })
  
  #Reactive functions from inputs
  #Date range selection
  d = reactive({
    input$dateRange
  })
  
  #Variable selection
  v = reactive({
    nam %>% filter(name == input$vis) %>% select(var) %>% as.character()
  })
  
  #Region selection
  r = reactive({
    str_replace_all(input$reg, c(" " = "" , "'" = "",  "-" = ""))
  })
  
  #Province selection
  p = reactive({
    str_replace_all(input$pro, c(" " = "" , "'" = "",  "-" = ""))
  })
  
  #Observer of province choice for dataframe tidying
  df1 = reactive(
    dfr %>%
      select(c(4, 6, v())) %>%
      mutate(province = str_replace_all(province, c(
        " " = "" , "'" = "", "-" = ""
      ))) %>%
      filter(province == p() | province == r()) %>%
      pivot_wider(names_from = province, values_from = v())
  )
  
  #Plot output
  output$plot = renderPlot({
    ggplot(df1(), aes_string(x = "date")) +
      #Baseline
      # geom_line(aes(y = 0)) +
      #Region, rolling mean 7 days, only if selected
      {
        if (input$chk == T)
          geom_line(
            aes(y = zoo::rollmean(
              get(r()), 7, na.pad = TRUE, align = "right"
            )),
            size = 1,
            colour = "#78B7C5",
            alpha = 0.75
          )
      } +
      #Province, change from baseline
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
      #Plot labels
      labs(
        title = nam %>% filter(var == v()) %>% select(name) %>% as.character(),
        subtitle = "Italy 2020",
        x = NULL,
        y = "% mobility change",
        caption = "Source: shinyapps.io/ilcavaliereverde/ItalianMobility2020/"
      ) +
      #Y axis mods
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100)) +
      scale_x_date(
        date_breaks = "1 month",
        limits = d(),
        labels = date_format("%b %y")
      ) +
      #General theme
      cowplot::theme_minimal_grid() +
      #Theme mod, angled x
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summ = renderText({
    HTML(nam %>% filter(var == v()) %>% select(text) %>% as.character())
  })
}