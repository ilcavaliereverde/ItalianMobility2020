server <- function(input, output, session) {

  # Helper function to extract single value from filtered tibble.
  extract_value <- function(data) {
    data %>% unlist() %>% as.character()
  }

  # Observer that updates available provinces based on selected region.
  observeEvent(input$reg, {
    updateSelectInput(
      session,
      "pro",
      choices = regpro %>%
        filter(
          region == regpro %>%
            filter(reglab == input$reg) %>%
            select(region) %>%
            extract_value()
        ) %>%
        select(prolab) %>%
        drop_na() %>%
        extract_value() %>%
        sort()
    )
  })

  # Reactive functions from inputs.

  # Date range selection.
  date_range <- reactive({
    input$dateRange
  })

  # Variable selection from the variable relational database.
  selected_var <- reactive({
    nam %>%
      filter(namlab == input$vis) %>%
      select(var) %>%
      as.character()
  })

  # Region selection from the region-province relational database.
  selected_region <- reactive({
    regpro %>%
      filter(reglab == input$reg) %>%
      select(region) %>%
      extract_value()
  })

  # Province selection from the region-province relational database.
  selected_province <- reactive({
    regpro %>%
      filter(prolab == input$pro) %>%
      select(province) %>%
      extract_value()
  })

  # Creates a temporary database from selected inputs.
  filtered_df <- reactive({
    dfr %>%
      select(province, date, selected_var()) %>%
      filter(province == selected_province() |
               province == selected_region() | province == NATIONAL_LABEL) %>%
      pivot_wider(names_from = province, values_from = selected_var())
  })
  
  # Plot output.
  output$plot <- renderPlot({
    ggplot(filtered_df(), aes(x = date)) +

      # Regional rolling average, only if selected.
      {
        if (isTRUE(input$chk))
          geom_line(
            aes(y = zoo::rollmean(
              .data[[selected_region()]], ROLLING_WINDOW, na.pad = TRUE, align = "right"
            )),
            colour = COLOR_REGIONAL,
            alpha = PLOT_ALPHA,
            linewidth = PLOT_LINE_SIZE
          )
      } +

      # National rolling average, only if selected.
      {
        if (isTRUE(input$ita))
          geom_line(
            aes(y = zoo::rollmean(
              .data[[NATIONAL_LABEL]], ROLLING_WINDOW, na.pad = TRUE, align = "right"
            )),
            colour = COLOR_NATIONAL,
            alpha = PLOT_ALPHA,
            linewidth = PLOT_LINE_SIZE
          )
      } +

      # Province, daily change from baseline.
      geom_line(
        aes(y = .data[[selected_province()]]),
        alpha = PLOT_ALPHA_DAILY,
        colour = COLOR_PROVINCE_DAILY
      ) +

      # Province, rolling average.
      geom_line(
        aes(y = zoo::rollmean(
          .data[[selected_province()]], ROLLING_WINDOW, na.pad = TRUE, align = "right"
        )),
        colour = COLOR_PROVINCE_AVG,
        alpha = PLOT_ALPHA,
        linewidth = PLOT_LINE_SIZE
      ) +
      geom_area(
        aes(y = zoo::rollmean(
          .data[[selected_province()]], ROLLING_WINDOW, na.pad = TRUE, align = "right"
        )),
        fill = COLOR_PROVINCE_AVG,
        alpha = PLOT_ALPHA_AREA
      ) +

      # Plot labels.
      labs(
        title = "Italian mobility changes",
        subtitle = paste0(
          nam %>% filter(var == selected_var()) %>% select(namlab) %>% as.character(),
          " 2020-2021"
        ),
        x = NULL,
        y = NULL,
        caption = "Source: egiovannini.shinyapps.io/ItalianMobility/"
      ) +

      # Y axis labels.
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100)) +
      scale_x_date(
        date_breaks = "1 month",
        limits = date_range(),
        labels = scales::date_format("%b %y")
      ) +

      # General theme.
      cowplot::theme_minimal_grid(font_size = PLOT_FONT_SIZE) +

      # Angle on x axis.
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  },
  height = 600)
  
  # Variable description, reactive on variable choice.
  output$summ1 <- renderText({
    HTML(paste0("<code>",
                nam %>% filter(var == selected_var()) %>% select(namlab) %>% as.character(),
                "</code>"),
         nam %>% filter(var == selected_var()) %>% select(text) %>% as.character())
  })

}
