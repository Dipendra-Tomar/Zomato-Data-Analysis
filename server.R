server <- function(input, output, session) {
  output$Visual3 <- renderPlotly({
    plot_ly(cntry_rest_count, labels = ~Country, values = ~Count, type = 'pie') %>%
      layout(title = 'Country-wise Restaurant Count Share',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$Visual4 <- renderPlotly({
    plot_ly(india_city_rstrnt_count, labels = ~City, values = ~Count, type = 'pie') %>%
      layout(title = 'City-wise Restaurant Count Share(Only in India)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$ratingtext_input_ui <- renderUI({
    selectInput(
      inputId = 'ratingtext_selectinput', label = 'Select Rating Text',
      choices = unique(zomato_data$Rating.text),
      selected = c("Excellent", "Very Good", "Good", "Average", "Poor"),
      multiple = TRUE)
    })
  
  zdat_rat_textren <- reactive({
    zomato_data[zomato_data$Rating.text %in% input$ratingtext_selectinput, ]
  })
  
  output$rating_input_ui <- renderUI({
    selectInput(
      inputId = 'rating_selectinput', label = 'Select Rating',
      choices = sort(unique(zdat_rat_textren()$Aggregate.rating)),
      selected = c(unique(zdat_rat_textren()$Aggregate.rating)),
      multiple = TRUE)
  })
  
  zdat_rat_rendr <- reactive({
    zdat_rat_textren()[zdat_rat_textren()$Aggregate.rating %in% input$rating_selectinput, ]
  })
  
  rating_cols_x <- reactive({
    c('Excellent' = 'green4', 'Very Good' = 'green', 'Good' = 'yellow',
      'Average' = 'orange', 'Poor' = 'red', 'Not rated' = 'white')
  })
  
  output$Visual1 <- renderPlot({
    ggplot(data = zdat_rat_rendr(),
           mapping = aes(x = Aggregate.rating)) +
      geom_bar(aes(fill = Rating.text)) +
      scale_fill_manual(name = "Rating Color", values = rating_cols_x()) +
      xlab('Rating') + ylab('Restaurant Count') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "ITCOfficinaSans LT Book", face = "bold"),
        axis.text.y = element_text(colour = "blue4", size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(colour = 'black'),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.length = unit(.20, "cm"),
        axis.ticks.x = element_line(colour = "black"),
        strip.text.x = element_text(size = 14, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 16, colour = "black", face = "bold",
                                    vjust = -2),
        axis.title.y = element_text(size = 16, colour = "blue4", face = "bold",
                                    angle = 90, vjust = -0.5),
        plot.title = element_text(size = 18, vjust = 3),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1, 1, 1, 1), "cm")
      )
    })
  
  rating_text_share_data <- reactive({
    zdat_rat_rendr() %>% group_by(Rating.text) %>%
      summarise('Count' = length(Restaurant.ID))
  })
  
  output$Visual6 <- renderPlotly({
    plot_ly(data = rating_text_share_data(), labels = ~Rating.text, values = ~Count) %>%
      add_pie(hole = 0.6) %>%
        layout(title = 'Rating-wise Restaurant Count Share',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               )
  })
  
  output$Visual5 <- renderPlotly({
    plot_ly(
      zdat_rat_rendr(), x = ~`Votes`, y = ~`Aggregate.rating`,
      color = ~Rating.text, type = 'scatter', text = ~paste("Name: ", Restaurant.Name,
                                                            "</br> Locality: ", Locality))%>%
      layout(title = 'Relationship between Votes and Rating')
  })
  
  output$country_filter_ui <- renderUI({
    selectInput(
      inputId = 'country_selectinput', label = 'Select Country',
      choices = sort(unique(zdat_rat_rendr()$Country)),
      selected = c("India"), multiple = TRUE)
  })
  
  restrnt_price_data <- reactive({
    zdat_rat_rendr()[zdat_rat_rendr()$Country %in% input$country_selectinput, ]
    })
  
  # restrnt_price_data <- reactive({
  #   zdat_rat_rendr() %>% filter(Country.Code == 1)
  # })
  
  output$Visual2 <- renderPlotly({
    plot_ly(
      restrnt_price_data(), x = ~`Average.Cost.for.two`, y = ~`Aggregate.rating`,
      color = ~Price.range, type = 'scatter', text = ~paste("Name: ", Restaurant.Name,
                                                            "</br> Locality: ", Locality))%>%
      layout(title = 'Relationship between Avg Cost for Two and Rating')
  })
}