
server <- function(input, output, session) {

    output$summary_txt <- renderUI({ 
    HTML("March 18, 2019 <br>
          <ul><li>Solstice is an extremely positive place place to work.</li>
          <li>Our ability to collaborate, trust, and treat each other with respect will be the key to future success. </li>
    <li>Although none of our performance metrics revealed any issue, management is taking the feedback seriously and is already initiating steps to improve.</li></ul>
         <hr>
          
      September 17, 2019 <br>
<p>The unbridled optimism from the last anonymous survey contrasts with the challenge of
building an innovative movie studio. Follow our initial honeymoon period, there was only one way for the
scores to go in a subsequent survey. While employees in believe Solstice is a positive
environment, with mutual admiration for the level of talent working here; appreciation for management's clear
communication and transparency, and an innate ability to get things done; we are now facing
growing pains, coming to terms with our roles, responsibilities, limitations, and sense of
freedom to work in a way we see fit.</p>
 
<p>On the positive side, there's a general recognition that Solstice will do whatever it takes to
survive--we are scrappy and able to adapt quickly. At this point, many of us would like to see a greater focus on
maintaining our core values of psychological safety, trust, and employee
empowerment.</p>

<p>There were two clear and related ideas within the open-end questions that we should consider
addressing. The first is the hours physically spent in the office, where people say they believe
they can perform better if they have more flexibility to recharge, connect with and care for our
children, family, friends, and pets. The second is they feel they can be more productive if they
were empowered to act more independently. These two ideas are related more generally to
the notion that we want the freedom to perform *when* we feel we can be at our best, and
also *how* we feel it’s best to carry out our duties.</p>
"
           )
      
  })
  
  output$info_completed_surveys <- renderInfoBox({
    
    n_respondents_filtered <- n_respondents %>% 
      filter(wave_date == input$wave_date_overview)
    
    
    infoBox(
      "Completed Surveys", n_respondents_filtered$n_completes,
      color = "purple", icon = icon("users")
    )
  })
  
  # output$info_number_of_waves <- renderInfoBox({
  #   infoBox(
  #     
  #     "Number of Waves", length(wave_df_li), color = "light-blue"
  #   )
  #   
  # })
  
  output$info_5pt_scale_count<- renderInfoBox({
    infoBox(
      "5-pt Scale Questions", n_5_scale_qs,
      color = "orange", icon = icon("ruler")
    )
  })
  
  output$info_10pt_scale_count<- renderInfoBox({
    infoBox(
      "10-pt Scale Questions", 1,
      color = "red", icon = icon("chart-line")
    )
  })
  
  output$info_OE_count <- renderInfoBox({
    infoBox(
      "Open End Questions", 5,
      color = "green", icon = icon("comments")
    )
  })
  
  output$info_total_q_count <- renderInfoBox({
    infoBox(
      "Total Questions", 88,
      color = "black", icon = icon("clipboard-check")
    )
  })
  
  output$enps_out <- renderPlotly({
   
    selected_wave_date <- input$wave_date_enps
    
    selected_wave_df <- enps %>% filter(wave_date == selected_wave_date)
    
    enps_selected <- ggplot(selected_wave_df, aes(y = value, x = measure)) +
      geom_col(aes(fill = color_group)) +
      geom_text(aes(label = value_l), nudge_y = .01) +
      scale_y_continuous(labels =scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = color_values) +
      labs(x = "Score", y = "Percent", title = paste0("Survey Date: ", selected_wave_date)) +
      theme_tufte() +
      theme(legend.position = "none") 
    
     ggplotly(enps_selected)
  }
    
  )

  
  output$enps_over_time_out <- renderPlotly({
    
    ggplotly(enps_over_time)
  })
  
  output$current_overall_gg <- renderPlotly({

    selected_overall_answers <- five_pt_dist_df %>% 
      mutate(wave_date = as.character(wave_date)) %>% 
      filter(as.character(wave_date) %in% input$wave_date_5p_dist)
  
    
    selected_overall_gg <- ggplot(selected_overall_answers, 
                                  aes(y = n, x = answer,  group = wave_date, fill = wave_date) ) + 
      geom_col(position = "dodge") +
      # geom_text(aes(label = n, y = n+ 50), position = position_dodge(0.9)) + 
      geom_text(aes(label = percent_l, y = n + 50), position = position_dodge(0.9)) + 
      labs(x = "Response Category (5 pt scale)",y = "Count/Percent", fill = "Survey Date") +
      ggtitle(paste0("Distribution of Scores")) + 
      theme_tufte()
    
    selected_overall_gg
    
    # ggplotly(selected_overall_gg)
  })

    
  output$current_8_categories <- renderTable({ 
    
    
    
    chosen_perform_categories <-  perform_categories%>% 
      filter(as.character(wave_date) %in% input$wave_sub_focus_areas)
    
    categories_table_current_kable <- chosen_perform_categories %>% 
      filter(as.character(wave_date) %in% input$wave_sub_focus_areas) %>% 
      select(wave_date, Grouping, `Average Score` = aggregate_average_score) %>%
      mutate(`Average Score` = round(`Average Score`, 2)) %>% 
      spread(wave_date, `Average Score`) %>% 
      arrange(desc(!!sym(as.character(max(chosen_perform_categories$wave_date)))))
    
    categories_table_current_kable
    
    
    
  })
  
  output$categories_table_over_time_gg <-  renderPlotly({
    
    ggplotly(categories_table_over_time_gg)
    
  })
  
  output$enps_scale_norms <- renderTable({
    tribble(~Rating, ~Performance, 
            "Below 0", "Poor",
            "0-9", "Fair",
            "10-30", "Good",
            "31-49", "Very Good",
            "50+", "Excellent") %>% return()
    
  })
  
  output$dist_across_respondents_5pt_plty <- renderPlotly({
    
    dist_across_respondents_5pt_hist <- dist_across_respondents_5pt_hist_cut %>% 
      filter(as.character(wave_date) %in% input$wave_sub_avg_score_dist) %>% 
      ggplot(aes(y = p, x = answer_cut, group = wave_date, fill = as.factor(wave_date)) ) +
      geom_col(position = "dodge") +
      coord_cartesian(ylim = c(0,1)) +
      labs(x = "Average Answer", y = "Percent of Scores", fill = "Survey Date") +
      scale_x_discrete(drop=FALSE) +
      scale_y_continuous(labels = scales::percent) +
      theme_tufte() 
    
    ggplotly(dist_across_respondents_5pt_hist)
  
  })

  # output$dist_across_respondents_5pt_hist_current <- renderPlotly({
  #   ggplotly(dist_across_respondents_5pt_hist_current)
  #   
  # })
  # 
  
  output$score_distribution_over_time_pltly <- renderPlotly({
    ggplotly(score_distribution_over_time_gg)
  })
  
  output$current_oe_ggpltly <- renderPlotly({
    selected_wave <- input$wave_sub_open_ends_current
    
    current_oe <- oe %>% 
      filter(as.character(wave_date) == selected_wave)
    
    current_oe_gg <- current_oe %>% 
      ggplot(aes(x = question_text, y = p, label = answer, color =question_text, text =sprintf("Answer: %s<br>Percent Selected: %s", answer, scales::percent(p)))) +
      geom_jitter(aes(size = p), alpha = .5 ) +
      labs(x = "", y = "Percent Answers in Category", title = paste0("Survey Date: ", selected_wave )) +
      theme_minimal() +
      coord_flip() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = c(0, .7)) +
      coord_flip()
    
    current_oe_ggpltly <-   ggplotly(current_oe_gg, tooltip = "text")
    
    current_oe_ggpltly
    
    
  })
  
  output$overtime_oe_ggpltly <- renderPlotly({
    overtime_oe_ggpltly
  })
  
  output$by_question_distribution_ggpltly <- renderPlotly({
    
    
    by_question_distribution_gg <- by_question_distribution %>% 
      filter(as.character(wave_date) ==input$wave_date_ind_questions) %>% 
      ggplot(aes(x = answer, y = question_text, fill = p)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(y = "")
    
    by_question_distribution_ggpltly <- ggplotly(by_question_distribution_gg)
    
    
    by_question_distribution_ggpltly
  })
  
  
  output$indiv_question_distributions <- renderPlotly({
  
  by_question_distribution_gg <- by_question_distribution %>% 
    filter(as.character(wave_date) %in% input$wave_date_ind_questions_exp, question_text == input$question_text_str) %>% 
    ggplot(aes(x = answer, y = p, fill = as.character(wave_date) ) ) +
    labs(title = paste0(input$wave_date_ind_questions_exp,":", input$question_text_str), y = "Percent", fill = "Dates Selected") +
    geom_col(position = "dodge") +
    scale_x_discrete(drop=FALSE) +
    coord_cartesian(ylim = c(0,1)) +
    scale_y_continuous(labels = scales::percent) +
    theme_tufte()
  
  by_question_distribution_ggpltly <- ggplotly(by_question_distribution_gg)
  
  return(by_question_distribution_ggpltly)
  
  })
  

}

# shinyApp(ui, server)