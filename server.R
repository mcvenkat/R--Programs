shinyServer(function(input, output,session) {
  
  # Sample of two ratios
  sample_ratios=sample(ratios, 2)
  
  # Option A rectangle
  output$A_plot <- renderPlot({
    ggplot() +
      geom_rect(fill = "gold", 
                colour = "grey50", 
                aes(xmin=(max_ratio-sqrt(sample_ratios[1]))/2, 
                    xmax=sqrt(sample_ratios[1])+(max_ratio-sqrt(sample_ratios[1]))/2, 
                    ymin=0, 
                    ymax=1/sqrt(sample_ratios[1]))) +
      scale_x_continuous(limits=c(0,max_ratio), expand=c(0,0)) +
      scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
      coord_fixed() +
      theme_void()
  })
  
  # Option B rectangle
  output$B_plot <- renderPlot({
    ggplot() +
      geom_rect(fill = "gold", 
                colour = "grey50", 
                aes(xmin=(max_ratio-sqrt(sample_ratios[2]))/2, 
                    xmax=sqrt(sample_ratios[2])+(max_ratio-sqrt(sample_ratios[2]))/2, 
                    ymin=0, 
                    ymax=1/sqrt(sample_ratios[2]))) +
      scale_x_continuous(limits=c(0,max_ratio), expand=c(0,0))+
      scale_y_continuous(limits=c(0,1), expand=c(0,0))+
      coord_fixed() +
      theme_void()
  })
  
  # Once the user chooses an option, vote button appears
  output$vote <- renderUI({ 
    actionButton(inputId = "vote", 
                 label = "Vote", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })
  
  # Once the user votes, Play again link appears
  output$refresh <- renderUI({ 
    actionLink(inputId = "refresh", label = "Play again") 
  })
  
  # Once the user votes, See results link appears
  output$results <- renderUI({ 
    actionLink(inputId = "results", label = "See results")
  })
  
  # A bit of JavaScript to refresh page
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  # After voting, options and vote button are disabled and result are appended to logfile
  observeEvent(input$vote, {
    shinyjs::disable("vote")
    shinyjs::disable("choice")
    cat(c(as.character(Sys.time()), sample_ratios, input$choice), '\n', file = logfilename,
        append = TRUE)
  })
  
  # If user press See results, we go to resu TabsetPanel
  observeEvent(input$results, {
    updateTabsetPanel(session, "tabs", selected = "resu")
  })
  
  # Value box with number of votes
  output$no_votes <- renderValueBox({
    valueBox(length(readLines(logfilename)),  "Answers", icon = icon("users"))
  })
  
  # Highcharter with visualization of results
  output$hc_results <- renderHighchart({
    
    results=read.table(logfilename, col.names=c("date", "time", "option_A", "option_B", "winner"))
    
    data.frame(ratio=c(results$option_A, results$option_B)) %>% 
      group_by(ratio) %>% 
      summarize(matchs=n()) -> all_matchs
    
    results %>% 
      filter(winner!="N") %>% 
      mutate(ratio=ifelse(winner=="A", option_A, option_B)) %>% 
      group_by(ratio) %>% 
      summarize(wins=n()) -> all_wins
    
    all_matchs %>% 
      left_join(all_wins, by="ratio") %>% 
      mutate(wins=ifelse(is.na(wins), 0, wins)/matchs) -> results_def
    
    highchart() %>%
      hc_title(text = "The Pleasing Ratio Project") %>%
      hc_subtitle(text = "Victories and games played by ratio") %>%
      hc_xAxis(title = list(text = "Ratio"), 
               labels = list(rotation=-90, style = list(fontSize = "10px")),
               categories = results_def$ratio) %>%
      hc_yAxis_multiples(
        list(title = list(text = "% victories"), max = 100, labels = list(format = "{value}%", useHTML = TRUE)),
        list(title = list(text = "# games played"), opposite = TRUE)
      ) %>%
      hc_add_series(yAxis = 0, name = "% victories", data = results_def$wins*100,  marker = list(enabled = FALSE), color="blue", type="column") %>%
      hc_add_series(yAxis = 1, name = "# games played", data = results_def$matchs,  marker = list(enabled = FALSE), color="gold") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_chart(zoomType = "xy")
  })
  
})