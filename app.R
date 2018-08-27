# deployed app 
# Generic plotting function for individual players 
# REQUIRE: player_templates
# REQUIRE: mylist.RData from preprocessing.r to be saved to desktop
# set to the folder containg the individual player templates
library(shiny)
library(rsconnect)
#setwd("/Users/ajeffares/Desktop/")
load("mylist.RData")

# colour palette
rich8equal = c("#000041", "#0000CB", "#0081FF", "darkorchid", "#80FE1A", "#FDEE02", "orange", "#FF3300")
rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")
pie(rep(1,8), col=rich8equal)

#function to find runs in binary lists
startend <- function(x){
  run_start = list()
  run_end = list()
  n = 1
  k = 1
  if(x[1] == 1){
    run_start[[n]] = 1
    n = n+1
    if(x[2]== 0){
      run_end[[k]] = 1
      k = k+1
    }
  }
  for(i in 2:210){
    if((x[i] ==1) &(x[i-1] ==0)){
      run_start[[n]] = i
      n = n+1
    }
    if( (x[i]== 1) & (x[i+1] == 0)){
      run_end[[k]] = i
      k = k+1
    }
  }
  if((x[211] == 1) &(x[210] == 0)){
    run_start[[n]] = 211
    run_end[[k]] = 211
  }
  if(x[211] == 1 &(x[210] == 1)){
    run_end[[k]] = 211
  }
  out <- list(run_start, run_end)
return(out)
}



#Defining the function
player_plot <- function(playerID, player_templates, mood_state = FALSE, sleep_quality = FALSE, wellness = FALSE,
                        energy_levels = FALSE, health = FALSE, stress = FALSE, muscle_readiness = FALSE,
                        jitter = FALSE, include_injuries = FALSE, inclue_matches = FALSE, include_warnings = FALSE){
  
  plot(1, type = "n", xlim = c(0, 211), ylim = c(1,5.5), ylab = "Values", xlab = "Day",
       main = "Plot of Variables Over Time", cex.main=1.5, cex.axis=1, cex.lab=1, font.lab=2)
  box()
  abline(h = 5.33, lwd = 9, col = "white", xpd = TRUE)
  abline(h = 5.28, lwd = 1, col = "black")
  abline(h = 5.38, lwd = 1, col = "black")
  rug(x = (0:43)*5, ticksize = -0.02, side = 1)
  legend_lines = c()
  legend_col = c()
  legend_text = c()
  lim <- par("usr")
  
  # add shaded regions corresponding to warnings 
  if(include_warnings == TRUE){
    x <- player_templates[[playerID]][["Warning"]]
    
    normal <- ifelse(is.na(x) | (x != "normal"), 0, 1)
    small <- ifelse(is.na(x) | (x != "small"), 0, 1)
    large <- ifelse(is.na(x) | (x != "large"), 0, 1)

    normal_out <- startend(normal)
    small_out <- startend(small)
    large_out <- startend(large)
    
    if(length(normal_out[[1]]>0)){
      for(i in 1:length(normal_out[[1]])){
        rect(normal_out[[1]][[i]] - 0.4, lim[3] + 0.03, normal_out[[2]][[i]] + 0.4, lim[4]- 0.02, border = "aliceblue", col = "aliceblue")
      }
    }
    if(length(small_out[[1]]>0)){
      for(i in 1:length(small_out[[1]])){
        rect(small_out[[1]][[i]] - 0.4, lim[3] + 0.03, small_out[[2]][[i]] + 0.4, lim[4] - 0.02, border = "rosybrown3", col = "rosybrown3")
      }
    }
    if(length(large_out[[1]]>0)){
      for(i in 1:length(large_out[[1]])){
        rect(large_out[[1]][[i]] - 0.4, lim[3] + 0.03, large_out[[2]][[i]] + 0.4, lim[4] - 0.02, border = "darkolivegreen3", col = "darkolivegreen3")
      }
    }
    abline(h = 5.33, lwd = 9, col = "white", xpd = TRUE)
    abline(h = 5.28, lwd = 1, col = "black")
    abline(h = 5.38, lwd = 1, col = "black")
  }
  
  #adding grey rectangles over injured periods
  if(include_injuries == TRUE){
    x = player_templates[[playerID]][["Injured"]]
    
    runs <- startend(x)
    
    if(length(runs[[1]])>0){
      for(i in 1:length(runs[[1]])){
        rect(runs[[1]][[i]] - 0.4, lim[3] + 0.03, runs[[2]][[i]] + 0.4, 5.28, border = "grey90", col = "grey90")
      }
    }
    abline(h = 5.33, lwd = 9, col = "white", xpd = TRUE)
    abline(h = 5.28, lwd = 1)
    abline(h = 5.38, lwd = 1 )
  }

  #adding gridlines for match days
  if(inclue_matches == TRUE){
    for(i in 1:211){
      if((!is.na(player_templates[[playerID]][["Activity type"]][i])) || (!is.na(player_templates[[playerID]][["Title"]][i]))){
        if((player_templates[[playerID]][["Activity type"]][i] == "Match/Competition") || (!is.na(player_templates[[playerID]][["Title"]][i])) ){
          abline(v = i, col="grey21", lwd = 2)
          if(!is.na(player_templates[[playerID]][["Work rate comparison"]][i])){
            if(player_templates[[playerID]][["Work rate comparison"]][i] == "Worse"){
              points(x = i, y = 5.58, pch = 25, col = "black", bg = "red", cex = 2.5)
            }
            if(player_templates[[playerID]][["Work rate comparison"]][i] == "Better"){
              points(x = i, y = 5.58, pch = 25, col = "black", bg = "green", cex = 2.5)
            }
            if(player_templates[[playerID]][["Work rate comparison"]][i] == "Normal"){
              points(x = i, y = 5.58, pch = 25, col = "black", bg = "white", cex = 2.5)
            }
          }
        }
      }
    }
    abline(h = 5.33, lwd = 9, col = "white", xpd = TRUE)
    abline(h = 5.28, lwd = 1, col = "black")
    abline(h = 5.38, lwd = 1, col = "black")
  }
  
  # Variables to include
  # colours based on rich8equal
  if(wellness == TRUE){
    legend_lines <- c(legend_lines, "#000041")
    legend_col <- c(legend_col, "#000041")
    legend_text <- c(legend_text, "Overall wellness")
    if(jitter == TRUE){
      points(jitter(player_templates[[playerID]][["Wellness"]]*5, factor = 0.15), col = "#000041", pch = 20)
    }
    else{
      points(player_templates[[playerID]][["Wellness"]]*5, col = "#000041", pch = 20)
    }
    lines(player_templates[[playerID]][["Wellness"]]*5, col = "light blue") 

  }
  if(mood_state == TRUE){
    legend_lines <- c(legend_lines, "#0081FF")
    legend_col <- c(legend_col, "#0081FF")
    legend_text <- c(legend_text, "Mood state")
    if(jitter == TRUE){
      points(jitter(player_templates[[playerID]][["Mood state"]], factor = 0.15), col = "#0081FF", pch = 20)
    }
    else{
      points(player_templates[[playerID]][["Mood state"]], col = "#0081FF", pch = 20)
    }
    #lines(player_templates[[playerID]][["Mood state"]], col = "light blue") might add later
  }
  if(sleep_quality == TRUE){
    legend_lines <- c(legend_lines, "black")
    legend_col <- c(legend_col, "#02DA81")
    legend_text <- c(legend_text, "Sleep quality")
    if(jitter == FALSE){
      points(player_templates[[playerID]][["Sleep quality"]], col = "black", pch = 21, bg = "#80FE1A", cex = 0.8)
    }
    else{
      points(jitter(as.numeric(player_templates[[playerID]][["Sleep quality"]]), factor = 0.15),  col = "black", pch = 21, bg = "#80FE1A", cex = 0.8)
    }
  }
  if(energy_levels == TRUE){
    legend_lines <- c(legend_lines, "darkorchid")
    legend_col <- c(legend_col, "darkorchid")
    legend_text <- c(legend_text, "Energy levels")
    if(jitter == FALSE){
      points(player_templates[[playerID]][["Energy levels"]], col = "darkorchid", pch = 20)
    }
    else{
      points(jitter(as.numeric(player_templates[[playerID]][["Energy levels"]]), factor = 0.15), col = "darkorchid", pch = 20)
    }
  }
  if(health == TRUE){
    legend_lines <- c(legend_lines, "black")
    legend_col <- c(legend_col, "#FDEE02")
    legend_text <- c(legend_text, "Health")
    if(jitter == FALSE){
      points(player_templates[[playerID]][["Health"]], col = "black", pch = 21, bg = "#FDEE02", cex = 0.8)
    }
    else{
      points(jitter(as.numeric(player_templates[[playerID]][["Health"]]), factor = 0.15), col = "black", pch = 21, bg = "#FDEE02", cex = 0.8)
    }
  }
  if(stress == TRUE){
    legend_lines <- c(legend_lines, "orange")
    legend_col <- c(legend_col, "orange")
    legend_text <- c(legend_text, "Stress")
    if(jitter == FALSE){
      points(player_templates[[playerID]][["Stress"]], col = "orange", pch = 20)
    }
    else{
      points(jitter(as.numeric(player_templates[[playerID]][["Stress"]]), factor = 0.15), col = "orange", pch = 20)
    }
  }
  if(muscle_readiness == TRUE){
    legend_lines <- c(legend_lines, "#FF3300")
    legend_col <- c(legend_col, "#FF3300")
    legend_text <- c(legend_text, "Muscle readiness")
    if(jitter == FALSE){
      points(player_templates[[playerID]][["Muscle readiness"]], col = "#FF3300", pch = 20)
    }
    else{
      points(jitter(as.numeric(player_templates[[playerID]][["Muscle readiness"]]), factor = 0.15), col = "#FF3300", pch = 20)
    }
  }
  
  # legend
  if(length(which(c(mood_state, sleep_quality, energy_levels, health, stress, muscle_readiness, wellness))) > 0 ){
    legend(x = "bottomleft", legend = legend_text, col = legend_lines , pt.bg = legend_col,  bty = "n", pch = 22)
  }
  if(inclue_matches == TRUE){
    legend(x = "bottomright", legend = c("Above", "Below", "Normal"), 
           col = c("black", "black", "black") , pt.bg = c("green", "red", "white"), 
           bg = "White", pch = 25, title = "Work rate")
  }
  
}

names <- c("P001", "P002", "P009", "P010", "P013", "P014", "P015", "P016", "P017", "P019", "P020", "P022", "P023",
           "P025", "P029", "P030", "P031", "P032", "P034", "P035", "P059", "P070", "P072", "P073", "P074", "P075", "P076",
           "P077", "P081", "P082", "P083")

#player_plot("P017", player_templates, mood_state = TRUE, include_injuries = FALSE, include_warnings = TRUE, inclue_matches = TRUE)

# app here
ui <- shinyUI(fluidPage(
  
  plotOutput('plot', click = "plot_click"),
  
  verbatimTextOutput("info"),
  
  fluidRow(
    column(3,
           selectInput("nameUI", "Select player", names)
    ),
    
    column(3,
           h4("Variables"),
           checkboxInput("wellnessUI", "Overall wellness"),
           checkboxInput("mood_stateUI", "Mood state"),
           checkboxInput("sleep_qualityUI", "Sleep quality"),
           checkboxInput("energy_levelsUI", "Energy levels"),
           checkboxInput("healthUI", "Health"),
           checkboxInput("stressUI", "Stress"),
           checkboxInput("muscle_readinessUI", "Muscle readiness")
           
    ),
    column(3, 
           h4("Other"),
           checkboxInput("include_injuriesUI", "Include injuries"),
           checkboxInput("include_matchesUI", "Include matches"),
           checkboxInput("jitterUI", "Jitter")
    ), 
    column(3, 
           h4("Wellness indicators"),
           checkboxInput("include_warningsUI", "Include warnings")
    )
    
  )
  
))


server <- function(input, output) {
  
  # the main plot output (runs the function defined above)
  output$plot <- renderPlot({
    player_plot(input$nameUI, player_templates, mood_state = input$mood_stateUI, sleep_quality = input$sleep_qualityUI ,
                energy_levels = input$energy_levelsUI, health = input$healthUI , wellness = input$wellnessUI,
                stress = input$stressUI, muscle_readiness = input$muscle_readinessUI,
                include_injuries = input$include_injuriesUI, jitter = input$jitterUI, 
                inclue_matches = input$include_matchesUI, include_warnings = input$include_warningsUI)
  })
  
  # the interactive text box
  output$info <- renderText({
    if(is.numeric(input$plot_click$x)){
      date = "NA"
      game = "No"
      rank = "NA"
      percentile = "NA"
      full = c("NA","","")
      comparison = "NA"
      if((input$plot_click$x >= 1) & (input$plot_click$x <= 211)){
        date = player_templates[[input$nameUI]][["Date"]][round(input$plot_click$x)]
        if(!is.na(player_templates[[input$nameUI]][["Title"]][round(input$plot_click$x)])){
          game = player_templates[[input$nameUI]][["Title"]][round(input$plot_click$x)]
          rank = player_templates[[input$nameUI]][["Rank"]][round(input$plot_click$x)] + 1
          num_involved = player_templates[[input$nameUI]][["Number involved"]][round(input$plot_click$x)]
          full = c(rank, "/", num_involved)
          percentile = round(player_templates[[input$nameUI]][["Percentile"]][round(input$plot_click$x)], digits = 4)
          comparison = player_templates[[input$nameUI]][["Work rate comparison"]][round(input$plot_click$x)]
        }
        else if(!is.na(player_templates[[input$nameUI]][["Activity type"]][round(input$plot_click$x)])){
          if(player_templates[[input$nameUI]][["Activity type"]][round(input$plot_click$x)] == "Match/Competition"){
          game = "Unknown" 
          }
        }
      }
      # to be printed in the interactive text box output
      paste0("Day = ", round(input$plot_click$x), " (", date, ")", 
             "\nGame = ", game, 
             "\nRank = ", full[1], full[2], full[3], 
             "\nPercentile = ", percentile,
             "\nRelative Work Rate = ", comparison)
    }
  })
  
}


shinyApp(ui, server)

