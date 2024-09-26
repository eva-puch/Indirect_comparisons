#
# This is a Shiny web application. To launch it, press the 'Run App' button.
# It allows for calculating weights for MAIC adjustment.
#
# A binary variable that needs to be adjusted must be coded as 0/1 in numeric format.
# The proportion of '1' in this variable will correspond to its mean value.
#
# Continuous variables to be adjusted must be in numeric format.
#


rm(list=ls())

#--------------------   Librairies   -------------------------------------------

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(maic)
library(ggplot2)
library(tableone)
library(survey)

col1 <- "cornflowerblue"
col2 <- "darkgoldenrod1"

#--------------------   Functions   --------------------------------------------
maic_distributions <- function(table, variable, var_type, opacity){
  
  if (is.null(table) || !("maic_weights" %in% colnames(table))) {
    stop("The dataset is NULL or the 'maic_weights' column is missing.")
  }
  if (!(variable %in% colnames(table))) {
    stop(paste("The variable", variable, "is not in the dataset."))
  }
  
  df_before <- table
  df_before$maic_weights <- 1
  df_before$type <- "Before MAIC"
  df_after <- table
  df_after$type <- "After MAIC"
  df <- rbind(df_before, df_after)
  df$type <- factor(df$type, levels = c("Before MAIC", "After MAIC"))
  
  if (var_type == "numeric") { # Quantitative variable (density)
    
    plot <- ggplot(df, aes_string(x = variable, fill = "type", weight = df$maic_weights)) +
      geom_density(alpha = opacity) +
      labs(x = variable, y = "Density") +
      scale_fill_manual(name = "Legend", values = c("Before MAIC"=col1, "After MAIC"=col2)) +
      theme(legend.position = "right")
  }
  else if (var_type == "factor") { # Qualitative variable (bars)
    df[[variable]] <- as.factor(df[[variable]])
    
    plot <- ggplot(df, aes_string(x = variable, fill = "type", weight = df$maic_weights)) +
      geom_bar(position="dodge", alpha = opacity) +
      labs(x = variable, y = "") +
      scale_fill_manual(name = "Legend", values = c("Before MAIC"=col1, "After MAIC"=col2)) +
      theme(legend.position = "right")
    
  } else {
    stop("Type de variable inconnu. Assurez-vous que la variable est soit numerique soit factorielle.")
  }
  
  return(plot)
}

#--------------------   UI   ---------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    # Add CSS to customize the buttons
    tags$style(HTML("
      .align-right {
        text-align: right;
      }
      .custom-button {
        width: 100%;
        margin-bottom: 10px; /* Espacement entre les boutons */
      }
    "))
  ),
  
  titlePanel("Interactive MAIC weights calculation"),
  
  tabsetPanel(
    tabPanel("Weights calculation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("ipd_file", "Loading individual data (.csv):"),
                 textInput("variables", "Variables to be adjusted (separated by ' ; ')", value="",
                           placeholder = "age ; sex"),
                 textInput("valeurs_cibles", "Target values (separated by ' ; ')", value = "",
                           placeholder = "65 ; 0.60"),
                 textInput("type_ajustement", "Match types (separated by ' ; '): proportion, mean, median, sd, min, max", value = "",
                           placeholder = "mean ; proportion"),
                 actionButton("run", "Weights calculation", class="custom-button"),
                 downloadButton("download_data", "Add weights and export", class="custom-button"),
               ),
               mainPanel(
                 conditionalPanel(
                   "input.run > 0",
                   h3("Variables to be adjusted"),
                   tableOutput("dic_table"),
                   h3("MAIC weights"),
                   sliderInput("breaks",
                               "Number of breaks",
                               min=5, max=100, value=30, step=5),
                   plotOutput("weights_output"),
                   h5("Number of individuals in the new pseudo-population"),
                   verbatimTextOutput("sum_weights_output"),
                   h5("Number of individuals with weight 0"),
                   verbatimTextOutput("zero_weights_output"),
                   h3("Effective Sample Size (ESS)"),
                   verbatimTextOutput("ess_output")
                 )
               )
             )
    ),
    
    tabPanel("Distributions",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "Variable", choices = NULL),
                 radioButtons("var_type", "Variable type", choices = c("Quantitative" = "numeric", "Qualitative" = "factor")),
                 actionButton("distributions", "Print", class="custom-button"),
                 sliderInput("opacity",
                             "Opacity",
                             min=0, max=1, value=0.6, step=0.05)
               ),
               
               mainPanel(
                 h4("Distribution before and after MAIC"),
                 plotOutput("distributionsPlot")
               )
             )
    )
  )
)


#--------------------   SERVER   ----------------------------------------------
server <- function(input, output, session) {
  
  # Load IPD data (individual-patient data)
  ipd_df <- reactive({
    req(input$ipd_file)
    read.csv(input$ipd_file$datapath)
  })
  
  common_inputs <- reactiveValues(
    variables = "",
    valeurs_cibles = "",
    type_ajustement = ""
  )
  
  # MAIC calculation ----
  results <- eventReactive(input$run, {
    req(input$variables, input$valeurs_cibles, input$type_ajustement)
    
    # Extracting parameters
    variables <- unlist(strsplit(input$variables, " ; "))
    valeurs_cibles <- as.numeric(unlist(strsplit(input$valeurs_cibles, " ; ")))
    type <- unlist(strsplit(input$type_ajustement, " ; "))
    
    # Checking entered variable
    invalid_vars <- setdiff(variables, names(ipd_df()))
    if(length(invalid_vars) > 0){
      showNotification(paste("The following variables are not present in the dataset: ",
                             paste(invalid_vars, collapse = ", ")), type = "error")
      return(NULL)
    }
    
    # Validation of input data
    if (length(valeurs_cibles) != length(type)) {
      showNotification("Number of target values and match types do not match.", type = "error")
      return(NULL)
    }
    
    n <- length(valeurs_cibles)
    cible <- c()
    matchid <- c()
    suppl <- rep("", n)
    
    for (i in 1:n) {
      cible[paste0("Var_", i)] <- valeurs_cibles[i]
      matchid[i] <- paste0("var", i)
      if (type[i] == "sd") {
        var <- variables[i]
        j <- which(variables == var & type == "mean")
        suppl[i] <- names(cible[j])
      }
    }
    
    dic <- data.frame(
      "match.id" = matchid,
      "target.variable" = names(cible),
      "index.variable" = variables,
      "match.type" = type,
      "supplementary.target.variable" = suppl,
      stringsAsFactors = FALSE
    )
    
    # Creating MAIC input
    maic <- createMAICInput(
      index = ipd_df(),
      target = cible,
      dictionary = dic,
      matching.variables = subset(dic, select = 'match.id')
    )
    
    # Calculating weights and ESS
    wgt <- maicWeight(maic)
    ess <- sum(wgt)^2 / sum(wgt^2)
    
    # Adding weights to the dataframe
    ipd_weighted <- ipd_df()
    ipd_weighted$maic_weights <- wgt
    
    list(
      dic = dic,
      ipd_weighted = ipd_weighted,
      maic_weights = wgt,
      ess = ess,
      min_weights = min(wgt),
      max_weights = max(wgt),
      sum_weights = sum(wgt),
      zero_weights = sum(wgt == 0)
    )
  })
  
  # Generate data for distributions (triggered after "Print")
  plot_data <- eventReactive(input$distributions, {
    req(input$var)
    req(input$var_type)
    
    res <- results()  # Retrieve MAIC calculation results
    if (is.null(res)) return(NULL)
    
    # Call the maic_distribution function to generate distributions before and after MAIC
    plot <- maic_distributions(
      table = res$ipd_weighted,
      variable = input$var,
      var_type = input$var_type,
      opacity = input$opacity
    )
    
    return(plot)  # Retourner the plot
  })
  
  # Display the distribution plot in the UI
  output$distributionsPlot <- renderPlot({
    req(plot_data())  # Ensure plot_data() contains data before generating the plot
    plot_data()       # Display the plot generated by maic_distributions
  })
  
  # Displaying results ----
  output$dic_table <- renderTable({
    res <- results()
    if (is.null(res)) return(NULL)
    
    # Display of the 'dic' table columns with the corresponding target values
    cbind(res$dic[,3:4], target.value = as.numeric(unlist(strsplit(input$valeurs_cibles, " ; "))))
  })
  
  output$weights_output <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)
    
    hist(res$maic_weights, 
         breaks = input$breaks,
         col = "skyblue", 
         main = "MAIC weights histogram", 
         xlab = "Weights",
         ylab = "Frequency",
         border = "white")
  })
  
  output$ess_output <- renderText({
    res <- results()
    if (is.null(res)) return(NULL)
    
    res$ess  # Display the Effective Sample Size (ESS)
  })
  
  output$zero_weights_output <- renderText({
    res <- results()
    if (is.null(res)) return(NULL)
    
    res$zero_weights  # Display number of individuals with weight=0
  })
  
  output$sum_weights_output <- renderText({
    res <- results()
    if (is.null(res)) return(NULL)
    
    res$sum_weights  # Display total number of individuals in the pseudo-population
  })
  
  # Export data with added MAIC weights as last column
  output$download_data <- downloadHandler(
    filename = function() {
      paste("ipd_weighted_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      res <- results()
      if (is.null(res)) return()
      
      ipd <- ipd_df()
      if (length(res$maic_weights) != nrow(ipd)) {
        showNotification("Mismatch between the number of weights and the number of rows in the data.", type = "error")
        return()
      }
      
      ipd$maic_weights <- res$maic_weights
      write.csv(ipd, file, row.names = FALSE)
    }
  )
  
  # Updated selectINput options for choosing the variable to be displayed in 'Distributions'
  observe({
    updateSelectInput(session, "var", choices = names(ipd_df()))
  })
}

#--------------------   Run the application   ----------------------------------
shinyApp(ui = ui, server = server)
