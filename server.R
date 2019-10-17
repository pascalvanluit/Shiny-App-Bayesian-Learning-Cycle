##################
####THE SERVER####
##################


shinyServer(function(input, output) {
  
  #Computing the total amount of observations:
  Total <- reactive(input$successes + input$failures)
  
  #Computing the observed success rate:
  ObsSuccessRate <- reactive(round((input$successes / (input$successes + input$failures)), digits = 2))
  
  #Inputs needed:
  dataset <- reactive(list(k = input$successes,
                           n = input$successes + input$failures))
  
  prediction_options <- list(n_prediction           = NULL,    #in case sample size differes from n
                             log_scale              = FALSE,
                             predict_from_posterior = FALSE)     #predictions from prior or posterior? only relevant in continuous case
  
  hypotheses_categorical <- reactive(data.frame(label                   = sapply(1:values$num_hypotheses, function(i) input[[paste0("N", i)]]),
                                                rate                    = sapply(1:values$num_hypotheses, function(i) input[[paste0("H", i)]]),
                                                prior_model_probability = sapply(1:values$num_hypotheses, function(i) input[[paste0("P", i)]])))
  
  theta0 <- data.frame(label                   = c("H0"),
                       rate                    = 0.5,
                       prior_model_probability = 0.5)
  
  results_table <- reactive(.hypothesesTable(hypothesis_table =  hypotheses_categorical(),
                                             dataset = dataset()))
  
  ppmpplot <- reactive(.priorPosteriorModelProbabilityPlot(results_table = results_table(),
                                                           posterior_is_included = TRUE))
  
  
  #Making output with the total amount of observations:
  output$Total <- renderText({
    paste("Total Observations:", Total())
  })
  
  #Making output with the observed success rate:
  output$ObsSuccessRate <- renderText({
    paste("Observed Success Rate:", ObsSuccessRate())
  })
  
  #Making a table which summarizes information about the hypotheses:
  output$.hypothesesTable <- renderTable(
    results_table()
  )
  
  #Making the prior and posterior model probability plot:
  output$.priorPosteriorModelProbabilityPlot <- renderPlot(
    ppmpplot()
  )

  
##################################################  
#Making the Add and Remove button for hypotheses:# 
##################################################
  
  #Keep track of the number of hypotheses:
  values <- reactiveValues(num_hypotheses = 0)
  
  #Add a hypothesis
  observeEvent(input$add, ignoreNULL = FALSE, {
    values$num_hypotheses <- values$num_hypotheses + 1
    insertUI(
      selector = "#hypotheses", where = "beforeEnd",
      splitLayout(
        cellWidths = c("45%","25%","25%"), 
        cellArgs = list(style = "padding: 3px"),
        id = paste0("hypothesis", values$num_hypotheses),
        
        #Text input to name the hypotheses:
        textInput(inputId = paste0("N", values$num_hypotheses),
                  label = paste0(values$num_hypotheses,". Name"),
                  value = ""),
        
        #Input for prior values:
        numericInput(inputId = paste0("P", values$num_hypotheses),
                  label = paste0("Prior:"),
                  value = 0.5, min = 0, max = 1, step = 0.01),
        
        #Input for hypothesis values:
        numericInput(inputId = paste0("H", values$num_hypotheses) ,
                  label = paste0("Hypothesis:"),
                  value = 0.5, min = 0, max = 1, step = 0.01)
      )
    )
  })
  
  #Remove a hypothesis
  observeEvent(input$remove, {
    
    #Don't let the user remove the very first hypothesis
    if (values$num_hypotheses == 1) {
      return()
    }
    removeUI(selector = paste0("#hypothesis", values$num_hypotheses))
    values$num_hypotheses <- values$num_hypotheses - 1
  })
  
  
  
 })
