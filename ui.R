############
###THE UI###
############

ui <- fluidPage(
  withMathJax(), #Mathjax is used for the formula syntax
  titlePanel("The Bayesian Learning Cycle", windowTitle = "The Bayesian Learning Cycle"), #Title of the Shiny app
  strong("Applied to a Binomial Example"), #Subtitle
  setBackgroundColor("Azure"), #Setting a background colour
  sidebarLayout(
    sidebarPanel(
      h3(strong(tags$u("Observed Data:"))),
      tags$div(
      fluidRow(
        #Creating input fields for observed data.
        div(style = "display: inline-block; width: 125px; margin-left: 15px",
               numericInput(inputId = "successes", label = "No. of Successes", value = 7, min = 0, max = 100, step = 1)),
        div(style = "display: inline-block; width: 125px; margin-left: 10px",
               numericInput(inputId = "failures", label = "No. of Failures", value = 3, min = 0, max = 100, step = 1))
        ),
      
      style = "background-color: #F0FFF0;
                      height: 80px;
                      width: 300px;
                      margin: 10px;
                      margin-left: 0px;
                      padding: 5px;
                      display: block;
                      border-radius: 5px;
                      border: 2px solid #2a334f;"
      ),
      br(),
      
      # Buttons to add/remove a hypothesis
      actionBttn("add", "Add Hypothesis", style = "simple", size = "s", color = "success"),
      actionBttn("remove", "Remove Last Hypothesis", style = "simple", size = "s", color = "primary"),
      h5("*Give each hypothesis a unique name*"),
      div(id = "hypotheses",
          style = "border: 1px solid silver;
                   background-color: #AFEEEE;
                      width: 360px;
                      margin: 10px;
                      padding: 5px;
                      display: block;
                      border-radius: 5px;
                      border: 2px solid #2a334f;")
      
    ),
    
    mainPanel(
      h3("Recall the formula for the posterior distribution:"),
      h3(helpText("$$\\text{posterior} = \\frac{\\text{prior} * \\text{likelihood}}{\\text{marginal likelihood}}$$")),
      br(),
      h4(textOutput("Total")),
      h4(textOutput("ObsSuccessRate")),
      br(),
      tableOutput(".hypothesesTable"),
      plotOutput(".priorPosteriorModelProbabilityPlot"),
      verbatimTextOutput("inputlist")
    )
  )
)