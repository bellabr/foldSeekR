

library(shiny)

# define UI
ui <- fluidPage(
  # change title
  titlePanel("Displays similar protein architectures"),

  sidebarLayout(
    # sidebar panel
    sidebarPanel(
      tags$p("Calculates BIC, ICL, and AIC values,  given log-likelihood,
             number of clusters, dimension of dataset, and number of observations,
             and the probability. See ?InfCriteriaCalculation for details
             of arguments required. "),

      tags$p("Enter or select values required to perform analysis. Default
                        values are shown. Then press 'Run'."),
      textInput(inputId = "logL",
                label = "Enter loglikelihood value", "-5080"),
      textInput(inputId = "nClusters",
                label = "Enter nClusters (Note: nClusters and length(probability) should match):", "2"),
      textInput(inputId = "dimensionality",
                label = "Enter dimensionality of original dataset:", "6"),
      textInput(inputId = "observations",
                label = "Enter observations in original dataset:", "100"),
      textInput(inputId = "probability",
                label = "Enter probability of each cluster (Note, should sum to 1):", "0.5, 0.5"),

      br(),

      # submit button
      actionButton(inputId = "button2",
                   label = "Run"),
    ),

    # main panel
    mainPanel(
      #
      tabsetPanel(type = "tabs",
                  tabPanel("Output", verbatimTextOutput("textOut")),
                  tabPanel("Plot", plotOutput("OuputPlot"))
      )
    )
  )
)

# define server
server <- function(input, output) {
  # Calculate information criteria value
  startcalculation <- eventReactive(eventExpr = input$button2, {

    TestingPackage::InfCriteriaCalculation(
      loglikelihood = as.numeric(input$logL),
      nClusters = as.numeric(input$nClusters),
      dimensionality = as.numeric(input$dimensionality),
      observations = as.numeric(input$observations),
      probability = as.numeric(unlist(strsplit(input$probability, ","))))

  })

  # Textoutput
  output$textOut <- renderPrint({
    if (! is.null(startcalculation))
      startcalculation()
  })


  # Plotting
  output$OuputPlot <- renderPlot({
    if (! is.null(startcalculation))
      TestingPackage::InfCriteriaPlot(infValues =  startcalculation())
  })

}

# create Shiny App
shiny::shinyApp(ui = ui, server = server)

# [END]
