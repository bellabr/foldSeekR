

library(shiny)

# define UI
ui <- fluidPage(
  # change title
  titlePanel("FoldSeekR: Large protein architecture comparisons"),

  sidebarLayout(
    # sidebar panel
    sidebarPanel(
      tags$p("Retrieves the top-k most similar architectures to your
             Uniprot accession identifier and displays them."),

      tags$p("Enter or select values required to perform analysis. Default
                        values are shown. Then press 'Run'."),
      textInput(inputId = "accession",
                label = "Accession:", "P00520"),
      # textInput(inputId = "k",
      #           label = "k (how many hits to display):", "3"),

      br(),

      # submit button
      actionButton(inputId = "button2",
                   label = "Run"),
    ),

    # main panel
    mainPanel(
      # output tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Output", verbatimTextOutput("textOut")),
                  tabPanel("Folds", r3dmolOutput("OutputPlot", width = "100%", height = "400px"))
      )
    )
  )
)

# define server
server <- function(input, output) {

  startrun <- eventReactive(eventExpr = input$button2, {

    prediction <- foldSeekR::pull_prediction(
      accession = input$accession
    )

    pdb_url <- foldSeekR::pull_url(
      accession = input$accession,
      url_type = "pdb"
    )

    c(prediction, pdb_url)

  })

  startplot <- eventReactive(eventExpr = input$button2, {

    vis <- foldSeekR::visualize_prediction(
      accession = input$accession
    )
    vis

  })

  # Textoutput
  output$textOut <- renderPrint({
    if (! is.null(startrun))
      startrun()
  })

  # Plotting
  output$OutputPlot <- renderR3dmol({
    if (! is.null(startplot))
      startplot()
  })

}

# create Shiny App
shiny::shinyApp(ui = ui, server = server)

# [END]
