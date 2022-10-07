# load the necessary libraries
library(shiny)
library(ggplot2)
library(plotly)

# load the data
data(faithful)

# Define UI 
ui <- fluidPage(
  
  # App title 
  headerPanel("Old Faithful Data"),
  
  # side-by-side layout
  sidebarLayout(
    
    #  panel for inputs 
    sidebarPanel(
      
      # checkboxes to turn on/off plot elements
      # https://shiny.rstudio.com/reference/shiny/latest/checkboxInput.html
      strong("Plot Options:"), # a simple html element to provide a title for this section of the UI
      checkboxInput(
        "showContours", "Include Contours",
        value = FALSE),
      checkboxInput(
        "showFitLine", "Include Linear Fit Line",
        value = TRUE)
    ),
    
    
    # Main panel for displaying outputs 
    mainPanel(
      # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
      plotlyOutput("finalPlot", height = "500px"),
      
      # https://shiny.rstudio.com/reference/shiny/1.0.3/verbatimTextOutput.html
      verbatimTextOutput("modelSummary")
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # when we have input values that we want to use for generating output,
  # we need to wrap that portion of the code in observe({}), or another reactive container
  observe({
    # create the scatter plot
    main_plot <- ggplot(faithful, aes(eruptions, waiting)) + 
      geom_point() + 
      scale_x_continuous(limits = c(1, 5.9), expand = c(0, 0)) +
      scale_y_continuous(limits = c(40, 99), expand = c(0, 0)) +
      labs(x ="Eruption Duration (min)", y = "Time Between Eruptions (min)") +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
    # add the contours if requested by the user
    if (input$showContours) main_plot <- main_plot + geom_density2d()
    
    # add the fit line if requested by the user
    if (input$showFitLine) main_plot <- main_plot + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", se = FALSE, color = "red") # lm for linear
    
    # store the figure in the "finalPlot" key of the output variable which can be seen by the UI
    output$finalPlot <- renderPlotly(main_plot)
    
    # store the output from the linear fit in the "modelSummary" key of the output variable which can be seen by the UI
    output$modelSummary <- renderPrint(
      summary(lm(waiting ~ eruptions, data = faithful))
    )
    
  })
  
  
  
}

# Return the shiny.appobj object
shinyApp(ui, server)