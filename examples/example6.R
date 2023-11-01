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
            
            # dropdown to choose which marginal plots to show
            # https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
            selectInput(
                "marginsToShow", "Marginal Axes to Plot:",
                c("x","y","both", "none"),
                selected = "both"
            ),
            
            # dropdown to choose the type of marginal distribution
            # https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalPanel.html
            conditionalPanel(
                condition = "input.marginsToShow != 'none'" ,
                selectInput(
                    "marginalFormat", "Marginal Format:", 
                    c("histogram", "density"),
                    selected = "histogram"
                )
            ),
            
            # slider to choose the bin size for the x axis (only relevant for histogram margin type)
            # https://shiny.rstudio.com/reference/shiny/latest/sliderInput.html
            sliderInput(
                "xbinwidth", "Eruption Duration binwidth (min.):", 
                min = 1e-3, max = 1, value = 0.1
            ),
            
            # checkboxes to turn on/off plot elements
            # https://shiny.rstudio.com/reference/shiny/latest/checkboxInput.html
            strong("Additional Plot Options:"), # a simple html element to provide a title for this section of the UI
            checkboxInput(
                "showContours", "Include Contours",
                value = FALSE
            ),
            checkboxInput(
                "showFitLine", "Include Linear Fit Line",
                value = TRUE
            )
        ),

        # Main panel for displaying outputs 
        mainPanel(
            # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
            plotlyOutput("finalPlot", height = "500px"),
            
            conditionalPanel(
                condition = "input.showFitLine",
                # https://shiny.rstudio.com/reference/shiny/1.0.3/verbatimTextOutput.html
                verbatimTextOutput("modelSummary")
            )
            
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
            labs(x ="Eruption Duration (min.)", y = "Time Between Eruptions (min.)") +
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

        # add the contours if requested by the user
        if (input$showContours) main_plot <- main_plot + geom_density2d()

        # add the fit line if requested by the user
        if (input$showFitLine) main_plot <- main_plot + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", se = FALSE, color = "red") # lm for linear

        f <- main_plot
        # add the marginal plots if desired
        if (input$marginsToShow != "none"){
            
            # set up empty plots that will hold the marginal distributions
            top_plot <- ggplot(faithful, aes(eruptions)) +
                scale_x_continuous(limits = c(1, 5.9), expand = c(0, 0)) +
                scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
                theme_classic() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
            
            right_plot <- ggplot(faithful, aes(waiting)) + coord_flip() + 
                scale_x_continuous(limits = c(40, 99), expand = c(0, 0)) + 
                scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
                theme_classic() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
            
            # add the histograms or density plots
            if (input$marginalFormat == 'histogram'){
                top_plot <- top_plot + geom_histogram(binwidth = input$xbinwidth)
                right_plot <- right_plot + geom_histogram(binwidth = 1)
            }
            
            if (input$marginalFormat == 'density'){
                top_plot <- top_plot + geom_density()
                right_plot <- right_plot + geom_density()
            }
            
            if (input$marginsToShow == "y") top_plot <- plotly_empty()
            if (input$marginsToShow == "x") right_plot <- plotly_empty()
            
            f <- subplot(
                top_plot, plotly_empty(), main_plot, right_plot, 
                nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
                shareX = TRUE, shareY = TRUE
            )
            
        }


        # store the figure in the "finalPlot" key of the output variable which can be seen by the UI
        output$finalPlot <- renderPlotly(f)

        # store the output from the linear fit in the "modelSummary" key of the output variable which can be seen by the UI
        output$modelSummary <- renderPrint(
            summary(lm(waiting ~ eruptions, data = faithful))
        )

    })

}

# Return the shiny.appobj object
shinyApp(ui, server)