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

        # panel for inputs
        sidebarPanel(

        ),


        # Main panel for displaying outputs
        mainPanel(
            # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
            plotlyOutput("finalPlot", height = "500px")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    # create the scatter plot
    main_plot <- ggplot(faithful, aes(eruptions, waiting)) + geom_point()

    # add the contours
    main_plot <- main_plot + geom_density2d()

    # add the fit line 
    main_plot <- main_plot + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", se = FALSE, color = "red") # lm for linear


    # store the figure in the "finalPlot" key of the output variable which can be seen by the UI
    output$finalPlot <- renderPlotly(main_plot)

}

# Return the shiny.appobj object
shinyApp(ui, server)