#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Primer Aplicativo Shiny"),
    # Titulo de segundo orden
    h2("Histograma"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Número de barras:",
                        min = 1,
                        max = 40,
                        value = 10),
            selectInput("color", "Seleccione el color de las barras", c("red", "blue", "green", "orange"))
        , width = 3 ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"), 
            width = 9
        )
    ),
    h2("Dispersión"),
    sidebarLayout(
        sidebarPanel(
            numericInput("tipo", "Ingrese el tipo de ícono a usar:", value = 1, min = 1, max = 20),
            width = 3,
        ),
        mainPanel(
            plotOutput("dispersion"),
            width = 9,
        )
    ),
    h2("Resumen variables"),
    # En caso de no ser necesario el sidebarPanel y el mainPanel (sidebarLayout requiere de ambas cosas)
    fluidRow(
        column(
            tableOutput("resumen"),
            width = 6
        ),
        column(
            dataTableOutput("datos"), 
            width = 6
        )
    )

    

)
