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

    #Title
    fluidRow(
        column(width = 2, tags$img(src = "poliperro.jpg", width = "210px", height = "90px"), class = "text-left"), 
        column(width = 8, h1("Aplicativo Simulación", style = "text-align:center; color: #132b60; padding:25px; font-size:2.7em; font-family:roman")),
        column(width = 2, tags$img(src = "poliperro.jpg", width = "210px", height = "90px"), class = "text-right")
    ),

    # Personalizacion de titulos para todo el archivo
    tags$style("h1 {color:#132b60; font-family:roman}"),
    tags$style("h2 {color:#2a406f; font-family:roman}"),
    tags$style("h3 {color:#42557f; font-family:roman}"),
    tags$style("h4 {color:#596a8f; font-family:roman}"),
    tags$style("h5 {color:#717f9f; font-family:roman}"),

    # Application title
    #titlePanel("Primer Aplicativo Shiny"),


    # Titulo de segundo orden
    h2("Histograma"),
    h3("Pendiente por definir"),
    h4("Aun no definido"),
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
