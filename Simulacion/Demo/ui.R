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

    #Titulo
    fluidRow(
        column(width = 2,tags$img(src ='max.jpg',width = '210px', height = '90px')),
        column(width = 8, h1("Aplicativo Simulación",style = 'text-align:center; color:#132b60; padding:25px; font-size:2.7em; font-family:roman')),
        column(width = 2,tags$img(src ='max.png',width = '210px', height = '90px'), class = "text-right")
    ),
    tags$style('h2 {color:#2a406f; font-family: roman}'),
    tags$style('h3 {color:#42557f; font-family: roman}'),
    tags$style('h4 {color:#596a8f; font-family: roman}'),

    navbarPage("Opciones",
        tabPanel("Resumen",
        # Application title
        titlePanel("Primer Aplicativo Shiny"),
        h2("Histograma"),
        h3('Carga de Datos'),
        fileInput("data","cargue sus datos:", accept = c(".txt",".csv", ".xlsx")),
        h3("Resumen de los datos cargados"),
        tableOutput("resumen_datos"),
        h4('Aun no definido'),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                sliderInput("bins",
                            "Número de barras:",
                            min = 1,
                            max = 40,
                            value = 10),
                selectInput("color", 
                "Seleccione el color de las barras:", choices = c(
                "Rojo"    = "red",
                "Azul"    = "blue",
                "Verde"   = "green",
                "Naranja" = "orange"
        ))
            ),

            mainPanel(
                width = 9,
                plotOutput("distPlot")
            )
        ),
        
        h2("Dispersión"),
        sidebarLayout(
            sidebarPanel(
                numericInput("tipo","Ingrese el tipo de ícono a usar:", min = 1, max = 20, value = 1),
                width = 3,
            ),
            mainPanel(
                plotOutput("dispersion"),
                width = 9,
            )  
        ),

        h2("Resumen Variables"),
        fluidRow(
        column(
            tableOutput("resumen"),
            width = 6
        ),
        column(
            tableOutput("dynamic"),
            width =6
        ),
        downloadButton("download1"),
        br(),
        br(),
        br(),
        br(),
        h5("Derechos reservados")
        )

            ),
            tabPanel("Modelos"),
            tabPanel("Validación"),
            navbarMenu("Distribuciones",
                        tabPanel("Uniforme",
                                h2("Distribución uniforme"),),
                        tabPanel("Bionomial",
                                h2("Distribución binomiral"),),
                        tabPanel("Poisson",
                                h2("Distribución poisson"),),
                    )
        )
    

)