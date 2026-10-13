#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(kableExtra)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = input$color, border = 'white',
             xlab = 'Tiempo de espera (min)',
             main = 'Histogramas de tiempos de espera')
    })

    output$dispersion <- renderPlot({
        plot(faithful[,1], pch = input$tipo, xlab = "Tiempo", ylab = "Valor", main = "Gradico de dispersión")
    })

    #output$resumen <- renderTable(
    #    summary(faithful)
    #)

    output$resumen <- function(){
        res <- summary(faithful)
      # Personalizacion
        kbl(res) |> 
            kable_styling(full_width = TRUE, bootstrap_options = c("condensed"), font_size = 15) |> 
            row_spec(0, color = "white", background = "#F54927", font_size = 14)
    }

    output$datos <- renderDataTable(
        faithful,
        options = list(pageLenght = 8)
    )
}
