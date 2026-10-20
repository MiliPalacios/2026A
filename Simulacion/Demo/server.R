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
library(openxlsx)


# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = input$color, border = 'white',
             xlab = 'Tiempo espera (min)',
             main = 'Histograma de tiempos de espera')

    })

    output$dispersion <- renderPlot({
        plot(faithful[,1], pch = input$tipo, xlab= "Tiempo", ylab = "valor", main = "Gráfico de dispersión")
    })
    
    output$resumen <-function(){
        res <- summary(faithful)
        #personalizacion
        kbl(res)|>
            kable_styling("hover",full_width = TRUE, bootstrap_options = c("condensed"), font_size = 15) |> 
            row_spec(0, color = "white", background = "#F54927", font_size = 14) 
    }
    output$dynamic <- function(){
        res <- faithful
        kbl(res)  %>%  scroll_box(width = "100%", height = "200px")
    }
    output$download1 <- downloadHandler(
        filename = function() {
            paste0("Ejemplo", ".xlsx")
        },
        content = function(file) {
            write.xlsx(faithful, file)
        }
    )

    DATOS <- reactive({
        req(input$data)
      
        readxl::read_excel(
            path = input$data$datapath,
            sheet = 1,
        )
    })
    output$resumen_datos <- function(){
        res <- summary(DATOS())
        #personalizacion
        kbl(res)|>
            kable_styling("hover",full_width = TRUE, bootstrap_options = c("condensed"), font_size = 15) |> 
            row_spec(0, color = "white", background = "#F54927", font_size = 14) 
    }
}