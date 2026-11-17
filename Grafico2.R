library(shiny)
library(shinydashboard)

# Cargar los datos desde el archivo CSV
dfAccidentesL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";")

# Contar el número de accidentes únicos
num_accidentes <- length(unique(dfAccidentesL$num_expediente))

# Imprimir el número total en la consola
cat("Número total de accidentes:", num_accidentes, "\n")

# Formatear el número con comas para mejorar la legibilidad
num_accidentes_formateado <- format(num_accidentes, big.mark = ",")

# Crear la aplicación Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Número total de accidentes"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Accidentes",
        valueBox(
          value = num_accidentes_formateado,
          subtitle = "Total",
          icon = icon("car")
        )
      )
    )
  )
)

# Ejecutar la aplicación
shinyApp(ui, server = function(input, output) {})
