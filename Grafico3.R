library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# Cargar los datos desde el archivo CSV
dfAccidentesL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";")

# Obtener la lista de años únicos
anios_unicos <- unique(dfAccidentesL$ANO)

# UI definition
ui <- fluidPage(
  titlePanel("Top 5 de Tipos de Accidente"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ano", "Seleccionar Año", choices = c("2021", "2022", "2023", "Todos"), selected = "Todos")
    ),
    mainPanel(
      plotOutput("circ_bar_plot")
    )
  )
)

# Server definition
server <- function(input, output) {
  
  # Filtrar y contar el número de accidentes únicos
  datos_filtrados <- reactive({
    if (input$ano == "Todos") {
      dfAccidentesL %>%
        distinct(num_expediente, .keep_all = TRUE) %>%
        group_by(tipo_accidente) %>%
        summarise(num_accidentes = n())
    } else {
      dfAccidentesL %>%
        filter(ANO == as.numeric(input$ano)) %>%
        distinct(num_expediente, .keep_all = TRUE) %>%
        group_by(tipo_accidente) %>%
        summarise(num_accidentes = n())
    }
  })
  
  # Calcular porcentaje
  dfAccidentesUnicos <- reactive({
    datos_filtrados() %>%
      mutate(porcentaje = (num_accidentes / sum(num_accidentes)) * 100) %>%
      arrange(desc(num_accidentes))
  })
  
  # Seleccionar los primeros 5 tipos de accidentes
  dfAccidentesTop5 <- reactive({
    head(dfAccidentesUnicos(), 5)
  })
  
  # Crear gráfico circular para el top 5
  output$circ_bar_plot <- renderPlot({
    ggplot(dfAccidentesTop5(), aes(x = "", y = porcentaje, fill = tipo_accidente)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "bottom") +
      ggtitle("Top 5 de Tipos de Accidente") +
      geom_text(aes(label = scales::percent(porcentaje/100), y = porcentaje), position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Tipo de Accidente"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

