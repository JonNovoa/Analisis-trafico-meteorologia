

# Cargar los datos desde el archivo CSV
dfAccidentesL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";")


dfAccidentesL$fecha_hora <- as.POSIXct(paste(dfAccidentesL$ANO, dfAccidentesL$MES, dfAccidentesL$DIA, dfAccidentesL$hora), format="%Y %m %d %H:%M:%S")

# Definir la interfaz de usuario de la aplicación Shiny
ui <- fluidPage(
  titlePanel("Número de Accidentes por Mes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ano", "Seleccionar Año", choices = c("2021", "2022", "2023", "Todos"))
    ),
    mainPanel(
      plotlyOutput("grafico"),
      textOutput("total_accidentes")  # Nuevo elemento para mostrar el total
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  # Filtrar los datos según el año seleccionado
  datos_filtrados <- reactive({
    if (input$ano == "Todos") {
      return(dfAccidentesL)
    } else {
      return(filter(dfAccidentesL, ANO == as.numeric(input$ano)))
    }
  })
  
  # Calcular el número total de accidentes únicos
  total_accidentes <- reactive({
    unique_accidents <- unique(datos_filtrados()$num_expediente)
    return(length(unique_accidents))
  })
  
  # Crear el gráfico interactivo
  output$grafico <- renderPlotly({
    grafico <- datos_filtrados() %>%
      count(MES) %>%
      plot_ly(
        x = ~factor(MES),
        y = ~n,
        type = "bar",
        text = ~paste("Número de Accidentes: ", n),
        opacity = 0.7,
        marker = list(color = "skyblue")  # Cambiar el color de las barras
      ) %>%
      layout(
        title = "Número de Accidentes por Mes",
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Número de Accidentes"),
        font = list(color = "darkblue"),  # Cambiar el color del texto
        paper_bgcolor = "lightgray",       # Cambiar el fondo del gráfico
        plot_bgcolor = "white"             # Cambiar el fondo de la trama
      )
    
    return(grafico)
  })
  
  # Mostrar el número total de accidentes únicos
  output$total_accidentes <- renderText({
    return(paste("Número total de accidentes:", total_accidentes()))
  })
}

# Crear la aplicación Shiny
shinyApp(ui, server)
