library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)
library(gridExtra)

# Cargar los datos desde el archivo CSV
dfAccidentesL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";")

dfAccidentesL$fecha_hora <- as.POSIXct(paste(dfAccidentesL$ANO, dfAccidentesL$MES, dfAccidentesL$DIA, dfAccidentesL$hora), format="%Y %m %d %H:%M:%S")

# Definir la interfaz de usuario de la aplicación Shiny
ui <- fluidPage(
  titlePanel("Análisis de Accidentes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ano", "Seleccionar Año", choices = c("2021", "2022", "2023", "Todos"))
    ),
    mainPanel(
      plotlyOutput("grafico"),
      textOutput("total_accidentes"),  # Nuevo elemento para mostrar el total
      plotOutput("circ_bar_plot"),
      plotOutput("line_plot_humedad"),
      plotOutput("line_plot_num_accidentes"),
      plotlyOutput("line_plot_humedad_accidentes")
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
  
  # Filtrar y contar el número de accidentes únicos para el gráfico circular
  datos_filtrados_tipo_accidente <- reactive({
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
  
  # Calcular porcentaje para el gráfico circular
  dfAccidentesUnicos <- reactive({
    datos_filtrados_tipo_accidente() %>%
      mutate(porcentaje = (num_accidentes / sum(num_accidentes)) * 100) %>%
      arrange(desc(num_accidentes))
  })
  
  # Seleccionar los primeros 5 tipos de accidentes para el gráfico circular
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
  
  # Crear gráfico de líneas para la suma de HUMEDAD_RELATIVA por MES
  output$line_plot_humedad <- renderPlot({
    ggplot(resultados, aes(x = MES, y = Suma_HUMEDAD_RELATIVA, group = 1)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Suma de HUMEDAD_RELATIVA por MES",
           x = "MES",
           y = "Suma de HUMEDAD_RELATIVA") +
      theme_minimal()
  })
  
  # Crear gráfico de líneas para el número de accidentes por MES
  output$line_plot_num_accidentes <- renderPlot({
    ggplot(num_accidentes, aes(x = MES, y = Num_Accidentes, group = 1)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Número de accidentes por MES",
           x = "MES",
           y = "Número de accidentes") +
      theme_minimal()
  })
  
  # Crear gráfico de líneas para la relación entre HUMEDAD_RELATIVA y número de accidentes por MES
  output$line_plot_humedad_accidentes <- renderPlotly({
    ggplot(resultados_combinados, aes(x = Suma_HUMEDAD_RELATIVA, y = Num_Accidentes, group = MES, color = MES)) +
      geom_line(size=3) +
      geom_point(size=3) +
      labs(title = "Relación entre HUMEDAD_RELATIVA y Número de accidentes por MES",
           x = "HUMEDAD_RELATIVA (en millares)",
           y = "Número de accidentes") +
      theme_minimal()
  })