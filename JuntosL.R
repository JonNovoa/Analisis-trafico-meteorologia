library(dplyr)

# Cargar los datos desde el archivo CSV
dfAccidentesL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/AccidentesLR2.csv", header = TRUE, sep = ";", dec = ",")

# Cargar los datos desde el archivo CSV
dfMeteoL <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/MeteoLR2.csv", header = TRUE, sep = ";", dec = ",")

str(dfMeteoL)


# Realizar la unión por ANO, MES, y DIA
dfCombinado <- left_join(dfAccidentesL, dfMeteoL, by = c("ANO", "MES", "DIA"))

str(dfCombinado)



write.table(
  dfCombinado,
  file = "C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv",
  sep = ";",
  dec = ",",
  row.names = FALSE,
  col.names = TRUE,  # Agregar nombres de columna
  quote = FALSE,     # No agregar comillas alrededor de los campos
  fileEncoding = "UTF-8"
)

