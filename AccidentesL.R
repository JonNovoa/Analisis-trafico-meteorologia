library(dplyr)
library(ggplot2)

# Cargar los datos desde el archivo CSV
dfAccidentes <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR/datosAccidentes/accidentalidad.csv", header = TRUE, sep = ";")

# Convertir la columna fecha a un objeto de fecha y extraer año, mes y día
dfAccidentes$fecha <- as.Date(dfAccidentes$fecha, format = "%d/%m/%Y")
dfAccidentes$ANO <- format(dfAccidentes$fecha, "%Y")
dfAccidentes$MES <- format(dfAccidentes$fecha, "%m")
dfAccidentes$DIA <- format(dfAccidentes$fecha, "%d")

# Seleccionar las columnas deseadas usando select
columnas_deseadas <- c("num_expediente", "DIA", "MES", "ANO", "hora", "distrito", "tipo_accidente","tipo_vehiculo", "rango_edad", "positiva_alcohol")
datos_combinados_df <- dfAccidentes %>% 
  select(columnas_deseadas)

# Eliminar filas duplicadas
datos_combinados_df <- distinct(datos_combinados_df)

# Limpiar caracteres especiales en las columnas
datos_combinados_df$distrito <- iconv(datos_combinados_df$distrito, to = "ASCII//TRANSLIT")
datos_combinados_df$tipo_accidente <- iconv(datos_combinados_df$tipo_accidente, to = "ASCII//TRANSLIT")
datos_combinados_df$rango_edad <- iconv(datos_combinados_df$rango_edad, to = "ASCII//TRANSLIT")

# Eliminar filas con NA en columnas específicas
datos_combinados_df <- datos_combinados_df %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter_all(all_vars(. != "NULL"))

# Mostrar las primeras filas del dataframe resultante
head(datos_combinados_df)

# Guardar el dataframe en un archivo CSV
write.table(
  datos_combinados_df,
  file = "C:/Users/somor/OneDrive/Escritorio/ProyR2/AccidentesLR2.csv",
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,  # Agregar nombres de columna
  quote = FALSE,     # No agregar comillas alrededor de los campos
  fileEncoding = "UTF-8"
)

