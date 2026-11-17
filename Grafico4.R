# Carga el paquete dplyr
library(dplyr)
library(ggplot2)

# Lee el archivo CSV
dfAccidentesL2 <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";", dec = ",")

# Convertir la columna HUMEDAD_RELATIVA a numérica y dividirla por 1000 para pasar a millares
dfAccidentesL2$HUMEDAD_RELATIVA <- as.numeric(dfAccidentesL2$HUMEDAD_RELATIVA)
dfAccidentesL2$HUMEDAD_RELATIVA <- dfAccidentesL2$HUMEDAD_RELATIVA / 1000

# Crear un nuevo dataframe con las sumas por MES (excluyendo "Total general")
resultados <- dfAccidentesL2 %>%
  group_by(MES) %>%
  summarise(Suma_HUMEDAD_RELATIVA = sum(HUMEDAD_RELATIVA))

# Filtrar los resultados para incluir solo los meses del 1 al 12
resultados <- resultados %>% filter(MES %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Convertir MES a factor para ordenar correctamente
resultados$MES <- factor(resultados$MES, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Grafico de lineas
ggplot(resultados, aes(x = MES, y = Suma_HUMEDAD_RELATIVA, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "HUMEDAD_RELATIVA por MES",
       x = "MES",
       y = "HUMEDAD_RELATIVA") +
  theme_minimal()

