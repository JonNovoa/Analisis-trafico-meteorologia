library(dplyr)
library(ggplot2)

# Lee el archivo CSV
dfAccidentesL2 <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR2/JuntosLR2.csv", header = TRUE, sep = ";", dec = ",")

# Convertir la columna HUMEDAD_RELATIVA a numérica y dividirla por 1000 para pasar a millares
dfAccidentesL2$HUMEDAD_RELATIVA <- as.numeric(dfAccidentesL2$HUMEDAD_RELATIVA)
dfAccidentesL2$HUMEDAD_RELATIVA <- dfAccidentesL2$HUMEDAD_RELATIVA / 1000

# Crear un nuevo dataframe con la suma de HUMEDAD_RELATIVA y el número de accidentes por MES
resultados_combinados <- dfAccidentesL2 %>%
  group_by(MES) %>%
  summarise(Suma_HUMEDAD_RELATIVA = sum(HUMEDAD_RELATIVA),
            Num_Accidentes = n_distinct(num_expediente))

# Filtrar los resultados para incluir solo los meses del 1 al 12
resultados_combinados <- resultados_combinados %>% filter(MES %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Convertir MES a factor para ordenar correctamente
resultados_combinados$MES <- factor(resultados_combinados$MES, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# Crear un gráfico de dispersión
ggplot(resultados_combinados, aes(x = Suma_HUMEDAD_RELATIVA, y = Num_Accidentes, group = MES, color = MES)) +
  geom_line(size = 3) +
  geom_point(size = 3) +
  labs(title = "Relación entre HUMEDAD_RELATIVA y Número de accidentes por MES",
       x = "HUMEDAD_RELATIVA (en millares)",
       y = "Número de accidentes") +
  theme_minimal()
