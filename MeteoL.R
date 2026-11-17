library(dplyr)
library(tidyr)

# Cargar datos
dfMeteo <- read.csv("C:/Users/somor/OneDrive/Escritorio/ProyR/datosMeteo/meteo.csv", header = TRUE, sep = ";")

# Filtrar y transformar datos
dfMeteo_filtrado <- dfMeteo %>%
  filter(magnitud %in% c(81, 86)) %>%
  select(magnitud, ano, mes, dia, starts_with("h")) %>%
  mutate(across(starts_with("h"), ~as.numeric(gsub(",", ".", as.character(.))))) %>%
  rowwise() %>%
  mutate(media = ifelse(all(c_across(starts_with("h")) == 0), NA, 
                        ifelse(magnitud == 86, 
                               mean(c_across(starts_with("h"))[c_across(starts_with("h")) != 0], na.rm = TRUE), 
                               mean(c_across(starts_with("h")), na.rm = TRUE)))) %>%
  ungroup() %>%
  group_by(ano, mes, dia, magnitud) %>%
  summarise(media_dia = mean(media, na.rm = TRUE)) %>%
  pivot_wider(names_from = magnitud, values_from = media_dia, names_prefix = "media_magnitud_") %>%
  rename(
    ANO = ano,
    MES = mes,
    DIA = dia,
    VIENTO = media_magnitud_81,
    HUMEDAD_RELATIVA = media_magnitud_86
  ) %>%
  mutate(HUMEDAD_RELATIVA = as.numeric(HUMEDAD_RELATIVA)) %>%
  select(ANO, MES, DIA, VIENTO, HUMEDAD_RELATIVA)

# Eliminar filas con NA en columnas específicas
datos_combinados_df <- datos_combinados_df %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter_all(all_vars(. != "NULL"))

# Verificar la estructura del nuevo dataframe
str(dfMeteo_filtrado$HUMEDAD_RELATIVA)



# Guardar el dataframe en un archivo CSV
write.csv2(
  dfMeteo_filtrado,
  file = "C:/Users/somor/OneDrive/Escritorio/ProyR2/MeteoLR2.csv",
  sep = ";",
  dec = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

