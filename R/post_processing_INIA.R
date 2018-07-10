# coding=utf-8
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# ==== Importar y limpiar los datos ====
pr <- read_delim("C:/Users/juan.barreneche/Downloads/datos_INIA.csv", ";", escape_double = FALSE, 
                 col_types = cols(Fecha = col_date(format = "%d-%m-%Y")), 
                 locale = locale(encoding = "ISO-8859-1", 
                                 asciify = TRUE), trim_ws = TRUE)
View(pr)

names(pr) <- c("id", "Fecha", "Estacion", "TempProm24",
               "TempMax", "TempMin", "TempMinCesped",
               "HumProm24", "TanqueA", "Penman", "Precip",
               "Viento", "Heliofania", "HorasFrioWeinberger",
               "UnidadesFrioRichardson", "HeladasMeteo", 
               "HeladasAgrometeo",
               "UnidadesTermicasDd50Arroz", "TempMin15")


# ---- Funciones auxiliares ----
tf <- function(x) sub("ºC", "", x) %>% as.numeric
pf <- function(x) sub("mm", "", x) %>% as.numeric
hf <- function(x) {
  x[x == "Agrometeorológica" | x == "Meteorológica"] <- NA
  return(as.integer(x))
}

pr <- mutate(pr,
            Estacion = factor(Estacion),
            TempProm24 = tf(TempProm24),
            TempMax = tf(TempMax),
            TempMin = tf(TempMin),
            TempMinCesped = tf(TempMinCesped),
            HumProm24 = sub("%", "", HumProm24) %>% as.numeric,
            TanqueA = pf(TanqueA),
            Penman = pf(Penman),
            Precip = pf(Precip),
            Viento = sub("Km/24h.", "", Viento) %>% as.numeric,
            Heliofania = sub("hs.", "", Heliofania) %>% as.numeric,
            HeladasMeteo = hf(HeladasMeteo),
            HeladasAgrometeo = hf(HeladasAgrometeo)
            )

# ==== Precipitaciones diarias ====
# Todas las estaciones:
p <- ggplot(pr, aes(x = Precip)) + geom_histogram()
p + ggtitle("Histograma de precipitaciones diarias, todas las estaciones") +
  xlab("Precipitación diaria (mm)") + ylab("Frecuencia")
p + scale_x_log10() + ggtitle("Histograma de precipitaciones diarias, todas las estaciones (escala Log10)") +
  xlab("Precipitación diaria (mm)") + ylab("Frecuencia")

# Sólo estación Estación Glencoe
p <- ggplot(filter(pr, Estacion == "Estación Glencoe"), aes(x = Precip)) + geom_histogram()
p + ggtitle("Histograma de precipitaciones diarias, Estación Glencoe") +
  xlab("Precipitación diaria (mm)") + ylab("Frecuencia")
p + scale_x_log10() + ggtitle("Histograma de precipitaciones diarias, Estación Glencoe (escala Log10)") +
  xlab("Precipitación diaria (mm)") + ylab("Frecuencia")
