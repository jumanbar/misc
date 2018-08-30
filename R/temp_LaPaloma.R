library(readr)
library(tidyverse)
temp <- read_delim("57285.txt", "\t", # Editar la ruta. Con la URL no me funcionó (http://www.seanoe.org/data/00445/55639/data/57285.txt)
                   escape_double = FALSE, trim_ws = TRUE)

names(temp)[c(4, 5)] <- c("hour", "Temp")

temp <- mutate(temp, datetime = ymd_h(paste0(Year, "-", Month, "-", Day, " ", hour)))

ggplot(temp, aes(x = datetime, y = Temp, col=month(temp$Month, label = T, abbr = F))) +
  geom_point() + theme(legend.title=element_blank()) + 
  scale_color_discrete() + 
  xlab("Fecha") + ylab("Temperatura | 0.5m (ºC)") + 
  ggtitle("La Paloma - 34.65ºS, 54.12ºW")
