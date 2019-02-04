library(readxl)
importacion <-
  read_excel("~/Producto 4 - ingreso datos/interfaz_validacion_mockup/ej_santa_lucia_dic2016.xlsx")
  
LCLD <- names(importacion) %>% 
  grep("^L[CD]_*", ., ignore.case = FALSE, value = TRUE)

newdata <- select(importacion, -LCLD)

ej <- tabla_sustitucion %>% 
  filter(Original %in% names(newdata))

dim(ej)
dim(newdata)

sustituciones <- function(x, ts) {
  # Cambia los nombres de las columnas de la tabla, utilizando la
  # tabla_sustituciones como referencia para unificar criterios.
  # Devuelve nombres de columnas cambiados.
  
  # x : nombres de las columnas de una tabla
  require(magrittr)
  # cl <- x %>% limpiaNombres
  i <- 1
  out <- x
  tmp <- ""
  while (i <= length(x)) {
    if (!grepl("^L[CD]", x[i])) {
      out[i] <- tabla_sustitucion$Sustituto[tabla_sustitucion$Original == x[i]]
      tmp <- out[i]
      i <- i + 1
    } else {
      out[i] <- paste0(tmp, "_LD")
      out[i + 1] <- paste0(tmp, "_LC")
      i <- i + 2
    }
  }
  out
}
newnames <- sustituciones(names(newdata), tabla_sustitucion)

names(newdata) <- newnames

newdata <- mutate_at(newdata, vars(-Programa:-Usuario), as.numeric)

newdata <- add_column(newdata, ID = 1:nrow(newdata), .before = 1)

save(newdata, file = "newdata.RData")
