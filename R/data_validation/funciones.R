gg_color_hue <- function(n) {
  require(grDevices)
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

inf.rm <- function(x) {
  x[is.finite(x)]
}

lindo_num <- function(x) {
  # x : vector de breaks
  number(x, accuracy = min(x))
  # format(x, big.mark = ",", decimal.mark = ".",
  #  scientific = FALSE, trim = TRUE)
}

cut2 <- function(x, breaks = 30) {
  # x <- log10(dt$PT)
  # x <- inf.rm(x)
  require(magrittr)
  h <- hist(x, breaks = breaks, plot = FALSE)
  cut(x, h$breaks, labels = h$mids) %>% 
    as.character %>% 
    as.numeric
}

cut_mids <- function(x, breaks, labels) {
  require(magrittr)
  cut(x, breaks, labels) %>% 
    as.character %>% 
    as.numeric
}

recambio <- function(f, labels) {
  # Usada por cut3
  # Se puede sustituir por recode_factor seguramente
  require(magrittr)
  levels(f) <- labels
  f %>% 
    as.character %>% 
    as.numeric
}

cut3 <- function(data, v, breaks = 30, log = FALSE) {
  require(magrittr)
  require(dplyr)
  require(rlang)
  # x <- log10(dt$PT)
  # x <- inf.rm(x)
  v <- enquo(v)

  if (log) {
    x <- mutate(data, x = log10(!!v))
  } else {
    x <- mutate(data, x = !!v)
  }
  
  h <- hist(x$x, breaks = breaks, plot = FALSE)
  x %>%
    mutate(
      rango = cut(x, h$breaks),
      mids  = recambio(rango, h$mids)) %>%
      # mids = recode_factor(rango)
    select(-x)
}



#' Marcas para ejes en escala Log
#'
#' (Posiblemente obsoleta) `ticksLog` toma un vector numerico y devuelve una
#' tabla con Valores, un vector de las potencias de 10 en donde van las marcas
#' y el texto a imprimir en dichas marcas (los numeros en escala lineal).
#' 
#' @param x `numeric` atomic vector
#'
#' @return Devuelve una tabla con dos elementos: Valores y Texto.
#' \describe{
#'   \item{Valores}{Vector numerico con los valores en que deben insertarse
#'   "ticks" dentro del eje.}
#'   \item{Texto}{Vector de texto con las cadenas a imprimir en las marcas
#'   del grafico}
#' }
#' @export
#'
#' @examples
#' y <-  rlnorm(200, 2, 3)
#' ti <- ticksLog(y)
#' plot_ly(data.frame(x = y), x = ~ log10(x), type = "histogram") %>%
#'   layout(xaxis = list(tickvals = ti$Valores, ticktext = ti$Texto))
ticksLog <- function(x) {
  require(magrittr)
  if (any(x <= 0)) {
    d <- length(x) - length(x <- x[x > 0])
    txt <- ifelse(d > 1, 
           paste0("eliminaron ", d, " valores"),
           paste0("eliminó ", d, " valor"))
    warning("Se ", txt, " <= 0 para determinar los ticks del eje")
  }
  x <- log10(x)
  rango <- range(x, na.rm = TRUE)
  tvals <- floor(rango[1]):ceiling(rango[2])
  ttext <- number(z <- 10 ** (tvals), accuracy = min(z))
  return(tibble(Valores = tvals, Texto = ttext))
}

rango2lineal <- function(x, log = FALSE) {
  # Para rangos/intervalos que estaban en 
  # log10: convierte los numeros a escala
  # lineal, usando 10 ** x
  require(magrittr)
  require(stringi)
  if (!log) return(x)
  nc <- 
    x %>%
    as.character %>% 
    nchar
  ini <- substr(x,  1,  1)
  fin <- substr(x, nc, nc)
  xlvl <- x %>%
    levels %>%
    gsub("\\(|\\)|\\[|\\]", "", .) %>% 
    stri_split_fixed(",") %>% 
    lapply(function(x) round(10 ** as.numeric(x), 1)) %>% 
    stri_join_list(", ") %>% 
    paste0(ini, ., fin)
  levels(x) <- xlvl
  return(x)
}


extRango <- function(x, min = TRUE) {
  require(magrittr)
  require(stringi)
  # Para rangos/intervalos que estaban en 
  # log10: convierte los numeros a escala
  # lineal, usando 10 ** x
  x %>%
    as.character %>%
    gsub("\\(|\\)|\\[|\\]", "", .) %>% 
    stri_split_fixed(",") %>% 
    lapply(function(x) round(as.numeric(x), 1)[ifelse(min, 1, 2)]) %>% 
    unlist
}

#' Prepara tabla para graficar histogramas con plto_ly
#'
#' @param .data 
#' @param var 
#' @param fill_var 
#' @param log 
#' @param breaks 
#'
#' @return Un data.frame con las siguientes columnas:
#' \describe{
#'   \item{Columna 1, opcional}{`character` Se trata de los valores de `fill_var`.}
#'   \item{mids}{`double` Puntos medios los intervalos a ser utilizados para hacer
#'   histogramas.}
#'   \item{rangos}{`factor` Intervalos para realizar histograma. `mids` contiene los
#'   puntos medios de estos intervalos.}
#'   \item{n}{`integer` Conteos de la cantidad de entradas en `.data` que caen en
#'   cada uno de los intervalos.}
#' }
#' @export
#'
#' @examples
#' prepData(diamonds, price)
#' prepData(diamonds, price, log = TRUE)
#' prepData(diamonds, price, breaks = 10)
#' prepData(diamonds, price, breaks = "FD)
#' prepData(diamonds, price, cut, color, log=TRUE)
prepData <- function(.data,
                     var,
                     ...,
                     log = FALSE,
                     breaks = 30) {
  require(magrittr)
  require(rlang)
  require(dplyr)
  
  var   <- enquo(var)
  gvars <- enquos(...)
  
  out <- .data %>%
    cut3(!!var, breaks = breaks, log = log) %>% 
    count(!!!gvars, mids, rango) %>% 
    mutate(rango = rango2lineal(rango, log))
  
  vnames <- gvars %>% sapply(quo_name)
  names(out)[1:(length(out) - 3)] <- vnames
  return(out)
}



#' Histograma plotly apilado
#'
#' @param .data `data.frame` con el formato generado por la función prepData
#' @param var Nombre de la variable a graficar.
#' @param fill_var Nombre de variable a usar para rellenar con diferentes colores las barras del histograma
#' @param log `logical`. Eje x en escala logaritmica, si es TRUE.
#' @param ylab `character`. Etiqueta para el eje y.
#' @param yrange `numeric`. Vector de longitud 2, que fija el rango de valores a abarcar en el eje y. 
#' @param slider `logical`. Si es TRUE, se agrega un `rangeslider` debajo del histograma.
#' @param ... Argumentos extra para pasar a la función `plot_ly`.
#'
#' @return
#' @export
#'
#' @examples
#' iris2 %>% 
#'   prepa(var = Sepal.Length, fill_var = Prueba) %>% 
#'   singleStackedHist2(var = Sepal.Length, fill_var = Prueba)
#' 
#' iris2 %>% 
#'     prepa(var = Sepal.Length, fill_var = NULL) %>% 
#'     singleStackedHist2(var = Sepal.Length, fill_var = NULL)
singleStackedHist <- function(.data,
                              var,
                              fill_var = NULL,
                              log = FALSE,
                              ylab = "Conteo",
                              yrange = NULL,
                              slider = TRUE,
                              ...) {
  require(rlang)
  require(magrittr)
  require(dplyr)
  require(plotly)
  require(stringi)
  
  var <- enquo(var)
  fill_var <- enquo(fill_var)
  
  ejex <- list(title = trae_etiqueta(!!var))
  
  if (log) {
    b <- log10_trans()
    
    x <- .data$rango %>% 
      levels %>% 
      as.character %>% 
      gsub("\\(|\\)|\\[|\\]", "", .) %>% 
      stri_split_fixed(",") %>%
      sapply(as.numeric) %>% 
      structure(dim = NULL) %>% 
      distinct
    
    cortes <- b$breaks(x[x > 0])
    cortesTexto <- number(cortes, accuracy = min(cortes))
    
    ejex$tickvals <- cortes %>% log10
    ejex$ticktext <- cortesTexto
    # ejex$range    <- range(x %>% log10)
  }  
  ejey <- list(title = ylab,
               fixedrange = FALSE)
  if (!is.null(yrange))
    ejey$range <- yrange
  
  if (quo_name(fill_var) != "Ninguna") {
    color <- as.formula(paste0("~", quo_name(fill_var)))
    nfill <-
      .data %>%
      select(!!fill_var) %>%
      distinct %>%
      nrow
  } else {
    color <- "#F8766D"
    nfill <- 0
  }
  colors <- gg_color_hue(nfill)
  
  pt <-
    .data %>%
    plot_ly(
      x = ~ mids,
      y = ~ n,
      color = color,
      # color = select(., !!fill_var)[[1]],
      colors = colors,
      text = ~ paste0("Rango: ", rango, "\n",
                      "Conteo: ", n),
      hoverinfo = "text+name",
      ...
    ) %>%
    add_bars() %>%
    layout(
      hovermode = "compare",
      barmode = "stack",
      bargap = 0,
      xaxis = ejex,
      yaxis = ejey
    )
  if (slider) {
    pt <- pt %>%
    rangeslider(yaxis = list(rangemode = "auto"),
                bgcolor = "#F2F2F2")
  }
  return(pt)
}


trae_etiqueta <- function(x, ts) {
  # Devuelve nombre completo del parametro x usando la tabla_sustitucion como referencia.
  
  # x : nombre de un parametro presente en la tabla_sustitucion.
  x <- enquo(x)
  require(magrittr)
  # if (!exists("tabla_sustitucion")) load("tabla_sustitucion.RData")
  # ts <- tabla_sustitucion
  out <- quo_name(x)
  if (out %in% ts$Sustituto) {
    out <- with(ts, Original[out == Sustituto])
  }
  return(out)
}

tsummary <- function(x, param) {
  # Summary pero con salida tipo tibble.
  # x : tabla
  # param : nombre de un parametro (columna) de x. Sin comillas
  require(rlang)
  require(dplyr)
  require(magrittr)
  # grupo <- quos(...)
  param <- enquo(param)
  x %>%
    # select(!!!grupo,!!param) %>%
    # filter(!is.na(!!param)) %>%
    # group_by(!!!grupo) %>%
    summarise(
      n = n(),
      Min = min(!!param, na.rm = TRUE),
      `1er Qu` = quantile(!!param, na.rm = TRUE, probs = .25),
      Media = mean(!!param, na.rm = TRUE) %>% round(ifelse(. < 1, 5, 3)),
      Mediana = median(!!param, na.rm = TRUE),
      `3er Qu` = quantile(!!param, na.rm = TRUE, probs = .75),
      Max = max(!!param, na.rm = TRUE)
      # NAs = sum(is.na(!!param))
    ) %>%
    filter(!is.na(Mediana))
}
# tsummary(tnum, Conduc)
# tnum %>% group_by(Programa) %>% tsummary(Conduc)
