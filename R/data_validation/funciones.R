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
    select(-x)
}



#' Marcas para ejes en escala Log
#'
#'`ticksLog` toma un vector numérico y devuelve un
#' @param x `numeric` atomic vector
#'
#' @return Devuelve una lista con dos elementos: Valores y Texto.
#' \describe{
#'   \item{Valores}{Vector numérico con los valores en que deben insertarse
#'   "ticks" dentro del eje.}
#'   \item{Texto}
#' }
#' @export
#'
#' @examples
ticksLog <- function(x) {
  require(magrittr)
  l1 <- length(x)
  x <- x[x > 0]
  l2 <- length(x)
  if (l1 != l2) {
    d <- l1 - l2
    txt <- ifelse(d > 1, 
           paste0("eliminaron ", d, " valores"),
           paste0("eliminó ", d, " valor"))
    warning("Se ", txt, " <= 0 para determinar los ticks del eje")
  }
  x <- log10(x)
  rango <- range(x, na.rm = TRUE)
  tvals <- ceiling(rango[1]):floor(rango[2])
  ttext <- 
    lindo_num(10 ** (tvals))
  return(list(Valores = tvals, Texto = ttext))
}

rango2lineal <- function(x, log = FALSE) {
  # Para rangos/intervalos que estaban en 
  # log10: convierte los números a escala
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
  x %>%
    as.character %>%
    gsub("\\(|\\)|\\[|\\]", "", .) %>% 
    stri_split_fixed(",") %>% 
    lapply(function(x) round(10 ** as.numeric(x), 1)) %>% 
    stri_join_list(", ") %>% 
    paste0(ini, ., fin)
}


extRango <- function(x, min = TRUE) {
  require(magrittr)
  require(stringi)
  # Para rangos/intervalos que estaban en 
  # log10: convierte los números a escala
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
#' @return
#' @export
#'
#' @examples
#' require(magrittr)
#' require(rlang)
#' require(dplyr)
#' iris2 <- iris %>% 
#' mutate(Prueba = sample(LETTERS[1:2], replace = TRUE, size = 150)) %>% 
#'   as.tibble 
#' prepa(iris2, var = Sepal.Length)
#' prepa(iris2, var = Sepal.Length, log = TRUE)
#' prepa(iris2, var = Sepal.Length, breaks = 10)
#' prepa(iris2, var = Sepal.Length, breaks = "FD")
#' prepa(iris2, var = Sepal.Length, fill_var = Prueba)
prepData <- function(.data,
                     var,
                     fill_var = NULL,
                     log = FALSE,
                     breaks = 30) {
  require(magrittr)
  require(rlang)
  require(dplyr)
  
  var <- enquo(var)
  fill_var <- enquo(fill_var)
  
  da <- .data %>%
    cut3(!!var, breaks = breaks, log = log)
  
  if (quo_name(fill_var) != "NULL") {
    da <- count(da, !!fill_var, mids, rango)
  } else {
    da <- count(da, mids, rango)
  }
  da %>%
    mutate(rango = rango2lineal(rango, FALSE))
}

#' Histograma plotly stacked
#'
#' @param .data 
#' @param var 
#' @param fill_var 
#' @param log 
#' @param yrange 
#' @param slider 
#' @param ... 
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
      unique
    
    # x <- select(.data,!!var)[[1]]
    cortes <- b$breaks(x[x > 0])
    cortesTexto <- number(cortes, accuracy = min(cortes))
    
    # ti <- ticksLog(x)
    
    ejex$tickvals <- cortes %>% log10
    ejex$ticktext <- cortesTexto
    
    ejex$tickvals <- ti$Valores
    ejex$ticktext <- ti$Texto
    # lindo_num(10 ** (ejex$tickvals))
  }
  
  ejey <- list(title = "Conteo",
               fixedrange = FALSE)
  if (!is.null(yrange))
    ejey$range <- yrange
  
  if (quo_name(fill_var) != "NULL") {
    color <- as.formula(paste0("~", quo_name(fill_var)))
    nfill <-
      .data %>%
      select(!!fill_var) %>%
      unique %>%
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


trae_etiqueta <- function(x) {
  # Devuelve nombre completo del parámetro x usando la tabla_sustitucion como referencia.
  
  # x : nombre de un parámetro presente en la tabla_sustitucion.
  x <- enquo(x)
  require(magrittr)
  if (!exists("tabla_sustitucion")) load("tabla_sustitucion.RData")
  ts <- tabla_sustitucion
  out <- quo_name(x)
  if (out %in% ts$Sustituto) {
    out <- with(ts, Original[out == Sustituto])
  }
  return(out)
}

tsummary <- function(x, param) {
  # Summary pero con salida tipo tibble.
  
  # x : tabla
  # param : nombre de un parámetro (columna) de x. Sin comillas
  
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
