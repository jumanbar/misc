# Descripcion ---------------------------------------------

# VISUALIZACIoN DE DATOS HISToRICOS + NUEVOS DATOS / HISTOGRAMA
# Autor: Juan Manuel Barreneche Sarasola (jumanbar@gmail.com)

# Aplicacion Shiny de ejemplo para ilustrar el modelo de interfaz de exploracion
# / validacion de datos a incorporar en las aplicaciones del SIA.

# El objetivo no es simular la interfaz completa, si no especificamente la
# visualizacion de datos a traves de histogramas. El objetivo es incorporar al
# menos dos formas de visualizacion mas:
# 1. series de tiempo historicas de los parametros y
# 2. correlacion entre parametros, aplicable a ciertos casos (Salinidad Vs.
#    Conductividad, NT Vs. NO2 o NO3 o NO2+NO3, PT Vs PO4 ...)
# Ejemplo de comparacion de PT vs PO4 (requiere tidyverse y Plotly):
# p <-
#   tnum %>%
#   ggplot() +
#   aes(PT, PO4, col = Programa) +
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_abline(slope = 1, intercept = 0)
# ggplotly(p)

# Version: 2019-01-31
# Pendientes / funcionalidades a agregar:
# - Incorporar facetas en columnas y/o facet_wrap, usando ggplotly. No
#   estrictamente necesario
# - Variante: exploracion de datos historicos, sin tabla de datos nuevos. Esta
#   modalidad tambien tendria una tabla con los datos historicos (como tnum)
# - De todas formas, la tabla que acompania al grafico en la demo (llamada
#   "Nuevos datos") no seria la definitiva, o al menos no necesariamente, si
#   no que es incluida para ilustrar un par de funcionalidades que se proponen
#   (que son menos que las deseadas!)
# - Boton para utomaticamente filtrar datos historicos que coincidan con "Nuevos
#   datos" (en cuanto a las columnas de "metadatos", es decir: Programa,
#   Departamento, Estacion, Institucion y Usuario).
# - Esta demo no tiene columnas de LC y LD (Limites de Cuantificacion y
#   Deteccion).
# - Otra posible incorporacion es la de mostrar una tabla con la seleccion de
#   datos hecha en el grafico... intente hacer algo parecido pero es complicado
#   debido a que el grafico incluye series de datos de diferente origen, pero
#   seguramente hay alguna forma de hacerlo.
# - Reever: cuando se elige una variable nueva (caja de grafico), se reinician
#   todos los filtros, lo cual no es el ideal creo.
# - Cambiar visualización por defecto: modo resaltado de estaciones presentes
#   en en la muestra de nuevos datos, sobre resto de los datos históricos. Ej:
#   si hay datos nuevos de 4 estaciones (A, B, C y D), se muestra histograma
#   con 5 colores: A, B, C, D y resto de muestras históricas (+ círculos en
#   todos los datos nuevos). Tendría que ser un tipo de visualización especial,
#   con botón propio específicamente para eso capaz.

# Preparacion -------------------------------------------------------------
# Paquetes:
library(tidyverse)
library(scales)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(crosstalk)
# Carpeta de trabajo y funciones propias:
# setwd("~/Producto 4 - ingreso datos/interfaz_validacion_mockup/R/aexp_histograma")
# setwd("C:/Users/el usuario/Desktop/jmb/DINAMA/interfaz_validacion/demo_hist/aexp_histograma")
source("funciones.R", encoding = "UTF-8")

# Datos de ejemplo para la demo:
#______________________________________
load("tablas_partes.RData")
# tnum
# tb
# tb_LD
# tb_LC
# tabla_sustitucion
#______________________________________
load("newdata.RData")
newdata <- select(newdata, -NombreEstacion)
# newdata
#______________________________________


# Variable seleccionada por defecto:
Variable_Inicial = "PT"

# USAR DISTINCT EN VEZ DE UNIQUE!!:
# > dim(tnum)
# [1] 12241   150
# > tiempos <- data.frame(Unique = numeric(100),
#   +                     Distinct = numeric(100))
# > for (i in 1:100) {
#   +   tiempos$Unique[i] <- system.time(unique(tnum))[3]
#   +   tiempos$Distinct[i] <- system.time(dplyr::distinct(tnum))[3]
#   + }
# > summary(tiempos)
# Unique          Distinct
# Min.   :0.7600   Min.   :0.0300
# 1st Qu.:0.7800   1st Qu.:0.0300
# Median :0.7800   Median :0.0300
# Mean   :0.7904   Mean   :0.0316
# 3rd Qu.:0.7900   3rd Qu.:0.0300
# Max.   :0.9600   Max.   :0.0500

# dt: Tabla con datos historicos de todas las variables y todos los programas,
# tomada de los datos del SIA (no incluye los LD y LC):
dt <- distinct(tnum) %>%
  # Agregar anio y mes como variables categoricas:
  mutate(Anio = year(FechaMuestreo) %>% as.character,
         Mes = month(FechaMuestreo, label = TRUE, abbr = FALSE)) %>%
  # Seleccionar columnas de interes:
  select(Programa, Periodo:Estacion, Departamento:FechaMuestreo,
         Anio:Mes, Replica:Usuario, Temp:`245TP`) %>%
  # Eliminar columnas sin datos:
  select_if(function(x) !all(is.na(x)))

# Rango de valores para el grafico inicial:
var_rango_ini <- range(dt[[Variable_Inicial]], na.rm = TRUE)

# Tabla con los "metadatos":
mdt <- dt %>%
  select(Programa, Departamento, Estacion, Institucion, Usuario) %>%
  arrange(Programa, Departamento, Estacion, Institucion, Usuario) %>%
  distinct

# NEWDATA -----------------------------------------------------------------
# Conjunto de datos de la campania Diciembre 2016, del programa Santa Lucia,
# bajados desde la aplicacion del SIA. No incluye LD y LC.
newdata <- newdata %>%
  mutate(Anio = year(FechaMuestreo) %>% as.character,
         Mes = month(FechaMuestreo, label = TRUE, abbr = FALSE)) %>%
  mutate_if(is.character, factor) %>%
  select(ID,
         Programa,
         Periodo:Estacion,
         Departamento:FechaMuestreo,
         Anio:Mes,
         Replica:Usuario,
         Temp:AMPA)

# Modifico uno de los "Nuevos datos" para que resulte llamativo, estando por muy
# encima de los rangos historicos observados para PT:
newdata$PT[24] <- 50534

# Opciones para menus desplegables ----------------------------------------
nms  <- names(dt)[1:10]   # Nombres de columnas con "metadatos"
vars <- names(dt)[-1:-10] # Nombres de columnas con variables

# Metodos de corte para histogramas:
br_methods <- c("Sturges", "Scott", "FD")

# Programas de monitoreo disponibles:
pr_choices <- dt %>% pull(Programa) %>% unique %>% sort
# Seleccion inicial (determinada por la variable inicial):
pr_ini <- dt %>%
  # Solo programas que tienen datos de dicha variable:
  filter(!is.na(!!sym(Variable_Inicial))) %>%
  pull(Programa) %>%
  unique %>%
  sort

# Opciones de Departamento. Seleccion inicial es indirectamente determinada por
# la variable inicial, a traves de la columna Programa:
dpt_ini <-
  mdt %>%
  filter(Programa %in% pr_ini) %>%
  pull(Departamento) %>%
  unique %>%
  sort

# Tabla proest: para poder incluir texto chico en el menu estaciones, a fin de
# poder ayudar al usuario a encontrar las estaciones que busca. En el menu
# desplegable se veran las estaciones asi:

# Estacion Programa (DEPARTAMENTO)
# Ejemplos:
# CA01 Santa Lucia (CANELONES)
# CA02 Santa Lucia (CANELONES)
# ...
# SJ01 Santa Lucia (FLORES)
# etc...
proest <- mdt %>%
  select(Programa, Departamento, Estacion) %>%
  arrange(Programa, Departamento, Estacion) %>%
  distinct

# Opciones de Estacion. Seleccion inicial es indirectamente
# determinada por la variable inicial, a traves de la columna
# Programa:
es_ini <-
  proest %>%
  filter(Programa %in% pr_ini) %>%
  pull(Estacion)

# Opciones de Institucion. Seleccion inicial es indirectamente
# determinada por la variable inicial, a traves de la columna
# Programa:
ins_ini <- mdt %>%
  filter(Programa %in% pr_ini) %>%
  pull(Institucion) %>%
  unique %>%
  sort

# Opciones de Usuario. Seleccion inicial es indirectamente
# determinada por la variable inicial, a traves de la columna
# Programa:
usr_ini <- mdt %>%
  filter(Programa %in% pr_ini) %>%
  pull(Usuario) %>%
  unique %>%
  sort

# UI ----------------------------------------------------------------------
# Armado de la interfaz grafica; ecabezado:
header <- dashboardHeader(title = "Histograma")

# Cuerpo:
body <- dashboardBody(
  # 1 fila con 2 columnas:
  fluidRow(
    column(width = 9,

           # CAJA SUMMARY TABLE -------------------------------------------
           # Muestra un resumen numerico de los datos historicos de la variable
           # a analizar. Segun el checkbox "Agrupada por leyenda/color", los
           # datos seran agrupados o no segun las categorias de la variable
           # seleccionada para la leyenda:
           box(width = NULL, solidHeader = TRUE, status = "info",
               title = div(icon("table"), "Tabla Resumen (historico)"),
               collapsible = TRUE, collapsed = TRUE,
               checkboxInput("st_group", "Agrupada por leyenda/color", 
                             value = TRUE),
               DT::dataTableOutput("SummaryTable")
           ),

           # CAJA HISTOGRAMA ----------------------------------------------
           box(width = NULL, solidHeader = TRUE, status = "success",
               title = div(icon("chart-bar"), "Histograma interactivo"),
               plotlyOutput('Histogramas', height = NULL)
           )
           # CAJA NEWDATA -------------------------------------------------
           # Muestra la tabla con nuevos datos ("recien ingresados"):
         , box(width = NULL, solidHeader = TRUE, status = "info",
               title = div(icon("table"), "Nuevos datos"),
               collapsible = TRUE, collapsed = FALSE,
               # Menu para seleccionar las columnas mostradas:
               pickerInput(inputId = "newdata_col", width = "50%",
                             label = "Variables", inline = TRUE,
                             choices = names(newdata)[-1],
                             selected = names(newdata)[-1],
                             options = list(`actions-box` = TRUE,
                                            `live-search` = TRUE,
                                            size = 10),
                             multiple = TRUE),
               DT::dataTableOutput("NewData")
           )
           ),
    column(width = 3,

           # CAJA DISEniO --------------------------------------------------
           # Disenio, porque implica tomar decisiones respecto a que variable se
           # va a graficar, asi como los colores o dispocisiones para distinguir
           # entre grupos (como Programa, Estacion, etc...).
           box(width = NULL, status = "warning",
               title = div(icon("sliders-h"), "Disenio del Grafico"),
               collapsible = TRUE, collapsed = FALSE,
               fluidRow(
                 # Elegir variable a analizar:
                 column(9, pickerInput("variable", "Variable",
                                       choices = vars,
                                       options = list(title = "Elija columna",
                                                      `live-search` = TRUE,
                                                      size = 10),
                                       selected = Variable_Inicial)),
                 # Escala logaritmica o lineal?:
                 column(3, checkboxInput("log",
                                         "Escala Log",
                                         value = TRUE)
                        )
               ),
               tags$small(
                 class = "text-muted",
                 "La eleccion de variable afecta las opciones de filtro"
                 ),
               fluidRow(
                 # Cortes para histograma, metodos predefinidos o ingreso manual
                 # del N° de cortes:
                 column(6, pickerInput(inputId = "br_choice",
                                       label = "Cortes (Nº/Metodo)",
                                       choices = c("Numero", "Metodo"),
                                       selected = "Numero"
                        )),
                 # En funcion de la opcion anterior, se mostrara un menu con
                 # opciones (opcion Metodo) o ingreso de un valor entero:
                 column(6, conditionalPanel(
                             condition = "input.br_choice == 'Metodo'",
                             pickerInput(inputId = "breaks_mtd",
                                         label = "Metodo",
                                         choices = br_methods,
                                         selected = "Scott")
                             ),
                           conditionalPanel(
                             condition = "input.br_choice == 'Numero'",
                             numericInput(inputId = "breaks_num",
                                          label = "#Cortes",
                                          30, min=3, max=2000)
                             )
                   )
                 ),

               ### Leyenda
               # Menu para elegir que metadato se va a codificar por colores en
               # el grafico:
               pickerInput("fill", "Leyenda",
                           options = list(`live-search` = TRUE,
                                          size = 10),
                           choices = c("Ninguna", nms),
                           selected = "Programa"),
               # Recordatorio: mandar issue a
               # https://github.com/dreamRs/shinyWidgets/issues para pedir que
               # incorporen una forma de volver a elegir "nada" luego de que ya
               # elegiste algo. Esto pasa cuando esta activada la opcion
               # "title".

               ### Filas (~facet_grid, vertical)
               # Si se activa, se divide el histograma en varias "filas", cada
               # una conteniendo valores de la variable analizada
               # correspondientes a una de las categorias presentes en la
               # variable/metadato seleccionada:
               fluidRow(
                 column(9, pickerInput("facet_row", "Facetas en filas",
                                       options = list(`live-search` = TRUE,
                                                      size = 10),
                                       choices = c("Ninguna", nms))
                 ),
                 # Opcion de dejar fijo el eje y (vertical). Esto es mejor para
                 # comparar cantidades en valores similares:
                 column(3, conditionalPanel(
                             condition = "input.facet_row != 'Ninguna'",
                             checkboxInput(
                               "scale_y",
                               "Eje y libre",
                               value = TRUE
                               )
                             )
                   )
                 ),
               ### Columnas (~facet_grid, horizontal)
               # No se habilita porque no pude hacerlo con plotly puro; la idea
               # es por lo menos habilitarlo utilizando ggplot + ggplotly, pero
               # por cuestiones de tiempo, se deja para mas adelante.
               # conditionalPanel(condition = "input.facet_row != 'Ninguna'",
               #                  pickerInput("facet_col", "Facetas en columnas",
               #                              options = list(# title = "Elija una variable",
               #                                             `live-search` = TRUE,
               #                                             size = 10),
               #                              choices = c("Ninguna", nms))
               #                  ),
               ### Altura
               # Regular la altura del grafico es importante para poder
               # visualizar bien los histogramas, especialmente cuando se usa la
               # opcion de facetas en filas:
               sliderInput("plotHeight",
                           "Altura del grafico (en pixeles)",
                           min = 300, max = 4000, value = 450)
               ),


           # CAJA FILTROS -------------------------------------------------
           # En esta caja se incluyen varios controladores para filtar el
           # conjunto de datos de los histogramas:
           box(width = NULL, status = "warning",
               title = div(icon("sliders-h"), "Filtros"),
               collapsible = TRUE, collapsed = FALSE,
               # Rango de fechas de los datos mostrados en el grafico. Por
               # defecto toma comienzo y fin del total de los datos existentes
               # (todo el historico):
               dateRangeInput("daterange", "Rango de fechas:",
                              separator = " hasta ",
                              start = min(dt$FechaMuestreo) %>%
                                as.Date %>% as.character,
                              end   = max(dt$FechaMuestreo) %>%
                                as.Date %>% as.character
               ),
               # Se pueden filtrar los datos a partir de cualquiera de las
               # variables presentes en el conjunto de datos:
               pickerInput(inputId = "pickVariable_4range",
                           label = "Elija variable de filtro",
                           choices = vars, selected = Variable_Inicial,
                           options = list(`live-search` = TRUE,
                                          size = 10)
                           ),
               fluidRow(
               # Ingresando valores numericos se puede controlar el rango de los
               # valores mostrados en el grafico:
                 column(6, numericInput(
                   inputId = "filtro_var_min",
                   label = "Minimo:", value = var_rango_ini[1])
                   ),
                 column(6, numericInput(
                   inputId = "filtro_var_max",
                   label = "Maximo:", value = var_rango_ini[2])
                 )),
               # Filtro de los programas de monitoreo incluidos:
               pickerInput(inputId = "listaProgramas",
                           label = "Programas:",
                           choices = pr_choices,
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE),
                           selected = pr_ini,
                           multiple = TRUE),
               # Filtro de los departamentos en los que se muestrearon los
               # datos:
               pickerInput(inputId = "listaDeptos",
                           label = "Departamentos:",
                           choices = dpt_ini,
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE),
                           multiple = TRUE),
               # Filtro de las estaciones de muestreo en que se tomaron los
               # datos. En cada estacion se muestra el programa y el
               # departamento al que pertenecen:
               pickerInput(inputId = "listaEstaciones",
                           label = "Estaciones:",
                           choices = es_ini,
                           choicesOpt =
                             list(subtext = paste0(
                               proest$Programa,
                               " (", proest$Departamento, ")"
                             )),
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE,
                                          size = 20),
                           multiple = TRUE),
               # Filtro de las instituciones que tomaron los datos:
               pickerInput(inputId = "listaInstituciones",
                           label = "Instituciones:",
                           choices = ins_ini,
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE
                           ),
                           multiple = TRUE),
               # Filtro de los usuarios que cargaron los datos:
               pickerInput(inputId = "listaUsuarios",
                           label = "Usuarios:",
                           choices = usr_ini,
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE
                           ),
                           multiple = TRUE)
               )
           )
    )
  )

# Armado de la interfaz grafica:
ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  skin = "purple"
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  # require(ggplot2)
  # require(dplyr)
  # require(magrittr)
  # require(plotly)
  # require(scales)
  # require(tibble)

  # Tabla reactiva --------------------------------------------------------
  # La tabla con el historico de todos los datos ambietales. Aqui es donde
  # se efectuan los cambios de las opciones en la caja de filtros.
  datahist <- reactive({
	# Solo se toman datos con valores (ie: se excluyen los NA):
    nas <- !is.na(dt[[input$variable]])
    # Filtro de rango de valores para la variable analizada:
    rango <- dt[[input$pickVariable_4range]] >= input$filtro_var_min &
      dt[[input$pickVariable_4range]] <= input$filtro_var_max
    # Filtros por fechas:
    fecha_min <- dt$FechaMuestreo >= input$daterange[1]
    fecha_max <- dt$FechaMuestreo <= input$daterange[2]
    # Filtro por programas:
    progs <- dt$Programa %in% input$listaProgramas
    # Filtro por estaciones de muestreo:
    estac <- dt$Estacion %in% input$listaEstaciones
    # Filtro por instituciones:
    insti <- dt$Institucion %in% input$listaInstituciones
    # Filtro por usuarios:
    usuar <- dt$Usuario %in% input$listaUsuarios
    dt[nas & rango & fecha_min & fecha_max & progs & estac & insti & usuar,]
  })

  # observe({}) # Observar el programa segun lo que hay en datahist?

  # NewData == SharedData -------------------------------------------------
  # Tabla con Nuevos datos, como objeto SharedData$new (paquete crosstalk):
  nd <- SharedData$new(newdata, ~ID)

  # Actualiza la variable a filtrar ---------------------------------------
  observe({
    updatePickerInput(
      session,
      inputId = "pickVariable_4range",
      selected = input$variable
    )
  })


  # Actualiza rango de variable --------------------------------------------
  observe({
    updateNumericInput(
      session,
      inputId = "filtro_var_min", # Por alguna razon se actualiza a NA
      value = min(dt[[input$pickVariable_4range]], na.rm = TRUE)
      )
  })
  observe({
    updateNumericInput(
      session,
      inputId = "filtro_var_max", # Por alguna razon se actualiza a NA
      value = max(dt[[input$pickVariable_4range]], na.rm = TRUE)
    )
  })


  # Actualiza Programas ---------------------------------------------------
  observe({
    p <-
      filter(dt, !is.na(!!sym(input$variable))) %>%
      pull(Programa) %>%
      unique %>%
      sort
    if (is.null(p))
      p <- character(0)
    updatePickerInput(session = session,
                      inputId = "listaProgramas",
                      choices = p,
                      selected = p)
  })

  # Actualiza Departamentos -----------------------------------------------
  observe({
    d <-
      filter(mdt, Programa %in% input$listaProgramas) %>%
      pull(Departamento) %>%
      unique %>%
      sort
    if (is.null(d))
      d <- character(0)
    updatePickerInput(session = session,
                      inputId = "listaDeptos",
                      choices = d,
                      selected = d)
  })

  # Actualiza Estaciones --------------------------------------------------
  observe({
    filtro <-
      filter(mdt, Programa %in% input$listaProgramas &
               Departamento %in% input$listaDeptos) %>%
      select(Programa, Departamento, Estacion) %>%
      distinct
    e <- filtro$Estacion
    if (is.null(e))
      e <- character(0)
    updatePickerInput(session = session,
                      inputId = "listaEstaciones",
                      choices = e,
                      choicesOpt =
                        list(subtext = paste0(
                          filtro$Programa,
                          " (", filtro$Departamento, ")"
                          )),
                      selected = e)
  })

  # Actualiza Instituciones -----------------------------------------------
  observe({
    i <-
      filter(mdt, Programa %in% input$listaProgramas) %>%
      pull(Institucion) %>%
      unique %>%
      sort
    if (is.null(i))
      i <- character(0)
    updatePickerInput(session = session,
                      inputId = "listaInstituciones",
                      choices = i,
                      selected = i)
    })

  # Actualiza Usuarios ----------------------------------------------------
  observe({
    u <-
      filter(mdt, Programa %in% input$listaProgramas &
               Institucion %in% input$listaInstituciones) %>%
      pull(Usuario) %>%
      unique %>%
      sort
    if (is.null(u))
      u <- character(0)
    updatePickerInput(session = session,
                      inputId = "listaUsuarios",
                      choices = u,
                      selected = u)
  })


  # renderPlotly: histograma/s ---------------------------------------------
  # Codigo que genera el grafico de el/los histogramas, utilizando plotly (sin
  # ggplot2)
  output$Histogramas <- renderPlotly({
    # AGREGAR: uso de la opcion "facetas en columnas", para lo cual habria que
    # implementar ggplot2 + ggplotly.

    # Opcion: se divide el grafico en varias partes?
    SPLIT <- if (input$facet_row %in% nms) TRUE else FALSE

    # Seleccion de filas de nuevos datos:
    nd_sel <- input$NewData_rows_selected

    # Preparar los datos ---------------------------------------------------
    # Cortes del histograma:
    br <- if (input$br_choice == "Numero")
      input$breaks_num else input$breaks_mtd
    group_vars <- c(
      input$fill,
      input$facet_row
      # input$facet_col # Afuera por no poder implementar con plotly
      )
    # No incluir variables cuando la opcion marcada es "Ninguna":
    group_vars <- group_vars[group_vars != "Ninguna"]

    dh <- datahist()                 # Datos historicos
    x <- dh[[input$variable]]        # Valores de la variable
    ndx <- newdata[[input$variable]] # Valores de la variable en nuevos datos

    # Valores historicos + valores nuevos de la variable:
    v <- c(x, ndx)

    # Descartar valores negativos en caso de que opcion Escala log = TRUE:
    if (input$log && (s <- sum(neg <- (x <= 0))) > 0) {
      txt <- ifelse(s > 1,
                    paste0("descartaron ", s, " valores"),
                    paste0("descarto ",    s, " valor"))
      warning("Se ", txt, " <= 0 para la construccion de graficos")
    }

    # Funciones de transformacion (solo 2 opcioens: log o lineal):
    vtrans <- if (input$log) log10_trans() else identity_trans()

    # Rango de valores de los nuevos datos:
    rango_nd <- newdata[[input$variable]] %>%
      vtrans$transform() %>%
      inf.rm %>%
      range(na.rm = TRUE)

    # Datos historicos de la variable transformados:
    xt <- vtrans$transform(x)

    # Dos vectores que seran usados para parametros graficos y para
    # los cortes de los histogramas:
    cortx <- vtrans$breaks(v[v > 0])
    tvals <- cortx %>% vtrans$transform()
    ttext <- scales::number(cortx, accuracy = min(cortx))

    # Funcion hist para determinar cortes y puntos medios:
    h <- hist(xt, breaks = br, plot = FALSE)
    intervalos <- cut(xt, h$breaks)

    ## Constuir mid_int:
    ## - mids = numerico; centro de intervalos contenedores del histograma
    ## - ints = factor; intervalo del hist. en escala log
    ## - br_info = character; intervalo del hist. en escala lineal

    # bri: puntos de corte del hist., en escala lineal y formato "lindo":
    bri <- h$breaks %>%             # cortes del hist.
      vtrans$inverse() %>%          # escala lineal
      scales::number(accuracy = min(cortx)) # formato

    # br_info: strings que muestran los intervalos del hist.:
    br_info <- paste0("(", bri[-length(bri)], " -- ", bri[-1], "]")

    # Tabla mid_int:
    mid_int <- tibble(mids = h$mids,
                      ints = factor(levels(intervalos)),
                      br_info = br_info)

    # if (input$fill == input$facet_row) browser()
    ugv <- unique(group_vars)

    # Creacion de la tabla plot_data, que tiene la ubicacion (mids) y altura (n)
    # de las barras a ser graficadas en el histograma:
    plot_data <- dh %>%
      # Agrega columna:
      add_column(ints = intervalos) %>%
      # Recuento por variables e intervalos:
      count(!!!syms(ugv), ints) %>% ## Error in !syms(ugv) : invalid argument type???
      # Completar combinaciones faltantes:
      complete(!!!syms(ugv), ints, fill = list(n = 0)) %>%
      # LEFT JOIN con mid_int:
      left_join(mid_int) %>%
      # Reordenar columnas:
      select(!!!syms(ugv), ints, br_info, mids, n) %>%
      # Eliminar intervalos NA:
      filter(!is.na(ints)) %>%
      # Convertir columnas character a factor:
      mutate_if(is.character, factor)

    # Preparar parametros graficos ----------------------------------------
    # Rango de valores en eje y:
    if (SPLIT) {
      maxCount <- plot_data %>%
        # Cual es el maximo conteo que vamos a encontrar en cada intervalo x
        # categoria de la variable elegida para las facetas en fila?:
        group_by(!!sym(input$facet_row), ints) %>%
        summarise(N = sum(n, na.rm = TRUE))
      yrange <- c(0, maxCount$N %>% max) # The range for all the y axis
    }

    # Configuracion del eje x:
    xaxis <- list(
      # Etiqueta del eje:
      title = trae_etiqueta(!!sym(input$variable), tabla_sustitucion),
      # Valores en los tics (cortes) del eje x (escala log):
      tickvals = tvals,
      # Texto en los tics (cortes) del eje x (escala lineal):
      ticktext = ttext
    )

    # Configuracion del eje y:
    yaxis <- list(
      fixedrange = FALSE,
      title = "Conteo" # Etiqueta por defecto para el eje y
      )

    # Colores de relleno:
    if (input$fill %in% nms) {
	  # Color determinado por la opcion en el menu "Leyenda":
      color <- as.formula(paste0("~", input$fill))
      # Cantidad de colores a usar para rellenar barras del hist.:
      nfill <- length(fill_strings <- unique(plot_data[[input$fill]]))
    } else {
      nfill <- 0
      color <- NULL
    }
    # Secuencias de valores de tono para los colores:
    hues   <- seq(15, 375, length = nfill + 1)
    # Codigos de colores HTML, imitando el estilo de ggplot2:
    colors <- hcl(h = hues, l = 65, c = 100)[1:nfill]

    # Texto para mouse hover: el mismo se convierte a un objeto de clase
    # "formula", de forma que plotly determine los valores con que rellenar para
    # cada elemento del grafico:
    texto <- c(
      "~paste0('Rango: ', br_info, '\n",
      "Conteo: ', n)"
    )
    # Dicho texto se modifica en caso de que se utilice la opcion de facetas en
    # filas, incluyendo a que categoria pertenece cada parte:
    if (SPLIT) {
      texto <- c(texto[1],
                 input$facet_row, ": ', ", input$facet_row, ", '\n",
                 texto[2])
    }
    texto <- paste0(texto, collapse = "")

    # Particiones de los datos:
    if (SPLIT) {
	  # Particionar plot_data segun categorias de la variable a usar en la
	  # opcion facetas en filas:
      pdata_split <- split(plot_data, plot_data[[input$facet_row]])
      # filas: vector con nombres de las categorias de cada fila:
      filas <- plot_data[[input$facet_row]] %>% levels
      # Lista vacia a rellenar con los plots de plot_ly:
      plots <- vector("list", length(pdata_split))
      # ndata_split <- split(nd$data(), nd$data()[[input$facet_row]])
    } else {
	  # En caso de no hacer particiones, pdata_split es igual a plot_data, solo
	  # que se excluyen los casos en que no hay datos (n = 0):
      pdata_split <- plot_data %>% filter(n > 0)
    }

    # Funcion definida para luego usar en un loop. Se define aqui para
    # simplificar el codigo, aunque bien se podria definir en el archivo
    # auxiliar "funciones.R" (agregando muchos mas arguemntos):
    plotFun <- function(i = 1, showlegend = TRUE, scales = "free_y", ...) {
	  # i: numero de iteracion en el loop
	  # showlegend: determina si se incluye leyenda en el grafico. Solo una vez
	  #             por loop.
	  # scales: determina si el eje y tiene escala fija o libre (ie: ajustada a
	  #         los rangos de cada sub-grafico)
	  # ...: argumentos a pasar a la funcion plot_ly

	  # Efecto del argumento scales:
      if (scales != "free_y") yaxis$range <- yrange

      # En caso de que se trate de varios sub-graficos (loop):
      if (SPLIT) {

        # El conjunto de datos se restringe a la iesima particion de plot_data:
        .data <- pdata_split[[i]]

        # No siempre se agrega etiqueta al eje y, para no saturar:
        yaxis$title <-
          if (i == round(median(1:length(filas)))) "Conteo" else ""

        # Para agregar etiquetas a la derecha de cada sub-grafico (con las
        # categorias de la variable elegida para las facetas en fila), es
        # necesario saber cual es el rango de valores en el eje y:
        dataCounts <- .data %>%
          group_by(ints) %>%
          summarise(N = sum(n))

        # Teniendo el rango de valores en el eje y, se calcula el punto medio.
        # En caso de que el eje y tenga escala libre, se usa el calculo anterior
        yn <- if (scales != "free_y")
          mean(yaxis$range) else max(dataCounts$N) / 2

      } else {
        # Si se trata de un unico grafico, no se usan particiones:
        .data <- pdata_split
      }

      # El comando plot_ly principal:
      p <- .data %>%
        plot_ly(
          x = ~mids, y = ~n, # Ejes
          color = color, # Formula para determinar los rellenos
          colors = colors, # Codigos HTML de colores a utilizar
          legendgroup = color, # Formula para el parametro legendgroup
          showlegend = showlegend, # Mostrar leyenda? (si es loop, 1 sola vez)
          text = as.formula(texto), # Texto para mose-hover
          hoverinfo = ifelse(is.null(color), "text", "text+name"),
          ...) %>%
        add_bars() %>% # Agrega barras del histograma
        layout(
          #hovermode = "compare", # Deshabilitado por resultar confuso
          # Estilo del histograma, el mismo que usa ggplot2 por defecto:
          barmode = "stack", 
          bargap = 0, # Espacio entre barras contiguas
          xaxis = xaxis, # Lista con parametros para el eje x
          yaxis = yaxis  # Lista con parametros para el eje y
        )

      # Agregar puntos con datos de la variable a analizar incluidos en la tabla
      # con nuevos datos:
      p <- p %>%
        add_markers(
          data = nd$data(),
          x = nd$data()[[input$variable]] %>%
            vtrans$transform(), # Datos transormados segun la opcion escala log
          y = 0, # Todos a la altura del eje x
          name = ~paste("Nuevos", Programa), # Nombre para incluir en la leyenda
          marker = list( # Opciones para el formato de los puntos:
            color = 'rgb(255, 255, 0)', # Color del circulo 
            opacity = .5, # Opacidad / Transparencia
            size = 12, # Tamanio del circulo
            line = list( # Linea de borde
              color = 'rgb(0, 0, 153)', # Color de la linea
              width = 3 # Ancho de la linea
            )
          ),
          showlegend = showlegend, # Incluir en la leyenda; mismo criterio
          # No hereda, porque es de un conjunto de datos distinto al historico:
          inherit = FALSE, 
          text = ~paste0("ID: ", ID, "\nPT: ", PT),
          hoverinfo = "text"
        )
      # Puntos rojos en filas de newdata seleccionadas:
      # Cuando el usuario hace click en una fila de la tabla de Nuevos datos,
      # la selecciona y esto se vera reflejado en el grafico, gracias a estos
      # comandos.
      if (length(nd_sel)) {
		# Conjunto de datos en las filas seleccionadas:
        filas_sel <- nd$data()[nd_sel,]

        # Valores de la variable en esas filas:
        x_sel <- filas_sel[[input$variable]] %>%
          vtrans$transform()

        # Comando plotly par aagregar puntos en esos valores:
        p <- p %>%
          add_markers(
            data = filas_sel,
            x = x_sel, #~vtrans$transform(!!sym(input$variable)),
            y = 0,
            name = "Seleccion",#~paste("Nuevos", Programa, "~seleccionados"),
            marker = list(
              color = 'rgb(255, 0, 0)',
              size = 8
            ),
            inherit = FALSE,
            text = paste0("~paste0('ID: ', ID, '\n",
                          input$variable, ": ', ",
                          input$variable, ")") %>%
              as.formula,
            hoverinfo = "text",
            showlegend = showlegend
          )
      }

      # Si se trata de varios sub-graficos (opcion facetas en filas activada),
      # se agregan etiquetas en el margen derecho, con los nombres de cada
      # categoria:
      if (SPLIT) {
        xrange <- range(c(h$breaks[c(1, length(h$breaks))],
                          rango_nd), na.rm=TRUE)

        # Nombres de facetas-fila en margen derecho:
        p <- p %>%
          add_annotations(
            x = xrange[2] + diff(xrange) / 50, #xaxis$range[2],
            y = yn,
            yref = paste0("y", ifelse(i > 1, i, "")),
            text = filas[i],
            showarrow = FALSE,
            textangle = 90,
            font = list(size = 11),
            bgcolor = "rgba(219, 219, 219, 200)",
            borderpad = 5,
            xanchor = "left",
            yanchor = "middle"
          )
      }
      return(p)
    } #### FIN DE LA FUNCIoN plotFun ###

    # Si se trata de un unico histograma, los comandos para hacer el grafico se
    # terminan aca:
    if (!SPLIT) {
      p <- plotFun(height = input$plotHeight) %>%
        # Abajo del grafico se agrega un deslizador de rango de datos:
        rangeslider(yaxis = list(rangemode = "auto"),
                    bgcolor = "#F2F2F2")
      return(p)
    }

    # En caso de multiples graficos, se ejecuta un loop con la funcion plotFun:
    scale_y <- if (input$scale_y) "free_y" else ""
    for (i in 1:length(pdata_split))
      plots[[i]] <- plotFun(i, showlegend = i == 1,
                            scales = scale_y,
                            height = input$plotHeight)

    # Deslizador de rango de datos:
    plots[[i]] <- plots[[i]] %>%
      rangeslider(yaxis = list(rangemode = "auto"),
                  bgcolor = "#F2F2F2",
                  thickness = .05)

    # Nota: si llegamos hasta aca se supone que hay facetas en filas, y tal vez
    # tambien facetas en columnas. En la funcion subplot alcanza con indicar
    # la cantidad de filas:
    sp <- subplot(plots,
                  nrows = length(filas),
                  shareX = TRUE,
                  shareY = TRUE, titleY = TRUE)
    return(sp)

    # Codigo viejo que no elimine porque puede servir para agregar la opcion de
    # facetas en columnas:
    
    # if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)

    # plot_ly(p, height = input$plotHeight, autosize=FALSE)
    # ggplotly(p)

    # gp <- ggplot(dh) +
    #   aes(PT, fill = Programa) +
    #   geom_histogram() +
    #   scale_x_log10() +
    #   facet_grid(Institucion ~ Anio)
    # gp
    # ggplotly(gp) %>% rangeslider(yaxis = list(rangemode = "auto"))

  }) #### FIN DE renderPlotly ###

  # Render Summary Table ---------------------------------------------------
  # Comandos para incluir una tabla resumen de los datos historicos para la
  # variable a analizar. Por defecto se agregan los datos segun la variable
  # seleccionada para la leyenda; esto se puede revertir con un boton checkbox.
  output$SummaryTable <- DT::renderDataTable({
	# Los datos se agrupan o no segun el checkbox "Agrupada por leyenda/color":
    if (input$fill %in% nms && input$st_group) {
      st <- datahist() %>%
        group_by(!!sym(input$fill))
    } else {
      st <- datahist()
    }
    # La funcion casera tsummary hace todo el trabajo:
    st <- st %>%
      tsummary(!!sym(input$variable)) %>%
      mutate_if(is.character, factor)
    return(st)
  },
  # Extensiones al paquete DT (DataTable, que es un paquete javascript):
  extensions = c('ColReorder', 'Buttons'),
  # Filtors de las columnas, abajo:
  filter = "bottom",
  # Lista de opciones, casi todas relativas a las extensiones usadas:
  options = list(
    dom = 'Blfrtip',
    searchHighlight = TRUE,
    colReorder = TRUE,
    buttons = I('colvis'),
    # Caption es un texto que no esta apareciendo, no se bien por que:
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      htmltools::em('Para usar filtro de columnas, escribir: "MIN ... MAX"')
    ),
    rownames = FALSE # No agrega nombres de las filas
  ))


  # Render Nuevos Datos ----------------------------------------------------
  # Agrega la tabla Nuevos datos a la interfaz, incorporando varias
  # funcionalidades, pero no todas las que se buscan en el proyecto:
  output$NewData <- DT::renderDataTable({
    nddt <- DT::datatable(
      newdata[, c("ID", input$newdata_col)],
      filter = "bottom",
      fillContainer = FALSE, rownames = FALSE,
      extensions = c("ColReorder",
                     "Scroller",
                     "Buttons"),
      options = list(dom = "lfrtip",
                     deferRender = TRUE,
                     scrollX = TRUE,
                     searchHighlight = TRUE,
                     colReorder = TRUE,
                     rownames = FALSE

                     )
    )
    nddt
  })

}


# Ejecutar la aplicacion ---------------------------------------------------
shinyApp(ui = ui, server = server)

# Para ver el codigo ejecutado en cada interaccion con le usuarie, usar este
# comando (ejecutar desde directorio de trabajo = directorio de la app):
# runApp(display.mode = "showcase")
