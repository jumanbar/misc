# Tabla con las posiciones actuales (4/10/2017)
tabla <- data.frame(Pos = 1:10, Seleccion = c("BRA", "URU", "COL", "PER", "ARG", "CHI", "PAR", "ECU", "BOL", "VEN"), 
                    Pts = c(37L, 27L, 26L, 24L, 24L, 23L, 21L, 20L, 13L, 8L),
                    PJ = c(16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L),
                    G = c(11L, 8L, 7L, 7L, 6L, 7L, 6L, 6L, 4L, 1L),
                    E = c(4L, 3L, 5L, 3L, 6L, 2L, 3L, 2L, 1L, 5L),
                    P = c(1L, 5L, 4L, 6L, 4L, 7L, 7L, 8L, 11L, 10L),
                    GF = c(38L, 28L, 19L, 26L, 16L, 24L, 17L, 24L, 14L, 18L),
                    GC = c(11L, 18L, 16L, 25L, 15L, 23L, 23L, 24L, 34L, 35L),
                    Dif = c(27L, 10L, 3L, 1L, 1L, 1L, -6L, 0L, -20L, -17L))

# Listas de locales y visitantes para las próximas 10 fechas:
local <- rep(0, 10)
names(local) <- c("BOL", "VEN", "CHI", "COL", "ARG", "ECU", "PER", "PAR", "BRA", "URU")
visitante <- rep(0, 10)
names(visitante) <- c("BRA", "URU", "ECU", "PAR", "PER", "ARG", "COL", "VEN", "CHI", "BOL")
puntos <- tabla$Pts
names(puntos) <- tabla$Seleccion

# Codificación de los resultados:
#  1: Gana el local
#  0: Empate
#  2: Gana el visitante
fpuntos_loc <- function(x) ifelse(x == -1, 0, ifelse(x == 0, 1, 3))
fpuntos_vis <- function(x) ifelse(x == -1, 3, ifelse(x == 0, 1, 0))

# Todas las combinaciones de resultados posibles:
library(gtools)
x <- c(-1, 0, 1)
permu <- permutations(3, 10, x, repeats.allowed = TRUE)
dim(permu)
head(permu)

# Función para calcular puntajes finales:
ffinal <- function(x=permu[1,], l=local, v=visitante, p=puntos) {
  p[names(l)] <- p[names(l)] + l + fpuntos_loc(x)
  p[names(v)] <- p[names(v)] + v + fpuntos_vis(x)
  return(p)
}

# Todos los puntajes finales para todos los 59049 escenarios posibles:
o <- t(apply(permu, 1, ffinal, local, visitante, puntos))

# Una función para determinar la posición final de Uruguay en un escenario x:
pos_uru <- function(x = o) rank(-x, ties.method = 'min')["URU"]

# Vector con las posiciones de Uruguay en todos los escenarios:
pos <- apply(o, 1, pos_uru)

range(pos) # Mejor escenario - peor escenario
table(pos) # Cantidad de combinaciones para cada posición posible

# Una bella gráfica:
r <- barplot(tp <- table(pos), col = 'skyblue', border = F,
             ylab = "Nro. de combinaciones", xlab = "Posición final de URUGUAY")
text(x = r, y = tp, labels = paste0(round(100 * tp / sum(tp), 3), " %"), pos = c(1, 3, 3, 3, 3, 3))


# Qué escenarios son los que nos dejan 6tos?:
p6 <- which(pos == 6)

# Imprimir esos 9 escenarios del horror:
lo <- names(local)
vi <- names(visitante)

for (j in 1:length(p6)) {
  y <- permu[p6[j],]
  cat("Escenario ", j, ":\n", sep = "")
  for (i in 1:10) {
    cat("\tPartido ", i, ": ", lo[i], 
        ifelse(y[i] == -1, " pierde con ", ifelse(y[i] == 0, " empata con ", " le gana a ")),
        vi[i], "\n", sep = "")
  }
  
}

# También se pueden meter dentro de una tabla:
texto <- function(y, l=names(local), v=names(visitante)) 
  paste0(l, ifelse(y == -1, " pierde con ", ifelse(y == 0, " empata con ", " le gana a ")), v)

texto(y, names(local), names(visitante))
Escenarios <- data.frame(Partido = 1:10, Local = lo, Visitante = vi,
           Escenario_1 = texto(permu[p6[1],]),
           Escenario_2 = texto(permu[p6[2],]),
           Escenario_3 = texto(permu[p6[3],]),
           Escenario_4 = texto(permu[p6[4],]),
           Escenario_5 = texto(permu[p6[5],]),
           Escenario_6 = texto(permu[p6[6],]),
           Escenario_7 = texto(permu[p6[7],]),
           Escenario_8 = texto(permu[p6[8],]),
           Escenario_9 = texto(permu[p6[9],]))

# Para escribir en un archivo csv:
write.csv(Escenarios, file = "Escenarios.csv", row.names = FALSE)

