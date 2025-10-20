# TRABAJO PRACTICO DE LABORATORIO DE ANALISIS DE DATOS - R
# Cruceño Karen, Yapur Najla, Borahona Pineda, Blanca

# Ejercicio de Analisis de datos:

# primero cargo la libreria tydiverse
install.packages("tidyverse")
library(tidyr)
library(ggplot2)
library(tidyverse)
# cargo el CSV sobre Spotify Tracks
data <- read.csv("/Users/ka/Documents/LAB/dataset.csv")

data %>%
  count(track_genre, sort = TRUE) # observamos que hay 114 generos distintos.

# Pregunta 1:
# Cuales son los 5 generos mas populares y las 5 menos populares de Spotify? 
# para este inciso utilizo la columna "popularity":

# calculo los 5 más escuchados:
top5_genre <- data %>%
  group_by(track_genre) %>%                    
  summarise(prom_popularity = mean(popularity, na.rm = TRUE)) %>%  
  arrange(desc(prom_popularity)) %>%           
  slice_head(n = 5)   
top5_genre

# Grafico
ggplot(top5_genre, aes(x = reorder(track_genre, prom_popularity), 
                       y = prom_popularity, 
                       fill = track_genre)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Los 5 géneros más populares en Spotify",
       x = "Género",
       y = "Popularidad promedio")


# Los 5 menos escuchados:
ultimos5_genre <- data %>%
  group_by(track_genre) %>%              
  summarise(prom_popularity = mean(popularity, na.rm = TRUE)) %>%  
  arrange(prom_popularity) %>%           
  slice_head(n = 5)                     

ultimos5_genre
#Grafico:
ggplot(ultimos5_genre, aes(x = reorder(track_genre, prom_popularity), 
                           y = prom_popularity, 
                           fill = track_genre)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Los 5 géneros menos populares en Spotify",
       x = "Género",
       y = "Popularidad promedio")

# Pregunta 2:
# Calculo la correlacion entre "energy" y "danceability":

r <- cor(data$energy, data$danceability, use = "everything") # Chequeamos antes que no hayan NAs
# r = 0.1343255. Observamos que las variables tienen correlación positiva, cercana a cero.

# Grafico:
data %>% ggplot(aes(x = energy, y = danceability)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre energía y capacidad de baile de las canciones",
       x = "Energy",
       y = "Danceability") +
  theme_minimal()

#Ejercicio 3:

#Calculo el promedio y la desaviacion estandar de la variable 'popularity' segun si el track 
# tiene contenido explciito o no. Calculamos, además, la cantidad de canciones según 'explicit'.
data %>%
  group_by(explicit) %>%
  summarise(prom_popularity = mean(popularity, na.rm = TRUE),
            sd_popularity = sd(popularity, na.rm = TRUE),
            observaciones = n())
# Graficamos el boxplot:
data %>%
  ggplot(aes(x = explicit, y = popularity, fill = explicit)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Distribución de popularidad según contenido explícito",
       x = "Explicit",
       y = "Popularity"
  ) +
  theme_minimal()

# ================================================================================
#Ejercicio 2: Analisis econometrico con datos de Gapminder
# PARTE 1: INGRESO POR PERSONA

# cargo librerias:
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
#cargamos el CSV:
gapminder <- read_csv("/Users/ka/Documents/LAB/gapminder (1).csv", na= "")

#view(gapminder)
#1. evolucion temporal de income_per_person en Argentina:

# Convertir 'income_per_person' a numérico:
gapminder <- gapminder %>% 
  mutate(income_per_person = as.numeric(income_per_person))

# primero extraigo la columna de Argentina:
argentina <- gapminder %>% filter(country == "Argentina") %>%
  mutate(income_per_person = as.numeric(income_per_person))

argentina$income_per_person <- as.numeric(as.character(argentina$income_per_person))

# grafico la evolución del ingreso per cápita en Argentina:
plot1 <- ggplot(argentina, aes(x = year, y = income_per_person)) +
  geom_line(color = "red", linewidth= 0.5) +
  geom_point(color="blue") +
  labs(
    title = "Evolución del ingreso per cápita en Argentina",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()
plot1
# Se observa una tendencia creciente aunque marcada por una alta volatilidad.


#Inciso 2:
# Para este inciso primero separamos los ultimos 10 años y los dejamos para el testeo.
#view(argentina$year) # Observamos que Argentina toma los datos desde 1960 hasta 2010.
train <- argentina %>% filter(year <= max(year) - 10) # filtro los años para argetina menos los
# ultimos 10 años. Es decir, tomamos como datos los años de 1960 al 2000. 
#view(gdp_percap_t) 
# Los datos de 2001 al 2010 los considero para el testeo;
test <- argentina %>% filter(year >max(year) - 10)

# Estimamos las regresiones para cada modelo:
#a) Modelo lineal: income per person = β1t + β0 + ε
modelo_lineal <- lm(income_per_person ~ year, data = train)
summary(modelo_lineal)
coeficientes <- coef(modelo_lineal)
# b) modelo polinomico de grado 2:
modelo_pol_2 <- lm(income_per_person ~ poly(year, 2), data = train)
summary(modelo_pol_2)
coeficientes2 <- coef(modelo_pol_2)
coeficientes2
# c) modelo polinomico de grado 10;
modelo_pol_10 <- lm(income_per_person ~ poly(year, 10), data = train)
summary(modelo_pol_10)
coeficientes10 <- coef(modelo_pol_10)
print(coeficientes10)

# Graficamos los tres modelos sobre los datos observados:
# modelo lineal:
pred_train_1 <- predict(modelo_lineal, newdata= train)

ggplot(train, aes(x = year, y = income_per_person)) +
  geom_point(color = "red", size = 2) +
  geom_line(aes(y = pred_train_1), color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Modelo lineal",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

# modelo polinomico de grado 2;
pred_train_2 <- predict(modelo_pol_2, newdata = train)
ggplot(train, aes(x = year, y = income_per_person)) +
  geom_point(color = "red", size = 2) +
  geom_line(aes(y = pred_train_2), color = "steelblue", size = 1.2) +
  labs(
    title = "Modelo Polinómico de Grado 2",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

#modelo polinomico de grado 10:
pred_train_10 <- predict(modelo_pol_10, newdata = train)
ggplot(train, aes(x = year, y = income_per_person)) +
  geom_point(color = "red", size = 2) +
  geom_line(aes(y = pred_train_10), color = "steelblue", size = 1.2) +
  labs(
    title = "Modelo Polinómico de Grado 10",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

# Ahora sobre el conjunto de testeo:
# predicciones de cada modelo con el conjunto de prueba:
pred_lineal <- predict(modelo_lineal, newdata = test)
pred_pol_2 <- predict(modelo_pol_2, newdata = test)
pred_pol_10 <- predict(modelo_pol_10, newdata = test)

# Grafico:
# modelo lineal
ggplot(test, aes(x = year, y = income_per_person)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = pred_lineal), color = "red", size = 1.2) +
  labs(
    title = "Predicciones del Modelo Lineal",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

# modelo polinomico de grado 2:
ggplot(test, aes(x = year, y = income_per_person)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = pred_pol_2), color = "blue", size = 1.2) +
  labs(
    title = "Predicciones del Modelo Polinómico Grado 2",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

#modelo polinomico de grado 10:
ggplot(test, aes(x = year, y = income_per_person)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = pred_pol_10), color = "green", size = 1.2) +
  labs(
    title = "Predicciones del Modelo Polinómico Grado 10",
    x = "Año",
    y = "Ingreso per cápita"
  ) +
  theme_minimal()

# Cuantificamos los resultados sobre el conjunto de testeo:
pred_lineal <- predict(modelo_lineal, newdata = test)
pred_pol_2 <- predict(modelo_pol_2, newdata = test)
pred_pol_10 <- predict(modelo_pol_10, newdata = test)

rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))

rmse_lin  <- rmse(test$income_per_person, pred_lineal)
rmse_2    <- rmse(test$income_per_person, pred_pol_2)
rmse_10   <- rmse(test$income_per_person, pred_pol_10)
rmse_lin
rmse_2
rmse_10
# Observamos que el modelo polinomico de grado 10 presenta un sobreajuste en los datos.

# Visualizo las predicciones sobre el conjunto de testeo:
ggplot() +
  geom_point(data = test, aes(x = year, y = income_per_person), color = "black") +
  geom_line(aes(x = test$year, y = pred_lineal, color = "Lineal")) +
  geom_line(aes(x = test$year, y = pred_pol_2, color = "Grado 2")) +
  geom_line(aes(x = test$year, y = pred_pol_10, color = "Grado 10")) +
  labs(title = "Predicciones sobre el conjunto de testeo",
       y = "Income per person", x = "Año")

# Inciso 3: selecciono 4 paises sudamericanos.
# Obervamos los paises pertenencientes a "america":
paises_america <- gapminder %>%
  filter(world_region == "america") %>%
  select(country) %>%
  distinct() %>%
  pull()

# selecciono Brazil, Uruguay, Chile y Peru.
# a) Matriz de correlaciones entre los ingresos (income per person) de los cinco paıses.
paises_sudam <- c("Brazil", "Uruguay", "Chile", "Peru", "Argentina")

datos_ingresos <- gapminder %>%
  filter(country %in% paises_sudam) %>%
  select(year, country, income_per_person) %>% 
  pivot_wider(names_from = country, values_from = income_per_person) %>%
  select(-year) %>%
  na.omit()

matriz_cor <- cor(datos_ingresos, method = "pearson")
print(matriz_cor)

# (b) Una matriz de correlaciones entre las variaciones porcentuales anuales
#(crecimiento interanual, Y /Y ) de dichos ingresos.

datos_crecimiento <- datos_ingresos %>%
  mutate(across(everything(),
                ~ ((. - lag(.)) / lag(.)) * 100)) %>%
  na.omit()
print(datos_crecimiento)

matriz_cor_crec <- cor(datos_crecimiento, method = "pearson")
print(matriz_cor_crec)

#================================================================================
# PARTE 2: ESPERANZA DE VIDA Y GENERO
install.packages(c("lmtest", "car"))

library(lmtest)
library(car)
# Filtrar un año en particular (ejemplo: 2000)
gap_2000 <- gapminder %>% filter(year == 2000)

# Asegurar que las variables sean numéricas
gap_2000$life_expectancy[gap_2000$life_expectancy == "-"] <- NA
gap_2000$life_expectancy_female[gap_2000$life_expectancy_female == "-"] <- NA
gap_2000$life_expectancy <- as.numeric(gap_2000$life_expectancy)
gap_2000$life_expectancy_female <- as.numeric(gap_2000$life_expectancy_female)
gap_2000$income_per_person <- as.numeric(gap_2000$income_per_person)

#5. Gráfica life expectancy frente a life expectancy female. 
#¿Qué relación observas entre ambas variables?
# Gráfico de dispersión (scatter plot)
ggplot(gap_2000, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Relación entre esperanza de vida total y femenina (año 2000)",
    x = "Esperanza de vida femenina",
    y = "Esperanza de vida total"
  ) +
  theme_minimal()
# Observamos una tendencia creciente entre ambas variables. A medida que aumenta la esperanza de vida
# total, aumenta la esperanza de vida femenina.

#6. Estima una regresión simple de life expectancy sobre life expectancy female. 
#Analiza el valor del coeficiente estimado. Calcular R2 . ¿Que observa? 
#¿Que implica este resultado?

# Ajuste del modelo lineal

mod_gap <- lm(life_expectancy ~ life_expectancy_female, data = gap_2000)
sum_gap <- summary(mod_gap)

cat("=== Modelo: life_expectancy ~ life_expectancy_female ===\n")
print(sum_gap)

# El valor del coeficiente estimado es 0.86534. Representa el efecto marginal de aunmentar la
# esperanza de vida femenina.

# Bondad del ajuste. Calculamos R²:
R2_sum <- sum_gap$r.squared
R2_sum
# R2 = 0.9424291. El R2 sirve para saber que tan bien explica mi modelo los datos. 
# Se encuentra entre 0 y 1, siendo cercano a 1 que explica muy bien. En nuestro 
# ejercicio obtuvimos un R2 significativo, es decir, el modelo tiene un alto poder
# explicativo.

# Gráfico con la recta ajustada
plot(gap_2000$life_expectancy_female, gap_2000$life_expectancy,
     pch = 16, col = rgb(0,0,1,0.5),
     main = "Regresión: Esperanza de vida total ~ femenina (2000)",
     xlab = "Esperanza de vida femenina",
     ylab = "Esperanza de vida total")
abline(mod_gap, col = "red", lwd = 2)
legend("topleft", legend = c("Recta estimada"), col = "red", lwd = 2, bty = "n")


#7. Contrasta la hipótesis nula H0: life expectancy female = life expectancy 
# contra la alternativa de que life expectancy female es mayor. ¿Cuál es la 
# evidencia empírica al respecto?

# Creamos la diferencia entre esperanza de vida femenina y total
gap_2000$diff_female_total <- gap_2000$life_expectancy_female - gap_2000$life_expectancy

# Test t de una muestra (unilateral)

# H0: media(diff) = 0
# H1: media(diff) > 0

t_diff <- t.test(gap_2000$diff_female_total, mu = 0,
                 alternative = "greater")


cat("=== Test H0: life_expectancy_female = life_expectancy ===\n")
print(t_diff)

# Interpretación:
cat("\n=== Interpretación ===\n")

media_diff <- mean(gap_2000$diff_female_total, na.rm = TRUE)
cat(sprintf("La diferencia media (femenina - total) es de %.2f años.\n", media_diff))

if (t_diff$p.value < 0.01) {
  cat("Evidencia MUY fuerte contra H0 (p < 0.01): las mujeres viven significativamente más.\n")
} else if (t_diff$p.value < 0.05) {
  cat("Evidencia moderada contra H0 (p < 0.05): las mujeres viven más.\n")
} else {
  cat("No se rechaza H0: no hay evidencia estadísticamente significativa.\n")
}

cat(sprintf("IC unilateral 95%%: [%.2f, +Inf)\n", t_diff$conf.int[1]))

#8. Estima una regresión múltiple de life expectancy sobre life expectancy female e 
#income per person. Interpreta los coeficientes de ambos regresores. Discute que 
# ocurre con la relación entre esperanza de vida y género al controlar por el nivel 
#de ingresos. Calcular R2 y comparar con la regresión simple. ¿Vale la pena incluir 
#la variable income per person?

# Eliminamos los NAs para evitar errores en el modelo
gap_2000<- na.omit(gap_2000[, c("life_expectancy",
                                "life_expectancy_female",
                                "income_per_person")])

# Estimación del modelo múltiple
mod_gap_multi <- lm(life_expectancy ~ life_expectancy_female + income_per_person,
                    data = gap_2000)
summary(mod_gap_multi) # observamos los resultados
# Calculamos el R2 de modelo multiple (lo observamos tambien en summary(mod_gap_multi)):
R2_multiple <- summary(mod_gap_multi)$r.squared
print(R2_multiple)

# Se puede observar que life_expectancy_female explica casi en su totalidad el modelo.
# No es relevante inlcluir los ingresos, ya que posee un efecto muy bajo (o,ooooo162).
# Recaordamos el R2 del modelo simple = 0.9424. Al incluir los ingresos, el nuevo R2 no 
# demuestra un cambio significativo. El R2 cuadrado del modelo multiple es 0.9428, el 
# modelo simple se ajusta tan bien a mis datos como el modelo multiple.

#9. Excluyendo life expectancy female y las variables que por algún motivo considere
#conveniente excluir, elegir un modelo lineal de máximo tres covariables que explique
#life expectancy. Explicar y justificar cada paso. Aclaración: No es necesario que
#esas tres covariables sean columnas originales del dataset. Pueden generarse una 
#nueva columna a partir de las existentes.

# Para explicar life_expectancy se eligieron las siguientes variables del dataset: 
#child_mortality, income_per_person y children_per_woman. 

# calculamos la regresion:
mod_multi_3 <- lm(life_expectancy ~ income_per_person + child_mortality + children_per_woman,
                  data = gapminder)
summary(mod_multi_3)

#=================================================================================
#EJERCICIO 3: ENVIDO/ TRUCO (Simulación)

#Inciso 1: crear el mazo del Truco
# valores y palos
set.seed(123)
valores <- c(1:7, 10:12)
palos <- c("espadas", "bastos", "oros", "copas")

# crear todas las combinaciones posibles
mazo <- expand.grid(valor = valores, palo = palos)

# opcional: visualizar
head(mazo, 10)
nrow(mazo)  # debe dar 40

#Inciso 2:extracción aleatorio
#función sacar_al_azar 
sacar_al_azar <- function(mazo) {
# elegir 3 cartas al azar sin reemplazo
indices <- sample(1:nrow(mazo), size = 3, replace = FALSE)
  
mano <- mazo[indices, ]               # cartas obtenidas
mazo_restante <- mazo[-indices, ]     # mazo sin esas cartas
  
# devolver como lista
return(list(mano = mano, mazo_restante = mazo_restante))
}

# probar la función 
set.seed(123)  # para que sea reproducible
resultado <- sacar_al_azar(mazo)

resultado$mano          # las tres cartas que salieron
nrow(resultado$mazo_restante)  # debería quedar 37

# simular muchas manos y graficar 
n_sim <- 10000
valores_medios <- numeric(n_sim)
for (i in 1:n_sim) {
  mano_i <- sacar_al_azar(mazo)$mano
  valores_medios[i] <- mean(mano_i$valor)  # valor medio de las tres cartas
}

# graficar histograma 
hist(valores_medios, breaks = 20, col = "steelblue",
     main = "Distribución del valor medio de 3 cartas al azar",
     xlab = "Valor medio",
     border = "white")
#Inciso 3: función contar_tantos +las reglas del envido
set.seed(123)
contar_tantos <- function(mano) {
  stopifnot(is.data.frame(mano), all(c("valor","palo") %in% names(mano)), nrow(mano) == 3)
  
# Coerciones seguras
valores_raw <- mano$valor
 if (is.factor(valores_raw)) valores_raw <- as.character(valores_raw)
  valores_raw <- as.numeric(valores_raw)  # asegura numérico
  
palos <- as.character(mano$palo)
  # Valores de envido: 1–7 valen su número; 10/11/12 valen 0
  valores_envido <- ifelse(valores_raw %in% 1:7, valores_raw, 0)
  
# ¿Hay al menos dos del mismo palo?
  cuenta_palos <- table(palos)
  palos_con_pareja <- names(cuenta_palos)[cuenta_palos >= 2]
  
if (length(palos_con_pareja) > 0) {
    # Para cada palo con ≥2 cartas, sumo las 2 mayores y agrego 20
    candidatos <- sapply(palos_con_pareja, function(p) {
      x <- valores_envido[palos == p]
      suma2 <- sum(sort(x, decreasing = TRUE)[1:2])
      suma2 + 20
    })
    return(max(candidatos, na.rm = TRUE))
  } else {
    # Sin pareja: el tanto es el mayor valor individual
    return(max(valores_envido, na.rm = TRUE))
  }
}
set.seed(123)
ej <- sacar_al_azar(mazo)$mano
ej; contar_tantos(ej)

#Inciso 4: probabilidades
# --- Probabilidad exacta de que el oponente gane el envido (2 jugadores) ---
# Regla: si hay empate, gana quien es mano (param: oponente_es_mano).
set.seed(123)
prob_oponente_gana_envido <- function(mano, mazo_restante, oponente_es_mano = TRUE) {
  stopifnot(is.data.frame(mano), nrow(mano) == 3)
  if (nrow(mazo_restante) < 3) stop("mazo_restante necesita >= 3 cartas.")
  
mi_tanto <- contar_tantos(mano)
  if (is.na(mi_tanto)) stop("Mi tanto dio NA. Revisá 'valor' numérico y 'palo' carácter.")
  
n <- nrow(mazo_restante)
combs <- combn(n, 3)
  
res <- apply(combs, 2, function(idx) {
    tant_op <- contar_tantos(mazo_restante[idx, , drop = FALSE])
    if (tant_op >  mi_tanto) return(1L)                    # gana oponente
    if (tant_op <  mi_tanto) return(0L)                    # pierde oponente
    return(as.integer(oponente_es_mano))                   # empate → mano
  })
  
mean(res)  # proporción de combinaciones ganadas por el oponente
}
set.seed(123)
tmp       <- sacar_al_azar(mazo)
mano   <- tmp$mano
restante  <- tmp$mazo_restante

# calculamos la prob exacta
p_op_mano    <- prob_oponente_gana_envido(mano, restante, oponente_es_mano = TRUE)   # si el rival es mano
p_op_no_mano <- prob_oponente_gana_envido(mano, restante, oponente_es_mano = FALSE)  # si vos sos mano
p_op_mano; p_op_no_mano

#Inciso 5: Caso de juego de 6 jugadores

# Función que simula muchas partidas 3 vs 3
simular_envido_3v3 <- function(mano, mazo, oponente_es_mano = TRUE, n_sim = 5000) {
  # Elimino tus cartas del mazo
  keys <- paste(mazo$valor, mazo$palo)
  restantes <- mazo[!(keys %in% paste(mano$valor, mano$palo)), , drop = FALSE]
  
mi_tanto <- contar_tantos(mano)
victorias_equipo <- 0
  
  for (i in 1:n_sim) {
    # Robar 6 manos para los otros 6 jugadores
    idx <- sample(seq_len(nrow(restantes)), 6*3, replace = FALSE)
    cartas <- restantes[idx, , drop = FALSE]
    manos <- split(cartas, rep(1:6, each = 3))
    
    # el resto de los compañeros: jugadores 1 y 2 (del mismo equipo)
    tantos_equipo <- c(mi_tanto,
                       contar_tantos(manos[[1]]),
                       contar_tantos(manos[[2]]))
    
    # rivales: jugadores 3 a 5 + 6 (oponente mano o no)
    tantos_rival <- sapply(manos[3:6], contar_tantos)
    
    # Filtramos compañeros con menos de 27 tantos
    if (any(tantos_equipo[-1] >= 27)) next
    
    max_equipo <- max(tantos_equipo)
    max_rival  <- max(tantos_rival)
    
    gana_equipo <- if (max_equipo > max_rival) {
      TRUE
    } else if (max_equipo < max_rival) {
      FALSE
    } else {
      oponente_es_mano   # empate: gana el equipo que es mano
    }
    
    victorias_equipo <- victorias_equipo + as.integer(gana_equipo)
  }
  prob <- victorias_equipo / n_sim
  return(prob)
}

# ----- Ejemplo de uso -----
set.seed(123)
tmp  <- sacar_al_azar(mazo)
mano <- tmp$mano

# Supongamos que el equipo rival es mano
prob_ganar_equipo <- simular_envido_3v3(mano, mazo, oponente_es_mano = TRUE, n_sim = 2000)
prob_ganar_equipo

#Inciso 6: Simulaciones e histograma 
simular_max_envido_rival <- function(mano, mazo, n_sim = 5000) {
  # Eliminar tus cartas del mazo
  keys <- paste(mazo$valor, mazo$palo)
  restantes <- mazo[!(keys %in% paste(mano$valor, mano$palo)), , drop = FALSE]
  
  maximos_rivales <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    # Robar 6 manos (3 para tus compañeros, 3 para los rivales)
    idx <- sample(seq_len(nrow(restantes)), 6 * 3, replace = FALSE)
    cartas <- restantes[idx, , drop = FALSE]
    manos <- split(cartas, rep(1:6, each = 3))
    
    # Calcular los tantos del equipo rival (jugadores 4,5,6)
    tantos_rival <- sapply(manos[4:6], contar_tantos)
    maximos_rivales[i] <- max(tantos_rival)
  }
  
  return(maximos_rivales)
}

# ---- Ejecutar simulación y graficar ----
set.seed(123)
tmp  <- sacar_al_azar(mazo)
mano <- tmp$mano

max_rival <- simular_max_envido_rival(mano, mazo, n_sim = 5000)

# Histograma
hist(max_rival,
     breaks = 15,
     col = "black",
     main = "Distribución del máximo puntaje del equipo rival",
     xlab = "Máximo tanto de envido (rival)",
     border = "white")

# Mostrar algunos estadísticos
summary(max_rival)












