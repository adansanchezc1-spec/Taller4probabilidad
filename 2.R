
# EJERCICIO: LANZAMIENTO DE DOS DADOS Y VARIABLE ALEATORIA W = X + Y

# Configuración de gráficos
par(mfrow = c(1, 1))

# ============================================================================
# 1. ESPACIO MUESTRAL
# ============================================================================
cat("1. GENERACIÓN DEL ESPACIO MUESTRAL\n")

# Crear todas las parejas ordenadas (x, y) posibles
dado1 <- 1:6
dado2 <- 1:6

# Generar espacio muestral completo
espacio_muestral <- expand.grid(Dado1 = dado1, Dado2 = dado2)
espacio_muestral$Suma <- espacio_muestral$Dado1 + espacio_muestral$Dado2

cat("Total de resultados posibles:", nrow(espacio_muestral), "\n\n")
cat("Primeros 10 resultados del espacio muestral:\n")
print(head(espacio_muestral, 10))
cat("\n...\n")
cat("Últimos 10 resultados del espacio muestral:\n")
print(tail(espacio_muestral, 10))
cat("\n")

# Mostrar espacio muestral en formato matriz
cat("Espacio muestral en formato de tabla (Dado1 vs Dado2):\n")
matriz_sumas <- matrix(espacio_muestral$Suma, nrow = 6, ncol = 6, byrow = TRUE)
rownames(matriz_sumas) <- paste("Dado1=", 1:6)
colnames(matriz_sumas) <- paste("Dado2=", 1:6)
print(matriz_sumas)
cat("\n")

# ============================================================================
# 2. DISTRIBUCIÓN DE PROBABILIDAD Y DISTRIBUCIÓN ACUMULADA
# ============================================================================
cat("============================================================================\n")
cat("2. DISTRIBUCIÓN DE PROBABILIDAD DE W\n")
cat("============================================================================\n\n")

# Calcular frecuencias de cada suma
tabla_frecuencias <- table(espacio_muestral$Suma)
valores_W <- as.numeric(names(tabla_frecuencias))
frecuencias <- as.numeric(tabla_frecuencias)

# Distribución de probabilidad
probabilidades <- frecuencias / sum(frecuencias)

# Distribución acumulada
prob_acumulada <- cumsum(probabilidades)

# Crear data frame con la distribución
dist_W <- data.frame(
  W = valores_W,
  Frecuencia = frecuencias,
  Probabilidad = probabilidades,
  Prob_Acumulada = prob_acumulada
)

cat("Distribución de Probabilidad de W:\n")
print(dist_W)
cat("\n")

# Visualización mejorada
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Gráfico 1: Función de Masa de Probabilidad (PMF)
barplot(probabilidades, 
        names.arg = valores_W,
        main = "Función de Masa de Probabilidad P(W = w)",
        xlab = "Valor de W (suma de los dados)",
        ylab = "Probabilidad",
        col = "steelblue",
        border = "darkblue",
        ylim = c(0, max(probabilidades) * 1.1))
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
text(x = barplot(probabilidades, plot = FALSE), 
     y = probabilidades + 0.01, 
     labels = round(probabilidades, 4), 
     cex = 0.7)

# Gráfico 2: Distribución Acumulada (CDF)
plot(valores_W, prob_acumulada, 
     type = "s",
     main = "Función de Distribución Acumulada F(w)",
     xlab = "Valor de W",
     ylab = "Probabilidad Acumulada",
     col = "darkred",
     lwd = 2,
     ylim = c(0, 1))
points(valores_W, prob_acumulada, pch = 19, col = "darkred", cex = 1.2)
grid()
abline(h = c(0.25, 0.5, 0.75), lty = 2, col = "gray")

# Gráfico 3: Frecuencias absolutas
barplot(frecuencias, 
        names.arg = valores_W,
        main = "Frecuencias Absolutas",
        xlab = "Valor de W",
        ylab = "Frecuencia",
        col = "lightgreen",
        border = "darkgreen")
text(x = barplot(frecuencias, plot = FALSE), 
     y = frecuencias + 0.3, 
     labels = frecuencias, 
     cex = 0.8)

# Gráfico 4: Diagrama de dispersión del espacio muestral
plot(espacio_muestral$Dado1, espacio_muestral$Dado2, 
     pch = 19, 
     col = rainbow(11)[espacio_muestral$Suma - 1],
     main = "Espacio Muestral Coloreado por Suma",
     xlab = "Dado 1",
     ylab = "Dado 2",
     cex = 1.5)
legend("topleft", 
       legend = 3:12, 
       col = rainbow(11), 
       pch = 19, 
       title = "Suma W",
       cex = 0.7,
       ncol = 2)
grid()

par(mfrow = c(1, 1))

# ============================================================================
# 3. MEDIDAS DE TENDENCIA CENTRAL Y POSICIÓN
# ============================================================================
cat("\n============================================================================\n")
cat("3. MEDIDAS DE TENDENCIA CENTRAL Y POSICIÓN\n")
cat("============================================================================\n\n")

# Valor esperado (media)
valor_esperado <- sum(valores_W * probabilidades)

# Mediana
mediana <- valores_W[which(prob_acumulada >= 0.5)[1]]

# Primer cuartil (Q1)
Q1 <- valores_W[which(prob_acumulada >= 0.25)[1]]

# Tercer cuartil (Q3)
Q3 <- valores_W[which(prob_acumulada >= 0.75)[1]]

cat("Valor Esperado E(W):", valor_esperado, "\n")
cat("Mediana:", mediana, "\n")
cat("Primer Cuartil (Q1):", Q1, "\n")
cat("Tercer Cuartil (Q3):", Q3, "\n")
cat("Rango Intercuartílico (IQR):", Q3 - Q1, "\n\n")

# Moda
moda <- valores_W[which.max(probabilidades)]
cat("Moda (valor más probable):", moda, "\n\n")

cat("============================================================================\n")
cat("ASOCIACIÓN CON JUEGOS DE MESA:\n")
cat("============================================================================\n")
cat("Estos resultados están directamente relacionados con:\n\n")
cat("1. MONOPOLY: Usa dos dados de 6 caras\n")
cat("   - La suma más probable es 7 (probabilidad ≈ 16.67%)\n")
cat("   - El valor esperado es 7, lo que significa que en promedio\n")
cat("     avanzarás 7 casillas por turno\n\n")
cat("2. BACKGAMMON: También usa dos dados estándar\n")
cat("   - Las combinaciones de suma 7 son las más frecuentes\n\n")
cat("3. CRAPS (juego de casino):\n")
cat("   - Ganar en la primera tirada: 7 u 11\n")
cat("   - Perder en la primera tirada: 2, 3 o 12\n")
cat("   - Probabilidad de ganar en primera tirada:\n")
cat("     P(7) + P(11) =", probabilidades[valores_W == 7], "+", 
    probabilidades[valores_W == 11], "=", 
    probabilidades[valores_W == 7] + probabilidades[valores_W == 11], "\n\n")


# 4. MEDIDAS DE DISPERSIÓN

cat("============================================================================\n")
cat("4. MEDIDAS DE DISPERSIÓN\n")
cat("============================================================================\n\n")

# Varianza
varianza <- sum((valores_W - valor_esperado)^2 * probabilidades)

# Desviación estándar
desviacion_std <- sqrt(varianza)

# Coeficiente de variación
coef_variacion <- (desviacion_std / valor_esperado) * 100

cat("Varianza Var(W):", round(varianza, 4), "\n")
cat("Desviación Estándar σ(W):", round(desviacion_std, 4), "\n")
cat("Coeficiente de Variación CV:", round(coef_variacion, 2), "%\n\n")

cat("Interpretación:\n")
cat("- La desviación estándar de", round(desviacion_std, 2), 
    "indica que los valores típicamente\n")
cat("  se desvían aproximadamente ±", round(desviacion_std, 2), 
    "unidades del valor esperado (7)\n")
cat("- Un CV de", round(coef_variacion, 2), 
    "% indica una dispersión moderada relativa a la media\n\n")

# ============================================================================
# 5. RESUMEN ESTADÍSTICO COMPLETO
# ============================================================================
cat("RESUMEN ESTADÍSTICO COMPLETO\n")

resumen <- data.frame(
  Medida = c("Tamaño del espacio muestral",
             "Valores posibles de W",
             "Valor mínimo",
             "Primer Cuartil (Q1)",
             "Mediana",
             "Media (Valor Esperado)",
             "Tercer Cuartil (Q3)",
             "Valor máximo",
             "Moda",
             "Rango",
             "Rango Intercuartílico",
             "Varianza",
             "Desviación Estándar",
             "Coeficiente de Variación"),
  Valor = c(nrow(espacio_muestral),
            paste(min(valores_W), "-", max(valores_W)),
            min(valores_W),
            Q1,
            mediana,
            round(valor_esperado, 4),
            Q3,
            max(valores_W),
            moda,
            max(valores_W) - min(valores_W),
            Q3 - Q1,
            round(varianza, 4),
            round(desviacion_std, 4),
            paste0(round(coef_variacion, 2), "%"))
)
print(resumen, row.names = FALSE)

# ============================================================================
# 6. VERIFICACIÓN POR SIMULACIÓN
# ============================================================================
cat("VERIFICACIÓN POR SIMULACIÓN (100,000 lanzamientos)\n")

set.seed(123)
n_sim <- 100000
dado1_sim <- sample(1:6, n_sim, replace = TRUE)
dado2_sim <- sample(1:6, n_sim, replace = TRUE)
sumas_sim <- dado1_sim + dado2_sim

# Estadísticas de la simulación
media_sim <- mean(sumas_sim)
var_sim <- var(sumas_sim)
sd_sim <- sd(sumas_sim)
mediana_sim <- median(sumas_sim)

cat("Comparación Teórico vs Simulación:\n")
cat(sprintf("%-25s %10s %10s\n", "Medida", "Teórico", "Simulación"))
cat(sprintf("%-25s %10.4f %10.4f\n", "Media", valor_esperado, media_sim))
cat(sprintf("%-25s %10.4f %10.4f\n", "Varianza", varianza, var_sim))
cat(sprintf("%-25s %10.4f %10.4f\n", "Desv. Estándar", desviacion_std, sd_sim))
cat(sprintf("%-25s %10.0f %10.0f\n", "Mediana", mediana, mediana_sim))

cat("\n")
cat("¡La simulación confirma los cálculos teóricos!\n")





