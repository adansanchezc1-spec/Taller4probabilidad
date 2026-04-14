# ============================================================================
# EJERCICIO DE COMBINATORIA Y PROBABILIDAD
# ============================================================================

# 1. ESTRUCTURA DE DATOS
# ============================================================================
mujeres <- c("MA", "MB", "MC", "MD")
hombres <- c("HA", "HB", "HC", "HD", "HE", "HF")
estudiantes <- c(mujeres, hombres)

cat("Total de estudiantes:", length(estudiantes), "\n")
cat("Mujeres:", length(mujeres), "\n")
cat("Hombres:", length(hombres), "\n\n")

# 2. ESPACIO MUESTRAL: TODAS LAS COMBINACIONES POSIBLES
# ============================================================================
# Usamos combn() para generar TODAS las combinaciones posibles de 3 personas
espacio_muestral <- combn(estudiantes, 3, simplify = FALSE)

# Convertir cada combinación a string para facilitar el análisis
espacio_muestral_str <- sapply(espacio_muestral, function(x) paste(sort(x), collapse = "-"))

cat("============================================\n")
cat("TOTAL DE COMBINACIONES POSIBLES (n=10, r=3):\n")
cat("C(10,3) =", length(espacio_muestral), "\n")
cat("============================================\n\n")

# Mostrar las primeras 10 combinaciones como ejemplo
cat("Primeras 10 combinaciones:\n")
print(head(espacio_muestral_str, 10))
cat("\n")

# 3. FUNCIÓN PARA CONTAR MUJERES EN UNA MUESTRA
# ============================================================================
contar_mujeres <- function(muestra) {
  sum(muestra %in% mujeres)
}

# 4. FUNCIÓN PARA VERIFICAR SI A Y D ESTÁN JUNTAS
# ============================================================================
contiene_A_y_D <- function(muestra) {
  "MA" %in% muestra && "MD" %in% muestra
}

# 5. CLASIFICACIÓN DE TODAS LAS MUESTRAS
# ============================================================================
clasificacion <- data.frame(
  muestra = espacio_muestral_str,
  num_mujeres = sapply(espacio_muestral, contar_mujeres),
  tiene_A_y_D = sapply(espacio_muestral, contiene_A_y_D)
)

# 6. CÁLCULO DE PROBABILIDADES
# ============================================================================

# a) Exactamente 2 mujeres
dos_mujeres <- sum(clasificacion$num_mujeres == 2)
prob_dos_mujeres <- dos_mujeres / length(espacio_muestral)

cat("============================================\n")
cat("a) EXACTAMENTE 2 MUJERES:\n")
cat("============================================\n")
cat("Combinaciones con 2 mujeres:", dos_mujeres, "\n")
cat("Probabilidad:", prob_dos_mujeres, "\n")
cat("Probabilidad (%):", round(prob_dos_mujeres * 100, 2), "%\n")
cat("Cálculo teórico: C(4,2) × C(6,1) = ", choose(4,2) * choose(6,1), "\n\n")

# b) Al menos 1 mujer
al_menos_una_mujer <- sum(clasificacion$num_mujeres >= 1)
prob_al_menos_una <- al_menos_una_mujer / length(espacio_muestral)

cat("============================================\n")
cat("b) AL MENOS 1 MUJER:\n")
cat("============================================\n")
cat("Combinaciones con ≥1 mujer:", al_menos_una_mujer, "\n")
cat("Probabilidad:", prob_al_menos_una, "\n")
cat("Probabilidad (%):", round(prob_al_menos_una * 100, 2), "%\n")
cat("Cálculo: 1 - P(0 mujeres) = 1 - C(6,3)/C(10,3) = 1 -", choose(6,3)/choose(10,3), "\n\n")

# c) No hay mujeres (todas son hombres)
cero_mujeres <- sum(clasificacion$num_mujeres == 0)
prob_cero_mujeres <- cero_mujeres / length(espacio_muestral)

cat("============================================\n")
cat("c) SIN MUJERES (SOLO HOMBRES):\n")
cat("============================================\n")
cat("Combinaciones sin mujeres:", cero_mujeres, "\n")
cat("Probabilidad:", prob_cero_mujeres, "\n")
cat("Probabilidad (%):", round(prob_cero_mujeres * 100, 2), "%\n")
cat("Cálculo teórico: C(6,3) = ", choose(6,3), "\n\n")

# d) MA y MD NO están juntas en la misma muestra
sin_A_y_D <- sum(!clasificacion$tiene_A_y_D)
prob_sin_A_y_D <- sin_A_y_D / length(espacio_muestral)

cat("============================================\n")
cat("d) MA y MD NO ESTÁN JUNTAS:\n")
cat("============================================\n")
cat("Combinaciones donde MA y MD NO aparecen juntas:", sin_A_y_D, "\n")
cat("Probabilidad:", prob_sin_A_y_D, "\n")
cat("Probabilidad (%):", round(prob_sin_A_y_D * 100, 2), "%\n")
cat("Cálculo: 1 - P(MA y MD juntas) = 1 - C(8,1)/C(10,3)\n")
cat("Combinaciones con MA y MD juntas:", sum(clasificacion$tiene_A_y_D), "\n\n")

# 7. RESUMEN EN TABLA
# ============================================================================
cat("============================================\n")
cat("RESUMEN DE RESULTADOS:\n")
cat("============================================\n")
resumen <- data.frame(
  Condicion = c("Exactamente 2 mujeres", 
                "Al menos 1 mujer", 
                "Sin mujeres", 
                "MA y MD NO juntas"),
  Casos_Favorables = c(dos_mujeres, al_menos_una_mujer, cero_mujeres, sin_A_y_D),
  Probabilidad = c(prob_dos_mujeres, prob_al_menos_una, prob_cero_mujeres, prob_sin_A_y_D),
  Porcentaje = round(c(prob_dos_mujeres, prob_al_menos_una, prob_cero_mujeres, prob_sin_A_y_D) * 100, 2)
)
print(resumen)
cat("\n")

# 8. VERIFICACIÓN CON SIMULACIÓN (OPCIONAL)
# ============================================================================
cat("============================================\n")
cat("VERIFICACIÓN POR SIMULACIÓN (100,000 muestras):\n")
cat("============================================\n")

n_simulaciones <- 100000
resultados_sim <- replicate(n_simulaciones, {
  sample(estudiantes, 3)
})

# Contar resultados de la simulación
sim_dos_mujeres <- sum(apply(resultados_sim, 2, contar_mujeres) == 2)
sim_al_menos_una <- sum(apply(resultados_sim, 2, contar_mujeres) >= 1)
sim_cero_mujeres <- sum(apply(resultados_sim, 2, contar_mujeres) == 0)
sim_sin_A_y_D <- sum(!apply(resultados_sim, 2, contiene_A_y_D))

cat("Exactamente 2 mujeres - Simulación:", round(sim_dos_mujeres/n_simulaciones, 4), 
    "| Teórico:", round(prob_dos_mujeres, 4), "\n")
cat("Al menos 1 mujer - Simulación:", round(sim_al_menos_una/n_simulaciones, 4), 
    "| Teórico:", round(prob_al_menos_una, 4), "\n")
cat("Sin mujeres - Simulación:", round(sim_cero_mujeres/n_simulaciones, 4), 
    "| Teórico:", round(prob_cero_mujeres, 4), "\n")
cat("MA y MD NO juntas - Simulación:", round(sim_sin_A_y_D/n_simulaciones, 4), 
    "| Teórico:", round(prob_sin_A_y_D, 4), "\n")

# 9. VISUALIZACIÓN (OPCIONAL)
# ============================================================================
cat("\n")
cat("Distribución de mujeres por muestra:\n")
tabla_distribucion <- table(clasificacion$num_mujeres)
print(tabla_distribucion)

barplot(tabla_distribucion, 
        main = "Distribución del número de mujeres en muestras de 3 personas",
        xlab = "Número de mujeres",
        ylab = "Frecuencia",
        col = "skyblue",
        border = "darkblue")

