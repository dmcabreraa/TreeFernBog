# Cargar librerías del ecosistema tidyverse
install.packages('tidyverse')
install.packages('ggrepel')
library(tidyverse)
library(ggrepel) # <- Librería clave para el etiquetado inteligente

# --- PASO 1: Carga y preparación de datos ---
# NOTA: En tu entorno de trabajo real, carga tu archivo físico usando:
# datos <- read_csv("ruta/de/tu/archivo.csv")

# Para este ejemplo reproducible, simulamos la lectura directa de tu estructura CSV:
datos_csv <- "
Especie,Altitud_Minima,Altitud_Maxima
Calamagrostis effusa,3000,4500
Stipa ichu,2500,3800
Loxsomopsis pearcei,1800,3200
Jamesonia bogotensis,3200,4300
Polystichum muricatum,1000,2800
Adiantum poiretii,500,2200
Pteris muricata,1200,2600
Agrostis breviculmis,2800,4100
Blechnum loxense,2200,3900
Hymenophyllum glandulosum,1500,3300
"

datos <- read_csv(datos_csv)

# Cálculo de variables ecológicas críticas
datos_analisis <- TreeFernBog_Altitud %>%
  mutate(
    Amplitud = Altitud_Maxima - Altitud_Minima,
    Punto_Medio = (Altitud_Maxima + Altitud_Minima) / 2
  )

# --- PASO 2: Análisis de la Regla de Rapoport Altitudinal ---
# Correlación de Pearson entre el punto medio del rango y su amplitud
test_correlacion <- cor.test(datos_analisis$Punto_Medio, datos_analisis$Amplitud, method = "pearson")

# Ajuste de un modelo de regresión lineal simple
modelo_rapoport <- lm(Amplitud ~ Punto_Medio, data = datos_analisis)
resumen_modelo <- summary(modelo_rapoport)

# Imprimir resultados en consola
cat("--- RESULTADOS DE LA PRUEBA DE RAPOPORT ---\n")
cat("Coeficiente de correlación de Pearson (r):", round(test_correlacion$estimate, 4), "\n")
cat("Valor p de la correlación:", round(test_correlacion$p.value, 5), "\n")
cat("R-cuadrado ajustado del modelo lineal:", round(resumen_modelo$adj.r.squared, 4), "\n\n")

# --- PASO 3: Identificación Estadística de Outliers (Criterio 1.5 * IQR) ---
q1 <- quantile(datos_analisis$Amplitud, 0.25)
q3 <- quantile(datos_analisis$Amplitud, 0.75)
iqr <- q3 - q1

limite_inferior <- q1 - (1.5 * iqr)
limite_superior <- q3 + (1.5 * iqr)

outliers <- datos_analisis %>%
  filter(Amplitud < limite_inferior | Amplitud > limite_superior)

cat("--- IDENTIFICACIÓN DE OUTLIERS (IQR) ---\n")
if (nrow(outliers) > 0) {
  print(outliers %>% select(Especie, Altitud_Minima, Altitud_Maxima, Amplitud))
} else {
  cat("No se detectaron especies con amplitudes altitudinales atípicas (outliers).\n\n")
}

# --- PASO 4: Visualización Gráfica ---

# A. Gráfico de dispersión + regresión lineal (Rapoport)
grafico_rapoport <- ggplot(datos_analisis, aes(x = Punto_Medio, y = Amplitud)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkred", fill = "lightcoral", alpha = 0.2) +
  labs(
    title = "Prueba de la Regla de Rapoport Altitudinal",
    subtitle = paste("Correlación r =", round(test_correlacion$estimate, 3), 
                     "| Valor p =", round(test_correlacion$p.value, 4)),
    x = "Punto Medio Altitudinal (m s.n.m.)",
    y = "Amplitud Altitudinal (m)"
  ) +
  theme_minimal()

# Gráfica con etiquetas inteligentes
ggplot(datos_analisis, aes(x = Punto_Medio, y = Amplitud)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkred", fill = "lightcoral", alpha = 0.15) +
  
  # AGREGAR ETIQUETAS QUE EVITAN EL SOLAPAMIENTO:
  geom_text_repel(
    aes(label = Especie),
    size = 3.2,                 # Tamaño de la letra
    box.padding = 0.5,          # Espacio alrededor del texto
    point.padding = 0.3,        # Espacio alejado del punto
    segment.color = "grey60",   # Color de la línea conectora si el texto se aleja mucho
    segment.size = 0.4,         # Grosor de la línea conectora
    max.overlaps = Inf          # Asegura que muestre todas las etiquetas, incluso si hay muchas
  ) +
  
  labs(
    title = "Prueba de la Regla de Rapoport Altitudinal",
    subtitle = paste("Correlación r =", round(test_correlacion$estimate, 3), 
                     "| Valor p =", round(test_correlacion$p.value, 4)),
    x = "Punto Medio Altitudinal (m s.n.m.)",
    y = "Amplitud Altitudinal (m)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11)
  )

# B. Boxplot con puntos de distribución individuales para identificar outliers
grafico_outliers <- ggplot(datos_analisis, aes(x = "", y = Amplitud)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 3, alpha = 0.7) +
  geom_jitter(color = "darkblue", width = 0.1, size = 2, alpha = 0.6) +
  labs(
    title = "Distribución de Amplitudes Altitudinales",
    subtitle = "El boxplot resalta límites para la detección de valores atípicos",
    x = "",
    y = "Amplitud Altitudinal (m)"
  ) +
  theme_minimal()

# Desplegar los gráficos
print(grafico_rapoport)
print(grafico_outliers)