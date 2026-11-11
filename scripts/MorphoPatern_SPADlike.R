library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)

# ==============================================================================
# PASO 1: EVALUACIÓN DIAGNÓSTICA DEL NÚMERO ÓPTIMO DE GRUPOS (k)
# ==============================================================================

# Extraer las coordenadas factoriales de las especies en el MCA
coordenadas_mca <- res_mca$ind$coord
coordenadas_mca <- res.mca$ind$coord

# 1.1 Criterio del Ancho de Silueta Promedio (Silhouette Method)
# Evalúa qué tan bien encaja cada especie dentro de su grupo frente a los grupos vecinos
plot_silueta <- fviz_nbclust(
  coordenadas_mca, 
  kmeans, 
  method = "silhouette", 
  k.max = 10
) +
  labs(
    title = "Criterio 1: Ancho de Silueta Promedio",
    subtitle = "El pico máximo indica el número de grupos con mayor separación matemática",
    x = "Número de Clústeres (k)",
    y = "Ancho de Silueta Promedio"
  ) +
  theme_minimal()

print(plot_silueta)


# ==============================================================================
# PASO 2: CLASIFICACIÓN JERÁRQUICA Y CONSOLIDACIÓN VÍA HCPC (Método Lebart / SPAD)
# ==============================================================================

# Si fijas nb.clust = -1, HCPC sugiere automáticamente el k óptimo según 
# la mayor pérdida de inercia interclúster (Ganancia de Inercia - ΔI).
# Si prefieres fijar un k específico guiado por la Silueta, reemplaza nb.clust = k (ej. 3 o 4)

res_hcpc <- HCPC(
  res_mca, 
  nb.clust = 6,        # -1 sugiere k automáticamente por la barra de inercia
  consol = TRUE,        # TRUE ejecuta la consolidación por centros móviles (K-means)
  iter.max = 50,        # Iteraciones para el algoritmo de K-means
  method = "ward",      # Criterio de aglomeración de Ward
  graph = FALSE
)


# ==============================================================================
# PASO 3: INSPECCIÓN DE RESULTADOS MATEMÁTICOS DE CORTE Y CONSOLIDACIÓN
# ==============================================================================

# 3.1 Gráfico del salto de Inercia (Inertia Gain)
# Muestra el "árbol de inercia" donde el salto más alto justifica el corte de k
plot_inercia <- fviz_viz_ghc(res_hcpc) # O puedes usar plot(res_hcpc, choice = "bar")
plot_inercia <- fviz_viz_ghc(res.hcpc) # O puedes usar plot(res_hcpc, choice = "bar")
plot(res_hcpc, choice = "bar")

# 3.2 Imprimir en consola el k seleccionado y métricas de inercia
cat("--- RESULTADOS MATEMÁTICOS DEL CORTE ---\n")
cat("Número final de grupos óptimos (k):", length(levels(res_hcpc$data.clust$clust)), "\n\n")

# 3.3 Extraer el dataset con los grupos consolidados
datos_consolidados <- res_hcpc$data.clust


# ==============================================================================
# PASO 4: VISUALIZACIÓN DEL ESPACIO FACTORIAL CON GRUPOS CONSOLIDADOS
# ==============================================================================

plot_mca_consolidado <- fviz_cluster(
  res_hcpc,
  repel = TRUE,            # Evita superposición de nombres de especies
  show.clust.cent = TRUE,  # Muestra los centroides móviles consolidados
  palette = "jco",         # Paleta de color limpia para publicaciones
  ggtheme = theme_minimal(),
  main = "Partición Consolidada (HCPC + Centros Móviles) en el Espacio MCA"
)

print(plot_mca_consolidado)

## Solución B: Planos Factoriales Combinados (Dim 1 vs Dim 2 y Dim 3 vs Dim 4)

library(patchwork) # Para unir gráficos

# Gráfico Dimensión 1 vs Dimensión 2
p_hcpc_dim1_2 <- fviz_cluster(
  res_hcpc,
  axes = c(1, 2),
  repel = TRUE,            # Evita superposición de nombres de especies
  show.clust.cent = TRUE,  # Muestra los centroides móviles consolidados
  palette = "jco",         # Paleta de color limpia para publicaciones
  ggtheme = theme_minimal(),
  main = "A) Plano Factorial Dim 1 vs Dim 2",
  legend.title = "Grupo"
)

# Gráfico Dimensión 3 vs Dimensión 4
p_hcpc_dim3_4 <- fviz_cluster(
  res_hcpc,
  axes = c(3, 4),
  repel = TRUE,            # Evita superposición de nombres de especies
  show.clust.cent = TRUE,  # Muestra los centroides móviles consolidados
  palette = "jco",         # Paleta de color limpia para publicaciones
  ggtheme = theme_minimal(),
  main = "B) Plano Factorial Dim 3 vs Dim 4",
  legend.title = "Grupo"
)

# Combinar ambas gráficas en una sola ventana horizontal
p_hcpc_combinado <- p_hcpc_dim1_2 + p_hcpc_dim3_4 + plot_layout(guides = "collect")
print(p_hcpc_combinado)