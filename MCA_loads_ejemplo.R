library(factoextra)
library(ggplot2)
# opcional: library(patchwork) o library(gridExtra) si quieres combinarlas en una sola imagen

# Definir el total de ejes retenidos en el MCA
ejes_totales <- 1:ncol(res_mca$var$contrib)


# --- GRÁFICO 1: Top 15 Categorías/Variables (Todos los ejes) ---
plot_top_var <- fviz_contrib(
  res_mca,
  choice = "var",
  top = 15,
  axes = ejes_totales,
  fill = "#2E9FDF",
  color = "#2E9FDF",
  ggtheme = theme_minimal()
) +
  labs(
    title = "Top 15 Categorías Morfológicas con Mayor Pesos/Contribución",
    subtitle = paste("Acumulado a lo largo de todos los ejes (1 a", max(ejes_totales), ")"),
    x = "Categoría Morfológica",
    y = "Contribución (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )


# --- GRÁFICO 2: Top 15 Especies/Individuos (Todos los ejes) ---
plot_top_ind <- fviz_contrib(
  res_mca,
  choice = "ind",
  top = 15,
  axes = ejes_totales,
  fill = "#E7B800",
  color = "#E7B800",
  ggtheme = theme_minimal()
) +
  labs(
    title = "Top 15 Especies con Mayor Pesos/Contribución",
    subtitle = paste("Acumulado a lo largo de todos los ejes (1 a", max(ejes_totales), ")"),
    x = "Especie (Código E)",
    y = "Contribución (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )


# --- Visualizar los gráficos ---
print(plot_top_var)
print(plot_top_ind)


# --- OPCIONAL: Mostrar ambas gráficas juntas en una sola ventana (con patchwork) ---
library(patchwork)
plot_top_var / plot_top_ind

## Solución A: Mapa de Calor de los 5 Ejes al tiempo (Heatmap de Contribuciones)

library(pheatmap)
library(RColorBrewer)

# 1. Extraer la matriz de contribuciones de las variables para los 5 ejes
contrib_var_5ejes <- res_mca$var$contrib[, 1:5]

# Cambiar los nombres de las columnas para mayor claridad
colnames(contrib_var_5ejes) <- paste0("Dim ", 1:5)

# 2. Generar el Mapa de Calor para el Top 25 de categorías con mayor contribución total
top_25_var <- names(sort(rowSums(contrib_var_5ejes), decreasing = TRUE)[1:25])
matriz_top25 <- contrib_var_5ejes[top_25_var, ]

# Gráfico Heatmap
pheatmap(
  matriz_top25,
  color = colorRampPalette(brewer.pal(n = 9, name = "YlOrRd"))(100),
  cluster_rows = TRUE,    # Agrupa variables con comportamiento similar
  cluster_cols = FALSE,   # Mantiene el orden de los ejes (Dim 1 a 5)
  display_numbers = TRUE, # Muestra el valor % de contribución dentro de la celda
  number_format = "%.1f",
  fontsize_number = 8,
  main = "Contribución (%) de las Categorías Morfológicas en los 5 Ejes del MCA"
)

## Solución B: Planos Factoriales Combinados (Dim 1 vs Dim 2 y Dim 3 vs Dim 4)

library(factoextra)
library(patchwork) # Para unir gráficos

# Gráfico Dimensión 1 vs Dimensión 2
p_dim1_2 <- fviz_mca_ind(
  res_mca,
  axes = c(1, 2),
  habillage = datos_con_grupos$Grupo_Morfologico,
  palette = mis_colores,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  title = "A) Plano Factorial Dim 1 vs Dim 2",
  legend.title = "Grupo"
)

# Gráfico Dimensión 3 vs Dimensión 4
p_dim3_4 <- fviz_mca_ind(
  res_mca,
  axes = c(3, 4),
  habillage = datos_con_grupos$Grupo_Morfologico,
  palette = mis_colores,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  title = "B) Plano Factorial Dim 3 vs Dim 4",
  legend.title = "Grupo"
)

# Combinar ambas gráficas en una sola ventana horizontal
p_combinado <- p_dim1_2 + p_dim3_4 + plot_layout(guides = "collect")
print(p_combinado)