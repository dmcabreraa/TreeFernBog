# 1. Cargar el paquete FactoMineR (instalar si no se tiene: install.packages("FactoMineR"))
library(FactoMineR)

# 2. Cargar el conjunto de datos de ejemplo 'decathlon'
data(decathlon)

# 3. Realizar el Análisis de Componentes Principales (PCA)
# Usamos las primeras 10 variables cuantitativas (pruebas del decatlón)
# Las variables 11:12 son suplementarias (Puntos y Clasificación) y la 13 es cualitativa (Competición)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)

# 4. Clasificación Jerárquica sobre Componentes Principales (HCPC)
# El argumento `consol = TRUE` activa la consolidación con K-means
res.hcpc <- HCPC(res.pca, 
                 nb.clust = 6,      # Fijamos 3 clústeres (o -1 para selección automática)
                 consol = FALSE,     # Activa la consolidación por K-means
                 iter.max = 10,     # Número máximo de iteraciones de K-means
                 graph = FALSE)     # graph = TRUE para hacer 3 gráficas(cluster, cluster/factor map, factor map)

# 5. Visualizar los resultados gráficos de la agrupación
plot(res.hcpc, choice = "3D.map")  # Mapa 3D de los componentes y los clústeres
plot(res.hcpc, choice = "map")     # Proyección 2D del PCA con los grupos consolidados

# Sin consolidación (solo árbol jerárquico de Ward)
hcpc_sin <- HCPC(res.pca, nb.clust = 6, consol = FALSE, graph = FALSE)

# Con consolidación (Ward + K-means)
hcpc_con <- HCPC(res.pca, nb.clust = 6, consol = TRUE, graph = FALSE)

# Comparar cuántos individuos cambiaron de grupo durante la consolidación
table(Sin_Consolidacion = hcpc_sin$data.clust$clust, 
      Con_Consolidacion = hcpc_con$data.clust$clust)

## Opción A: Personalización nativa en FactoMineR
library(FactoMineR)

# Dendrograma clásico con rectángulos y diagrama de barras de inercia
plot(res.hcpc, 
     choice = "tree",       # Grafica solo el dendrograma
     rect = TRUE,           # Dibuja rectángulos rodeando cada clúster
     tree.barplot = TRUE,   # Muestra el gráfico de ganancias de inercia
     title = "Dendrograma de HCPC con FactoMineR")

## Opción B: Personalización avanzada con factoextra (Recomendado)
library(factoextra)

fviz_dend(res.hcpc, 
          k = 3,                             # Número de clústeres
          cex = 0.7,                         # Tamaño de las etiquetas de texto
          k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), # Paleta de colores personalizada
          rect = TRUE,                       # Añadir rectángulos
          rect_fill = TRUE,                  # Rellenar rectángulos con transparencia
          rect_border = "jco",               # Estilo de borde
          horiz = TRUE,                      # Orientación horizontal
          ggtheme = theme_minimal(),         # Tema visual
          main = "Dendrograma Consolidado Horizontal")

# Dendrograma en formato circular
fviz_dend(res.hcpc, 
          type = "circular", 
          k = 3, 
          k_colors = "jco", 
          cex = 0.6,
          rect = TRUE)

# Dendrograma estilo árbol filogenético
fviz_dend(res.hcpc, 
          type = "phylogenic", 
          k = 3, 
          k_colors = "Dark2", 
          repel = TRUE)                     # Evita el solapamiento de textos

### Ejemplo de Uso Práctico: Script completo en R para extraer y exportar a Excel / CSV

# ==============================================================================
# 1. CARGA DE LIBRERÍAS Y EJECUCIÓN DE EJEMPLO
# ==============================================================================
library(FactoMineR)
library(openxlsx) # Para exportar múltiples pestañas a Excel (o usa writexl)

# Ejemplo con el dataset decathlon
data(decathlon)
res.pca  <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
res.hcpc <- HCPC(res.pca, nb.clust = 3, consol = TRUE, graph = FALSE)

# ==============================================================================
# 2. EXTRAER ASIGNACIÓN DE CLÚSTERES POR INDIVIDUO
# ==============================================================================
# Contiene el dataset original más la columna 'clust' con el grupo asignado
datos_con_cluster <- res.hcpc$data.clust

# Agregar el nombre/ID de los individuos como columna explícita
datos_con_cluster$Individuo <- rownames(datos_con_cluster)
# Reordenar para que 'Individuo' y 'clust' queden al principio
datos_con_cluster <- datos_con_cluster[, c("Individuo", "clust", setdiff(colnames(datos_con_cluster), c("Individuo", "clust")))]

# ==============================================================================
# 3. EXTRAER CARACTERIZACIÓN DE VARIABLES CUANTITATIVAS
# ==============================================================================
# Convertir la lista de matrices de desc.var$quanti en un único data.frame ordenado
caract_quanti_df <- do.call(rbind, lapply(names(res.hcpc$desc.var$quanti), function(cl) {
  mat <- res.hcpc$desc.var$quanti[[cl]]
  df <- as.data.frame(mat)
  df$Variable <- rownames(mat)
  df$Cluster  <- cl
  rownames(df) <- NULL
  return(df)
}))

# Reorganizar columnas
caract_quanti_df <- caract_quanti_df[, c("Cluster", "Variable", "v.test", "Mean in category", "Overall mean", "sd in category", "Overall sd", "p.value")]

# ==============================================================================
# 4. EXTRAER INDIVIDUOS REPRESENTATIVOS (PARAGONES Y DISTANTES)
# ==============================================================================
# A. Paragones (Casos más típicos / centroides reales de cada grupo)
paragones_df <- do.call(rbind, lapply(names(res.hcpc$desc.ind$para), function(cl) {
  vec <- res.hcpc$desc.ind$para[[cl]]
  data.frame(
    Cluster = cl,
    Individuo = names(vec),
    Distancia_al_Centro = as.numeric(vec)
  )
}))

# B. Casos más distantes (Casos límite o extremos de cada grupo)
distantes_df <- do.call(rbind, lapply(names(res.hcpc$desc.ind$dist), function(cl) {
  vec <- res.hcpc$desc.ind$dist[[cl]]
  data.frame(
    Cluster = cl,
    Individuo = names(vec),
    Distancia_al_Centro = as.numeric(vec)
  )
}))

# ==============================================================================
# 5. EXPORTAR A EXCEL (MÚLTIPLES PESTAÑAS)
# ==============================================================================
lista_pestanias <- list(
  "Asignacion_Clusters"       = datos_con_cluster,
  "Caracterizacion_Variables" = caract_quanti_df,
  "Individuos_Paragones"      = paragones_df,
  "Individuos_Distantes"      = distantes_df
)

# Exportar todo a un solo archivo Excel (.xlsx)
write.xlsx(lista_pestanias, file = "Resultados_Clustering_HCPC.xlsx", rowNames = FALSE)

cat("¡Exportación a Excel completada con éxito como 'Resultados_Clustering_HCPC.xlsx'!\n")

# ==============================================================================
# 6. ALTERNATIVA: EXPORTAR A ARCHIVOS CSV
# ==============================================================================
# Si prefieres archivos CSV separados:
write.csv(datos_con_cluster, "asignacion_clusters.csv", row.names = FALSE)
write.csv(caract_quanti_df,  "caracterizacion_variables.csv", row.names = FALSE)
write.csv(paragones_df,       "individuos_paragones.csv", row.names = FALSE)
write.csv(distantes_df,       "individuos_distantes.csv", row.names = FALSE)

# ==============================================================================
# 7. Obtener resumen ejecutivo de 1 fila por clúster (quant)
# ==============================================================================
top_variables_resumen <- obtener_top_vtest(res.hcpc, top_n = 3, formato = "resumen")

print(top_variables_resumen)

### Ejemplo de Uso Práctico (quant & qual)
library(FactoMineR)

# 1. Cargar datos y ejecutar PCA con la variable cualitativa 'Competition' en la col 13
data(decathlon)
res.pca  <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
res.hcpc <- HCPC(res.pca, nb.clust = 3, consol = TRUE, graph = FALSE)

# 2. Obtener el Top 4 de variables (Cuantitativas + Cualitativas)
top_mixto_detallado <- obtener_top_vtest(res.hcpc, top_n = 4, formato = "detallado")
print(top_mixto_detallado)

# 3. Obtener resumen ejecutivo de 1 fila por clúster
top_mixto_resumen <- obtener_top_vtest(res.hcpc, top_n = 3, formato = "resumen")
print(top_mixto_resumen)

### Método dela silueta
# 1.1 Criterio del Ancho de Silueta Promedio (Silhouette Method)
# Evalúa qué tan bien encaja cada especie dentro de su grupo frente a los grupos vecinos
coords <- res.pca$ind$coord

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

### Escoger entre Silouette y KL Ejemplo 1
library(factoextra)
library(cluster)

# Extraer coordenadas del PCA y siluetas para k=5 y k=8
coords <- res.pca$ind$coord

# Silueta para 5 grupos
hc_5 <- HCPC(res.pca, nb.clust = 5, graph = FALSE)
sil_5 <- silhouette(as.numeric(hc_5$data.clust$clust), dist(coords))

# Silueta para 8 grupos
hc_8 <- HCPC(res.pca, nb.clust = 8, graph = FALSE)
sil_8 <- silhouette(as.numeric(hc_8$data.clust$clust), dist(coords))

# Visualizar perfiles
fviz_silhouette(sil_5) + ggtitle("Perfil Silueta para k = 5")
fviz_silhouette(sil_8) + ggtitle("Perfil Silueta para k = 8")

### Evaluación automática del k óptimo de clusters (cualitativo)
library(FactoMineR)

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)

# Ejecutar script con k_macro = 3 y umbral de silueta = 0.35
res_sub <- subclustering_automatico(res.pca, k_macro = -1, umbral_silueta = 0.35)

# 1. Ver la tabla diagnóstica corregida
print(res_sub$diagnostico)

# 2. Ver la distribución de las nuevas etiquetas jerárquicas
table(res_sub$data$cluster_jerarquico)

### visualizar el plano PCA con las etiquetas jerárquicas finales (1.1, 1.2, 2, etc.)
## Opción 1. Con factoextra (Rápido y Funcional)
library(factoextra)

# 1. Asegurar que las etiquetas jerárquicas estén como factor
grupos_jerarquicos <- as.factor(res_sub$data$cluster_jerarquico)

# 2. Graficar en el plano factorial (Dim 1 vs Dim 2)
fviz_pca_ind(res.pca,
             habillage = grupos_jerarquicos,   # Asigna los colores por sub-clúster
             addEllipses = TRUE,              # Dibuja elipses de confianza
             ellipse.level = 0.95,            # Nivel de confianza de la elipse (95%)
             ellipse.type = "convex",         # "convex" para polígonos o "confidence" para elipses suave
             palette = "Set1",                # Paleta de colores RColorBrewer
             repel = TRUE,                    # Evita que el texto de las muestras se solape
             legend.title = "Clústeres",
             title = "Plano PCA: Clasificación Jerárquica de 2 Niveles") +
  theme_minimal()

## Opción 2. Con ggplot2 (Personalización Avanzada para Publicación)
library(ggplot2)

# 1. Preparar el DataFrame con coordenadas y etiquetas jerárquicas
df_plot <- as.data.frame(res.pca$ind$coord[, 1:2])
colnames(df_plot) <- c("Dim1", "Dim2")

df_plot$Individuo          <- rownames(df_plot)
df_plot$cluster_jerarquico <- as.factor(res_sub$data$cluster_jerarquico)

# 2. Extraer el porcentaje de varianza explicada para las etiquetas de los ejes
var_exp <- round(res.pca$eig[1:2, 2], 1)

# 3. Calcular los centroides geométricos de cada sub-clúster para ubicar la etiqueta principal
centroides <- aggregate(cbind(Dim1, Dim2) ~ cluster_jerarquico, data = df_plot, FUN = mean)

# 4. Construir el gráfico
ggplot(df_plot, aes(x = Dim1, y = Dim2, color = cluster_jerarquico, fill = cluster_jerarquico)) +
  # Ejes de referencia en el origen (0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Polígonos / Elipses alrededor de los sub-clústeres
  stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.95, linetype = "solid") +
  
  # Puntos de las observaciones
  geom_point(size = 3, alpha = 0.85) +
  
  # Nombres de los individuos (muestras)
  geom_text(aes(label = Individuo), vjust = -0.7, size = 2.8, show.legend = FALSE) +
  
  # Etiquetas de los centroides de cada sub-grupo
  geom_label(data = centroides, 
             aes(x = Dim1, y = Dim2, label = cluster_jerarquico), 
             color = "black", fontface = "bold", size = 3.5, 
             alpha = 0.8, show.legend = FALSE) +
  
  # Personalización de títulos y ejes
  labs(
    title = "Estructura de Clústeres Jerárquicos en el Espacio PCA",
    subtitle = "Sub-clústeres generados automáticamente por heterogeneidad de silueta",
    x = paste0("Dimensión 1 (", var_exp[1], "%)"),
    y = paste0("Dimensión 2 (", var_exp[2], "%)"),
    color = "Sub-clúster",
    fill  = "Sub-clúster"
  ) +
  
  # Tema visual limpio
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10)
  )

## Exportar a PDF Vectorial (Recomendado)
library(ggplot2)

# 1. Asignar el gráfico a una variable (o usar el último generado)
mi_grafico <- ggplot(df_plot, aes(x = Dim1, y = Dim2, color = cluster_jerarquico)) +
  geom_point(size = 3) +
  theme_bw()

# 2. Guardar en PDF vectorial
ggsave(
  filename = "grafico_clusters_vectorial.pdf",
  plot = mi_grafico,
  width = 18,           # Ancho (18 cm equivale aproximadamente a 2 columnas de revista)
  height = 14,          # Alto
  units = "cm",         # Unidades: "cm", "in" (pulgadas), "mm"
  device = cairo_pdf    # Dispositivo Cairo: asegura renderizado perfecto de fuentes y símbolos
)

## Exportar a TIFF en Alta Resolución (300 DPI)
# Guardar en TIFF a 300 DPI para publicación
ggsave(
  filename = "grafico_clusters_300dpi.tiff",
  plot = mi_grafico,
  width = 18,
  height = 14,
  units = "cm",
  dpi = 300,             # Resolución de impresión científica
  compression = "lzw"   # Compresión sin pérdida de calidad (reduce drásticamente el tamaño en MB)
)

## Paneles compartidos para publicación
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. PREPARAR ANÁLISIS DE EJEMPLO (PCA + HCPC)
# ==============================================================================
data(decathlon)
res.pca  <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
res.hcpc <- HCPC(res.pca, nb.clust = 3, consol = TRUE, graph = FALSE)

# ==============================================================================
# 2. CREAR PANEL A: DENDROGRAMA
# ==============================================================================
panel_a <- fviz_dend(
  res.hcpc, 
  rect = TRUE, 
  rect_fill = TRUE,
  k_colors = "jco",
  cex = 0.6,
  ggtheme = theme_minimal(),
  main = "Dendrograma de Jerarquización"
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9)
  )

# ==============================================================================
# 3. CREAR PANEL B: MAPA FACTORIAL DE CLÚSTERES
# ==============================================================================
panel_b <- fviz_cluster(
  res.hcpc, 
  geom = c("point", "text"), 
  ellipse.type = "convex",
  palette = "jco",
  repel = TRUE,
  ggtheme = theme_minimal(),
  main = "Agrupación en el Plano PCA (Dim 1 vs Dim 2)"
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

# ==============================================================================
# 4. COMBINAR PANELES Y AGREGAR ETIQUETAS A Y B
# ==============================================================================
# El operador '+' coloca los paneles lado a lado (horizontal)
# tag_levels = 'A' asigna automáticamente 'A' al primer panel y 'B' al segundo
figura_compuesta <- panel_a + panel_b + 
  plot_annotation(
    title = "Análisis de Clústeres sobre Componentes Principales (HCPC)",
    subtitle = "Integración de la estructura jerárquica y la proyeccion espacial",
    tag_levels = 'A'
  ) & 
  # El operador '&' aplica este tema a TODOS los paneles individualmente
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 13)
  )

# Mostrar figura en la consola de R
print(figura_compuesta)

# ==============================================================================
# 5. EXPORTAR CON ggsave()
# ==============================================================================

# A. Exportación a PDF vectorial (ideal para revistas)
ggsave(
  filename = "Figura_1_HCPC_Compuesta.pdf",
  plot = figura_compuesta,
  width = 24,            # Ancho amplio para dos paneles horizontales (24 cm)
  height = 12,           # Alto (12 cm)
  units = "cm",
  device = cairo_pdf
)

# B. Exportación a TIFF a 300 DPI (formato ráster de alta resolución)
ggsave(
  filename = "Figura_1_HCPC_Compuesta.tiff",
  plot = figura_compuesta,
  width = 24,
  height = 12,
  units = "cm",
  dpi = 300,
  compression = "lzw"
)

## Disposición vertical de paneles con leyenda compartida
library(patchwork)
library(ggplot2)

# ==============================================================================
# 1. COMBINAR PANELES EN VERTICAL CON LEYENDA ÚNICA
# ==============================================================================
# El operador '/' apila los gráficos (A arriba, B abajo)
# 'guides = "collect"' unifica las leyendas idénticas en una sola
figura_vertical <- panel_a / panel_b + 
  plot_layout(
    guides = "collect",          # Consolida leyendas duplicadas
    heights = c(1, 1.2)          # Opcional: ajusta la proporción de altura (ej. B un poco más alto)
  ) + 
  plot_annotation(
    title = "Análisis de Clústeres sobre Componentes Principales (HCPC)",
    subtitle = "Estructura jerárquica (A) y proyección factorial en componentes (B)",
    tag_levels = 'A'
  ) & 
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"    # Posición de la leyenda compartida (bottom, right, etc.)
  )

# Mostrar figura en consola
print(figura_vertical)

# ==============================================================================
# 2. EXPORTAR CON DIMENSIONES ADAPTADAS A FORMATO VERTICAL
# ==============================================================================
# Para disposiciones verticales, la altura debe ser mayor que el ancho

# A. En formato PDF vectorial
ggsave(
  filename = "Figura_1_Vertical_Unificada.pdf",
  plot = figura_vertical,
  width = 16,            # Ancho ajustado a columna y media de revista (16 cm)
  height = 24,           # Mayor altura para alojar ambos paneles (24 cm)
  units = "cm",
  device = cairo_pdf
)

# B. En formato TIFF a 300 DPI
ggsave(
  filename = "Figura_1_Vertical_Unificada.tiff",
  plot = figura_vertical,
  width = 16,
  height = 24,
  units = "cm",
  dpi = 300,
  compression = "lzw"
)

### Evaluación automática del k óptimo de clusters (multimodelo)
library(FactoMineR)

# 1. Cargar datos de ejemplo y realizar MCA
data(tea)
# Usar solo las primeras 18 variables cualitativas (preguntas sobre hábitos de consumo de té)
res_mca <- MCA(tea[, 1:18], graph = FALSE)

# 2. Ejecutar sub-clustering automático sobre el MCA
# Usamos n_min = 30 aprovechando el tamaño de muestra más grande (N = 300)
res_sub_mca <- subclustering_automatico(
  res_mca, 
  k_macro = 6, 
  umbral_silueta = 0.25, 
  n_min = 30, 
  mejora_min_silueta = 0.05
)

# 3. Ver diagnóstico y distribución final
print(res_sub_mca$diagnostico)
table(res_sub_mca$data$cluster_jerarquico)

# ==============================================================================
# 1. PREPARAR ANÁLISIS DE EJEMPLO (MCA + HCPC) TreeFernBog_CharMatrix original
# ==============================================================================
# 1.1 Asignar la primera columna como rownames
rownames(TreeFernBog_CharMatrix) <- TreeFernBog_CharMatrix[, 1]

# 1.2 Eliminar la columna de nombres para dejar solo la matriz de caracteres morfológicos
TreeFernBog_CharMatrix <- TreeFernBog_CharMatrix[, -1]

# 1.3 Ejecutar MCA + HCPC
res.mca  <- MCA(TreeFernBog_CharMatrix, graph = TRUE)
res_mca  <- MCA(TreeFernBog_CharMatrix, graph = FALSE)
res.hcpc <- HCPC(res.mca, nb.clust = -1, consol = TRUE, graph = TRUE)

# ==============================================================================
# 2. CREAR PANEL A: DENDROGRAMA
# ==============================================================================
panel_a <- fviz_dend(
  res.hcpc, 
  rect = TRUE, 
  rect_fill = TRUE,
  k_colors = "jco",
  cex = 0.6,
  ggtheme = theme_minimal(),
#  main = "Dendrograma de Jerarquización"
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9)
  )

# ==============================================================================
# 3. CREAR PANEL B: MAPA FACTORIAL DE CLÚSTERES
# ==============================================================================
panel_b <- fviz_cluster(
  res.hcpc, 
  geom = c("point"), # geom = c("point", "text"),
  ellipse.type = "convex",
  palette = "jco",
  repel = TRUE,
  ggtheme = theme_minimal(),
#  main = "Agrupación en el Plano PCA (Dim 1 vs Dim 2)"
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

# ==============================================================================
# 4. COMBINAR PANELES Y AGREGAR ETIQUETAS A Y B
# ==============================================================================
# El operador '+' coloca los paneles lado a lado (horizontal)
# tag_levels = 'A' asigna automáticamente 'A' al primer panel y 'B' al segundo
figura_compuesta <- panel_a + panel_b + 
  plot_annotation(
    title = "Análisis de Clústeres sobre Componentes Principales (HCPC)",
    subtitle = "Integración de la estructura jerárquica y la proyeccion espacial",
    tag_levels = 'A'
  ) & 
  # El operador '&' aplica este tema a TODOS los paneles individualmente
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 13)
  )

# Mostrar figura en la consola de R
print(figura_compuesta)
print(panel_a)
print(panel_b)

plot_var_contrib <- fviz_mca_var(
  res.mca, 
  col.var = "contrib", 
  gradient.cols = c("#E0E0E0", "#3a86d4", "#08306b"), # De gris neutro/suave a azul intenso
  alpha.var = "contrib",                              # Transparencia proporcional a la contribución
  repel = TRUE, 
  ggtheme = theme_minimal(),
  title = "Categorías Morfológicas - Gradiente por Contribución",
  subtitle = "Las variables oscuras y opacas son las que definen el espacio multivariado"
)

print(plot_var_contrib)

# B. En formato TIFF a 300 DPI
ggsave(
  filename = "Figura_1_Vertical_Unificada.tiff",
  plot = figura_vertical,
  width = 16,
  height = 24,
  units = "cm",
  dpi = 300,
  compression = "lzw"
)

top_mixto_detallado <- obtener_top_vtest(res.hcpc, top_n = 10, formato = "detallado")
print(top_mixto_detallado)

res.hcpc$desc.axes
res.hcpc$desc.var
res.hcpc$desc.var$category
datos_consolidados <- res.hcpc$data.clust

especies_cluster <- data.frame(
  Nombre = rownames(datos_consolidados),
  Cluster = datos_consolidados$clust
)

print(especies_cluster)

# ==============================================================================
# 1. PREPARAR ANÁLISIS DE EJEMPLO (MCA + HCPC) TreeFernBog_CharMatrix codificado
# ==============================================================================
# 1.1 Asignar la primera columna como rownames
rownames(TreeFernBog_CharMatrix) <- TreeFernBog_CharMatrix[, 1]

# 1.2 Eliminar la columna de nombres para dejar solo la matriz de caracteres morfológicos
TreeFernBog_CharMatrix <- TreeFernBog_CharMatrix[, -1]

# 1.3 Ejecutar MCA + HCPC
res.mca  <- MCA(datos_codificados, graph = FALSE)
res.hcpc <- HCPC(res.mca, nb.clust = 6, consol = TRUE, graph = TRUE)

# ==============================================================================
# 2. CREAR PANEL A: DENDROGRAMA
# ==============================================================================
panel_a <- fviz_dend(
  res.hcpc, 
  rect = TRUE, 
  rect_fill = TRUE,
  k_colors = "jco",
  cex = 0.6,
  ggtheme = theme_minimal(),
#  main = "Dendrograma de Jerarquización"
  main = FALSE
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9)
  )

# ==============================================================================
# 3. CREAR PANEL B: MAPA FACTORIAL DE CLÚSTERES
# ==============================================================================
panel_b <- fviz_cluster(
  res.hcpc, 
  geom = c("point", "text"), 
  ellipse.type = "convex",
  palette = "jco",
  repel = TRUE,
  ggtheme = theme_minimal(),
#  main = "Agrupación en el Plano PCA (Dim 1 vs Dim 2)"
  main = FALSE
) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

# ==============================================================================
# 4. COMBINAR PANELES Y AGREGAR ETIQUETAS A Y B
# ==============================================================================
# El operador '+' coloca los paneles lado a lado (horizontal)
# tag_levels = 'A' asigna automáticamente 'A' al primer panel y 'B' al segundo
figura_compuesta <- panel_a + panel_b + 
  plot_annotation(
    title = "Análisis de Clústeres sobre Componentes Principales (HCPC)",
    subtitle = "Integración de la estructura jerárquica y la proyeccion espacial",
    tag_levels = 'A'
  ) & 
  # El operador '&' aplica este tema a TODOS los paneles individualmente
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 13)
  )

# Mostrar figura en la consola de R
print(figura_compuesta)
print(panel_a)
print(panel_b)