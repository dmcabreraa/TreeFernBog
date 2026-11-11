# Instalar paquetes si no los tienes:
install.packages(c("cluster", "FactoMineR", "factoextra", "tidyverse"))

# Cargar librerías necesarias
library(tidyverse)
library(cluster)     # Contiene la función daisy() para Gower
library(FactoMineR)  # Para ejecutar el MCA
library(factoextra)  # Para extraer y visualizar resultados de FactoMineR

# --- PASO 1: Carga de Datos ---
# Asigna tu matriz a este objeto (asegúrate de que Especie sea una columna, no rownames)
# datos_morfologicos <- read_csv("TreeFernBog_CharMatrix.csv")

# (Simulación mínima para que el código corra si lo copias tal cual)
#datos_morfologicos <- tibble(
#  Especie = c("Cyathea caracasana", "Alsophila engelii", "Sphaeropteris senilis", 
#              "Dicksonia sellowiana", "Lophosoria quadripinnata"),
#  Tipo_Rizoma = as.factor(c("Erecto", "Erecto", "Erecto", "Arborescente", "Rastrero")),
#  Escamas_Base = as.factor(c("Bicoloras", "Concoloras", "Setíferas", "Pelos_no_escamas", "Pelos"))
#)

datos_morfologicos <- TreeFernBog_CharMatrix

# --- PASO 2: Creación de Diccionarios y Codificación ---

# A. Diccionario de Especies (E001, E002...)
dict_especies <- datos_morfologicos %>%
  select(Especie) %>%
  mutate(Codigo_Especie = paste0("E", sprintf("%03d", row_number())))

# B. Diccionario de Categorías Morfológicas (C001, C002...)
# Extraemos todas las categorías únicas de todas las columnas (excepto Especie)
dict_categorias <- datos_morfologicos %>%
  select(-Especie) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Caracter", values_to = "Estado") %>%
  distinct(Caracter, Estado) %>%
  arrange(Caracter, Estado) %>%
  mutate(Codigo_Categoria = paste0("C", sprintf("%03d", row_number())))

# C. Aplicar los códigos a la matriz original
datos_codificados <- datos_morfologicos %>%
  # Reemplazar nombres de especies
  left_join(dict_especies, by = "Especie") %>%
  select(Codigo_Especie, everything(), -Especie) %>%
  column_to_rownames(var = "Codigo_Especie")

# Función para reemplazar los valores de las columnas por sus códigos
for (col in colnames(datos_codificados)) {
  datos_codificados[[col]] <- unlist(lapply(datos_codificados[[col]], function(x) {
    if(is.na(x)) return(NA)
    codigo <- dict_categorias$Codigo_Categoria[dict_categorias$Caracter == col & dict_categorias$Estado == x]
    return(codigo)
  }))
}

# Convertir todo a factor nuevamente para el MCA
datos_codificados <- datos_codificados %>% mutate(across(everything(), as.factor))

# --- PASO 3: Clustering Jerárquico para crear los grupos ---
# daisy() calcula la matriz de disimilitud para datos categóricos
matriz_distancia <- daisy(datos_codificados, metric = "gower")

# Crear el dendrograma usando el método de Ward (minimiza la varianza dentro de los grupos)
arbol_jerarquico <- hclust(matriz_distancia, method = "ward.D2")

# Extraer a qué grupo pertenece cada especie para cruzarlo luego con las altitudes y asignar nombres descriptivos
grupos_cortados <- cutree(arbol_jerarquico, k = 3)
datos_con_grupos <- datos_morfologicos %>% mutate(Grupo_Morfologico = as.factor(grupos_cortados))

grupos_cortados <- cutree(arbol_jerarquico, k = 3)

datos_con_grupos <- datos_morfologicos %>% 
  mutate(Grupo_Morfologico = factor(
    grupos_cortados,
    levels = c(1, 2, 3),
    # Renombramos para que el gráfico y las tablas sean autosuficientes
    labels = c("1", #"G1: Palustres y Dimórficas"
               "2", #"G2: Arborescentes (Cyathea)"
               "3") #"G3: Herbáceas (Amauropelta)"
  ))

# --- PASO 3.2: Definir vector de colores NOMBRADO ---
# Asociamos directamente cada etiqueta al color correspondiente en tu dendrograma
#mis_colores <- c(
#  "G1: Palustres y Dimórficas"    = "#E7B800", # Amarillo
#  "G2: Arborescentes (Cyathea)"  = "#0073C2", # Azul
#  "G3: Herbáceas (Amauropelta)"  = "#00AFBB"  # Turquesa
#)

mis_colores <- c(
  "1"    = "#E7B800", # Amarillo
  "2"  = "#0073C2", # Azul
  "3"  = "#00AFBB"  # Turquesa
)

# Visualización del Dendrograma
plot_dendrograma <- fviz_dend(
  arbol_jerarquico, 
  k = 3,                 # Cortar el árbol en 3 grupos morfológicos (ajusta este valor)
  cex = 0.7,             # Tamaño del texto
  k_colors = mis_colores,      # <- Asigna los colores a las ramas/etiquetas
  rect = TRUE,           # Dibujar rectángulos alrededor de los grupos
  rect_fill = TRUE,
  rect_border = mis_colores,   # <- Asigna los mismos colores al borde de las cajas
  main = "Dendrograma de Agrupamiento Morfológico Funcional",
  ggtheme = theme_minimal()
)

print(plot_dendrograma)

# --- CÓDIGO COMPLEMENTARIO: Dendrograma con Nombres Científicos de Especies ---

# 1. Crear una copia del árbol jerárquico para no modificar el original
arbol_nombres <- arbol_jerarquico

# 2. Reemplazar los códigos E00X por los nombres reales usando el diccionario
arbol_nombres$labels <- dict_especies$Especie[match(arbol_jerarquico$labels, dict_especies$Codigo_Especie)]

# 3. Graficar el dendrograma con los nombres de las especies
plot_dendrograma_nombres <- fviz_dend(
  arbol_nombres, 
  k = 3,                 # Número de grupos morfológicos (mismo k que usaste antes)
  cex = 0.7,             # Tamaño de letra (ajustado para nombres más largos)
  k_colors = mis_colores,      # <- Asigna los colores a las ramas/etiquetas
  color_labels_by_k = TRUE, 
  rect = TRUE,           # Dibuja los recuadros de cada grupo
  rect_border = mis_colores,   # <- Asigna los mismos colores al borde de las cajas
  horiz = TRUE,          # Orientación horizontal: ideal para leer nombres científicos largos
  main = "Dendrograma Morfológico Funcional (Nombres de Especies)"
)

# Desplegar el nuevo dendrograma
print(plot_dendrograma_nombres)

# --- PASO 4: MCA y Gráficos Separados ---
res_mca <- MCA(datos_codificados, graph = TRUE)

# GRÁFICO 1: Solo Variables (Categorías codificadas)
plot_variables <- fviz_mca_var(
  res_mca, 
  choice = "var.cat", 
  repel = TRUE, 
  col.var = "contrib", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  title = "Disposición de Categorías Morfológicas (Códigos C)",
  subtitle = "Usa 'dict_categorias' para interpretar cada código C",
  labelsize = 4
)

# GRÁFICO 2: Solo Individuos (Especies codificadas agrupadas por clúster)
plot_individuos <- fviz_mca_ind(
  res_mca, 
  repel = TRUE,
  habillage = datos_con_grupos$Grupo_Morfologico, # Colorea por el grupo del clúster
  palette = mis_colores,                          # <- Aplica el mismo vector de colores
  addEllipses = TRUE, 
  ellipse.level = 0.95,
  title = "Agrupamiento de Especies en el Espacio Funcional (Códigos E)",
  subtitle = "Usa 'dict_especies' para interpretar cada código E",
  labelsize = 4
)

# Desplegar gráficos
print(plot_variables)
print(plot_individuos)

# Difuminado de variables

# --- ENFOQUE 1: Atenuación por Calidad de Representación (cos2) ---
# Difumina lo que está mal representado en las dimensiones 1 y 2
plot_var_cos2 <- fviz_mca_var(
  res_mca, 
  col.var = "cos2", 
  gradient.cols = c("#dbe9f6", "#3a86d4", "#08306b"), # De azul muy claro a azul oscuro
  alpha.var = "cos2",                                  # Agrega transparencia según cos2
  repel = TRUE, 
  ggtheme = theme_minimal(),
  title = "Categorías Morfológicas - Gradiente por cos2",
  subtitle = "Las variables transparentes/claras tienen baja representación en 2D"
)

# --- ENFOQUE 2: Atenuación por Contribución (contrib) ---
# Difumina lo que aporta poco a la construcción de los ejes
plot_var_contrib <- fviz_mca_var(
  res_mca, 
  col.var = "contrib", 
  gradient.cols = c("#E0E0E0", "#3a86d4", "#08306b"), # De gris neutro/suave a azul intenso
  alpha.var = "contrib",                              # Transparencia proporcional a la contribución
  repel = TRUE, 
  ggtheme = theme_minimal(),
  title = "Categorías Morfológicas - Gradiente por Contribución",
  subtitle = "Las variables oscuras y opacas son las que definen el espacio multivariado"
)

# --- ENFOQUE 3: Filtro Directo (Ocultar el ruido de fondo) ---
# Si el gradiente no basta, este comando OCURRE o filtra las categorías irrelevantes
plot_var_filtrado <- fviz_mca_var(
  res_mca, 
  col.var = "contrib", 
  gradient.cols = c("#dbe9f6", "#3a86d4", "#08306b"),
  select.var = list(contrib = 25),                    # Muestra ÚNICAMENTE el Top 25 de categorías
  repel = TRUE, 
  ggtheme = theme_minimal(),
  title = "Top 25 Categorías Morfológicas de Mayor Contribución"
)

# Desplegar para comparar visualmente cuál funciona mejor para tu manuscrito
print(plot_var_cos2)
print(plot_var_contrib)
print(plot_var_filtrado)

# --- PASO 5: Exportar diccionarios para tu referencia ---
# Descomenta estas líneas para guardar los diccionarios como CSV y saber qué significa cada código
 write_excel_csv(dict_especies, "Diccionario_Especies.csv")
 write_excel_csv(dict_categorias, "Diccionario_Categorias.csv")
 
 # Asegúrate de haber ejecutado el código anterior para tener el objeto 'res_mca'
 
 # --- 1. EXTRAER MATRICES ---
 # A. Para las variables (Categorías morfológicas)
 coord_var <- res_mca$var$coord       # Coordenadas en cada dimensión
 contrib_var <- res_mca$var$contrib   # Contribución (%) a la varianza de cada dimensión
 
 # B. Para los individuos (Especies)
 coord_ind <- res_mca$ind$coord
 contrib_ind <- res_mca$ind$contrib
 
 # C. Valores propios (Eigenvalues - Varianza explicada por cada dimensión)
 eigenvalues <- res_mca$eig
 
 
 # --- 2. EXPORTAR A CSV ---
 # Convertimos a data.frame y pasamos los nombres de fila a una columna real para no perderlos en el CSV
 write_excel_csv(as.data.frame(contrib_var) %>% rownames_to_column("Codigo_Categoria"), 
                 "MCA_Contribuciones_Variables.csv")
 
 write_excel_csv(as.data.frame(coord_var) %>% rownames_to_column("Codigo_Categoria"), 
                 "MCA_Coordenadas_Variables.csv")
 
 write_excel_csv(as.data.frame(contrib_ind) %>% rownames_to_column("Codigo_Especie"), 
                 "MCA_Contribuciones_Especies.csv")
 
 write_excel_csv(as.data.frame(eigenvalues) %>% rownames_to_column("Dimension"), 
                 "MCA_Varianza_Explicada.csv")
 
 # --- 3. GRAFICAR CONTRIBUCIONES (PESOS) ---
 
 # A. Top 15 categorías morfológicas que más aportan a la Dimensión 1
 plot_contrib_var_dim1 <- fviz_contrib(
   res_mca, 
   choice = "var", 
   axes = 1, 
   top = 15,
   title = "Contribución de Variables a la Dimensión 1",
   fill = "steelblue", 
   color = "black"
 )
 
 # B. Top 15 categorías morfológicas que más aportan a la Dimensión 2
 plot_contrib_var_dim2 <- fviz_contrib(
   res_mca, 
   choice = "var", 
   axes = 2, 
   top = 15,
   title = "Contribución de Variables a la Dimensión 2",
   fill = "darkgreen", 
   color = "black"
 )
 
 # Mostrar gráficos
 print(plot_contrib_var_dim1)
 print(plot_contrib_var_dim2)
 
 # Nota: Puedes hacer lo mismo para las especies cambiando choice = "var" por choice = "ind"
 
 # A. Top 15 categorías morfológicas que más aportan a la Dimensión 1
 plot_contrib_ind_dim1 <- fviz_contrib(
   res_mca, 
   choice = "ind", 
   axes = 1, 
   top = 15,
   title = "Contribución de Especies a la Dimensión 1",
   fill = "steelblue", 
   color = "black"
 )
 
 # B. Top 15 categorías morfológicas que más aportan a la Dimensión 2
 plot_contrib_ind_dim2 <- fviz_contrib(
   res_mca, 
   choice = "ind", 
   axes = 2, 
   top = 15,
   title = "Contribución de Especies a la Dimensión 2",
   fill = "darkgreen", 
   color = "black"
 )
 
 # Mostrar gráficos
 print(plot_contrib_ind_dim1)
 print(plot_contrib_ind_dim2)
 
 ## Descripción categórica
 
 # --- PASO 1: Identificación Estadística de Caracteres por Grupo ---
 # La función catdes() evalúa qué variables definen a cada grupo de datos_con_grupos
 indice_grupo <- which(colnames(datos_con_grupos) == "Grupo_Morfologico")
 res_catdes <- catdes(datos_con_grupos, num.var = indice_grupo, proba = 0.05)
 
 
 # --- PASO 2: Procesar los Resultados y Cruzar con el Diccionario ---
 
 lista_tabla_grupos <- list()
 
 # Recorremos cada grupo identificado en catdes
 for (nombre_grupo in names(res_catdes$category)) {
   
   matriz_grupo <- res_catdes$category[[nombre_grupo]]
   
   if (!is.null(matriz_grupo) && nrow(matriz_grupo) > 0) {
     df_temp <- as.data.frame(matriz_grupo) %>%
       rownames_to_column(var = "Codigo_Categoria") %>%
       mutate(Grupo_Morfologico = paste("Grupo", nombre_grupo))
     
     lista_tabla_grupos[[nombre_grupo]] <- df_temp
   }
 }
 
 # Consolidación de la tabla final
 tabla_caracteristicas_grupos <- bind_rows(lista_tabla_grupos) %>%
   # Unir con tu diccionario de categorías para recuperar Caracter y Estado
   left_join(dict_categorias, by = "Codigo_Categoria") %>%
   # Seleccionar, renombrar y ordenar las métricas clave
   select(
     Grupo_Morfologico,
     Codigo_Categoria,
     Caracter,
     Estado,
     Pct_en_Grupo = `Mod/Cla`,     # % de especies del grupo que presentan este estado
     Pct_en_Total = `Cla/Mod`,     # % del total general que cayeron en este grupo
     Pct_Global = `Global`,        # % de presencia de la categoría en toda la matriz
     Valor_p = `p.value`,          # Significancia estadística
     Test_V = `v.test`             # Fuerza de la asociación (V-test > 1.96 indica sobrerepresentación)
   ) %>%
   # Filtrar por significancia y ordenar por la fuerza de asociación (Test_V más alto primero)
   filter(Valor_p < 0.05) %>%
   arrange(Grupo_Morfologico, desc(Test_V))
 
 
 # --- PASO 3: Exportar Resultados ---
 
 # Exportar tabla completa
 write_excel_csv(tabla_caracteristicas_grupos, "Caracteristicas_Diagnosticas_por_Grupo.csv")
 
 # Imprimir en consola una vista previa de las variables principales por grupo
 print(head(tabla_caracteristicas_grupos, 15))
 
 ### Función para calcular la inercia ajustada de Benzécri sobre un objeto FactoMineR MCA
 mca_bench <- function(res_mca) {
   Q <- ncol(res_mca$call$X) # Número de variables activas
   eig <- res_mca$eig[, 1]   # Autovalores
   
   # Filtrar autovalores superiores al promedio (1/Q)
   k_mca <- eig[eig > (1 / Q)]
   
   # Aplicar fórmula de Benzécri
   eig_adj <- ((Q / (Q - 1))^2) * ((k_mca - (1 / Q))^2)
   perc_adj <- (eig_adj / sum(eig_adj)) * 100
   
   data.frame(
     Autovalor_Bruto = k_mca,
     Pct_Bruto = res_mca$eig[1:length(k_mca), 2],
     Pct_Ajustado_Benzecri = perc_adj
   )
 }
 
 # Ejecutar sobre tu resultado
 mca_bench(res_mca)
 MCA_VarExpl_CorrecBench <- mca_bench(res_mca)
 write_excel_csv(MCA_VarExpl_CorrecBench, "MCA_VarExpl_CorrecBench.csv")