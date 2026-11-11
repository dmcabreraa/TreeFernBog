#' Sub-clustering automático jerárquico basado en el ancho medio de silueta (requiere "FactoMineR" y "cluster")
#'
#' @param res_pca Objeto de clase PCA generado por FactoMineR.
#' @param k_macro Número de clústeres para la clasificación principal (default: 5).
#' @param umbral_silueta Valor mínimo de silueta media por clúster (default: 0.35).
#'                       Clústeres por debajo de este valor se dividen.
#'
#' @return Una lista con el dataset etiquetado y la tabla diagnóstica de siluetas.

subclustering_automatico <- function(res_pca, 
                                     k_macro = 3, 
                                     umbral_silueta = 0.35, 
                                     n_min = 15, 
                                     mejora_min_silueta = 0.08,
                                     max_k_sub = 2) {
  
  # 1. HCPC Macro (Nivel 1)
  res_hcpc_macro <- HCPC(res_pca, nb.clust = k_macro, consol = TRUE, graph = FALSE)
  df_base        <- res_hcpc_macro$data.clust
  
  # Filtrar variables estrictamente numéricas/cuantitativas
  cols_evaluadas <- setdiff(colnames(df_base), "clust")
  vars_quanti    <- names(which(sapply(df_base[, cols_evaluadas], is.numeric)))
  
  # 2. Cálculo de Siluetas Globales (Nivel 1)
  coords      <- res_pca$ind$coord
  dist_mat    <- dist(coords)
  dist_matrix <- as.matrix(dist_mat)
  clust_macro <- as.numeric(df_base$clust)
  sil_obj     <- silhouette(clust_macro, dist_mat)
  sil_summary <- summary(sil_obj)
  
  sil_promedios        <- sil_summary$clus.avg.widths
  names(sil_promedios) <- levels(df_base$clust)
  
  etiquetas_finales <- character(nrow(df_base))
  
  diagnostico <- data.frame(
    Cluster_Macro    = names(sil_promedios),
    N_Individuos     = as.numeric(table(df_base$clust)),
    Silueta_Original = round(sil_promedios, 3),
    Silueta_Sub      = round(sil_promedios, 3),
    Accion           = "Conservado",
    N_Subclusters    = 1,
    stringsAsFactors = FALSE
  )
  
  # 3. Bucle de evaluación por macro-clúster
  for (i in seq_along(names(sil_promedios))) {
    c_nombre  <- names(sil_promedios)[i]
    filas_idx <- which(df_base$clust == c_nombre)
    n_obs     <- length(filas_idx)
    sil_orig  <- sil_promedios[c_nombre]
    
    # REGLA 1: Solo intentar sub-clustering si silueta < umbral Y N >= n_min
    if (sil_orig < umbral_silueta && n_obs >= n_min) {
      
      datos_locales <- df_base[filas_idx, vars_quanti]
      
      tryCatch({
        # PCA local
        pca_local <- PCA(datos_locales, graph = FALSE)
        
        # REGLA 2: Limitar la partición local máximo a max_k_sub (default = 2)
        hcpc_local <- HCPC(pca_local, nb.clust = -1, min = 2, max = max_k_sub, consol = TRUE, graph = FALSE)
        
        sub_labs_num <- as.numeric(hcpc_local$data.clust$clust)
        sub_labs_chr <- as.character(hcpc_local$data.clust$clust)
        n_sub        <- length(unique(sub_labs_chr))
        
        if (n_sub > 1) {
          # Evaluar la nueva silueta sobre la matriz de distancia
          sub_dist_mat <- as.dist(dist_matrix[filas_idx, filas_idx])
          sil_sub_obj  <- silhouette(sub_labs_num, sub_dist_mat)
          sil_sub_prom <- mean(sil_sub_obj[, 3])
          
          ganancia_silueta <- sil_sub_prom - sil_orig
          
          # REGLA 3: Validar que la ganancia de silueta supere el mínimo exigido
          if (ganancia_silueta >= mejora_min_silueta) {
            etiquetas_finales[filas_idx] <- paste0(c_nombre, ".", sub_labs_chr)
            diagnostico$Accion[i]        <- "Sub-clustered"
            diagnostico$N_Subclusters[i] <- n_sub
            diagnostico$Silueta_Sub[i]   <- round(sil_sub_prom, 3)
          } else {
            etiquetas_finales[filas_idx] <- as.character(c_nombre)
            diagnostico$Accion[i]        <- paste0("Conservado (Mejora silueta < ", mejora_min_silueta, ")")
            diagnostico$Silueta_Sub[i]   <- round(sil_sub_prom, 3)
          }
        } else {
          etiquetas_finales[filas_idx] <- as.character(c_nombre)
          diagnostico$Accion[i]        <- "Conservado (Estructura homogénea)"
        }
        
      }, error = function(e) {
        etiquetas_finales[filas_idx] <<- as.character(c_nombre)
        diagnostico$Accion[i]        <<- paste0("Conservado (Error: ", conditionMessage(e), ")")
      })
      
    } else {
      etiquetas_finales[filas_idx] <- as.character(c_nombre)
      if (n_obs < n_min && sil_orig < umbral_silueta) {
        diagnostico$Accion[i] <- paste0("Conservado (Muestra < ", n_min, ")")
      } else if (sil_orig >= umbral_silueta) {
        diagnostico$Accion[i] <- "Conservado (Silueta aceptable)"
      }
    }
  }
  
  # 4. Consolidar resultado
  df_resultado                    <- df_base
  df_resultado$cluster_macro      <- df_resultado$clust
  df_resultado$cluster_jerarquico <- as.factor(etiquetas_finales)
  df_resultado$clust              <- NULL
  
  cols_finales <- c("cluster_macro", "cluster_jerarquico", setdiff(colnames(df_resultado), c("cluster_macro", "cluster_jerarquico")))
  df_resultado <- df_resultado[, cols_finales]
  
  return(list(
    data = df_resultado,
    diagnostico = diagnostico
  ))
}