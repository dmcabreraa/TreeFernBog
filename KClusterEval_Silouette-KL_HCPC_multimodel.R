#' Sub-clustering automático multi-modelo (PCA, MCA, FAMD) (requiere "FactoMineR" y "cluster")
#'
#' @param res_facto Objeto factorial generado por FactoMineR (clase 'PCA', 'MCA' o 'FAMD').
#' @param k_macro Número de clústeres principales (-1 para automático por HCPC).
#' @param umbral_silueta Valor mínimo de silueta media por clúster (default: 0.35).
#' @param n_min Tamaño muestral mínimo por clúster para intentar sub-clustering (default: 15).
#' @param mejora_min_silueta Incremento mínimo de silueta requerido para aceptar la división (default: 0.08).
#' @param max_k_sub Máximo de sub-clústeres locales a generar por división (default: 2).
#'
#' @return Lista con el dataset etiquetado y la tabla diagnóstica.
subclustering_automatico <- function(res_facto, 
                                     k_macro = 3, 
                                     umbral_silueta = 0.35, 
                                     n_min = 15, 
                                     mejora_min_silueta = 0.08,
                                     max_k_sub = 2) {
  
  # 1. DETECTAR EL TIPO DE ANÁLISIS FACTORIAL Y FILTRAR VARIABLES
  clase_facto <- class(res_facto)[1]
  
  # 2. HCPC Macro (Nivel 1)
  res_hcpc_macro <- HCPC(res_facto, nb.clust = k_macro, consol = TRUE, graph = FALSE)
  df_base        <- res_hcpc_macro$data.clust
  cols_evaluadas <- setdiff(colnames(df_base), "clust")
  
  # Seleccionar variables según el tipo de modelo
  if (clase_facto == "PCA") {
    vars_eval <- names(which(sapply(df_base[, cols_evaluadas, drop = FALSE], is.numeric)))
  } else if (clase_facto == "MCA") {
    vars_eval <- names(which(sapply(df_base[, cols_evaluadas, drop = FALSE], function(x) is.factor(x) || is.character(x))))
  } else if (clase_facto == "FAMD") {
    vars_eval <- cols_evaluadas
  } else {
    stop("Clase de objeto no soportada. Debe ser 'PCA', 'MCA' o 'FAMD' generado por FactoMineR.")
  }
  
  # Función auxiliar para ejecutar el modelo factorial local dinámicamente
  ejecutar_facto_local <- function(datos, tipo) {
    if (tipo == "PCA")  return(PCA(datos, graph = FALSE))
    if (tipo == "MCA")  return(MCA(datos, graph = FALSE))
    if (tipo == "FAMD") return(FAMD(datos, graph = FALSE))
  }
  
  # 3. Cálculo de Siluetas Globales (Coordenadas en res_facto$ind$coord)
  coords      <- res_facto$ind$coord
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
  
  # 4. Bucle de evaluación por macro-clúster
  for (i in seq_along(names(sil_promedios))) {
    c_nombre  <- names(sil_promedios)[i]
    filas_idx <- which(df_base$clust == c_nombre)
    n_obs     <- length(filas_idx)
    sil_orig  <- sil_promedios[c_nombre]
    
    # REGLA 1: Silueta < umbral Y N >= n_min
    if (sil_orig < umbral_silueta && n_obs >= n_min) {
      
      datos_locales <- df_base[filas_idx, vars_eval, drop = FALSE]
      
      tryCatch({
        # Ejecución factorial local dinámica (PCA, MCA o FAMD)
        facto_local <- ejecutar_facto_local(datos_locales, clase_facto)
        
        # REGLA 2: Limitar la partición local
        hcpc_local <- HCPC(facto_local, nb.clust = -1, min = 2, max = max_k_sub, consol = TRUE, graph = FALSE)
        
        sub_labs_num <- as.numeric(hcpc_local$data.clust$clust)
        sub_labs_chr <- as.character(hcpc_local$data.clust$clust)
        n_sub        <- length(unique(sub_labs_chr))
        
        if (n_sub > 1) {
          # Evaluar ganancia de silueta
          sub_dist_mat <- as.dist(dist_matrix[filas_idx, filas_idx])
          sil_sub_obj  <- silhouette(sub_labs_num, sub_dist_mat)
          sil_sub_prom <- mean(sil_sub_obj[, 3])
          
          ganancia_silueta <- sil_sub_prom - sil_orig
          
          # REGLA 3: Validar mejora mínima de silueta
          if (ganancia_silueta >= mejora_min_silueta) {
            etiquetas_finales[filas_idx] <- paste0(c_nombre, ".", sub_labs_chr)
            diagnostico$Accion[i]        <- paste0("Sub-clustered (", clase_facto, " local)")
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
  
  # 5. Consolidar DataFrame final
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