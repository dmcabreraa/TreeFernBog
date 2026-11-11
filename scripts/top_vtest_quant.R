#' Extrae las top N variables cuantitativas de un objeto HCPC según v.test
#'
#' @param res_hcpc Objeto resultado de la función HCPC() de FactoMineR.
#' @param top_n Número de variables principales a retornar por clúster (por defecto: 3).
#' @param formato "detallado" para tabla larga o "resumen" para 1 fila por clúster.
#'
#' @return Un data.frame limpio listo para reportar o exportar.
obtener_top_vtest <- function(res_hcpc, top_n = 3, formato = "detallado") {
  
  # 1. Validar que el objeto sea de clase HCPC
  if (!inherits(res_hcpc, "HCPC")) {
    stop("El objeto provisto debe ser de clase 'HCPC' generado por FactoMineR.")
  }
  
  quanti_list <- res_hcpc$desc.var$quanti
  if (is.null(quanti_list)) {
    stop("No se encontraron variables cuantitativas en 'res.hcpc$desc.var$quanti'.")
  }
  
  # 2. Procesar cada clúster
  filas_list <- lapply(names(quanti_list), function(cl) {
    mat <- quanti_list[[cl]]
    df <- as.data.frame(mat)
    df$Variable <- rownames(mat)
    df$Cluster <- paste0("Clúster ", cl)
    
    # Filtrar solo significativos (|v.test| > 1.96) y ordenar por magnitud de v.test
    df <- df[abs(df$`v.test`) > 1.96, ]
    df <- df[order(-abs(df$`v.test`)), ]
    
    # Seleccionar el top_n
    df_top <- head(df, top_n)
    
    if (nrow(df_top) == 0) return(NULL)
    
    # Crear etiquetas descriptivas y redondear
    df_top$Efecto <- ifelse(df_top$`v.test` > 0, "Alto (+)", "Bajo (-)")
    df_top$v.test_val <- round(df_top$`v.test`, 2)
    df_top$Media_Cluster <- round(df_top$`Mean in category`, 2)
    df_top$Media_Global  <- round(df_top$`Overall mean`, 2)
    df_top$p_valor       <- round(df_top$`p.value`, 4)
    
    # Reordenar columnas
    df_top[, c("Cluster", "Variable", "v.test_val", "Efecto", 
               "Media_Cluster", "Media_Global", "p_valor")]
  })
  
  # 3. Consolidar en una sola tabla
  tabla_larga <- do.call(rbind, filas_list)
  rownames(tabla_larga) <- NULL
  colnames(tabla_larga) <- c("Cluster", "Variable", "v_test", "Efecto", 
                             "Media_Cluster", "Media_Global", "p_valor")
  
  # 4. Retornar según el formato seleccionado
  if (formato == "resumen") {
    clusters <- unique(tabla_larga$Cluster)
    resumen_list <- lapply(clusters, function(c) {
      sub <- tabla_larga[tabla_larga$Cluster == c, ]
      vars_str <- paste0(sub$Variable, " (", sub$Efecto, " v.test=", sub$v_test, ")", collapse = "; ")
      data.frame(Cluster = c, Top_Variables = vars_str)
    })
    return(do.call(rbind, resumen_list))
  }
  
  return(tabla_larga)
}