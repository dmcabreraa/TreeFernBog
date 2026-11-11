#' Extrae las top N variables (cuantitativas y cualitativas) de HCPC según v.test
#'
#' @param res_hcpc Objeto resultado de la función HCPC() de FactoMineR.
#' @param top_n Número de variables principales a retornar por clúster (por defecto: 3).
#' @param formato "detallado" para tabla explicativa o "resumen" para 1 fila por clúster.
#'
#' @return Un data.frame unificado y limpio listo para reportar.
obtener_top_vtest <- function(res_hcpc, top_n = 3, formato = "detallado") {
  
  # 1. Validar clase del objeto
  if (!inherits(res_hcpc, "HCPC")) {
    stop("El objeto provisto debe ser de clase 'HCPC' generado por FactoMineR.")
  }
  
  desc_var <- res_hcpc$desc.var
  quanti_list <- desc_var$quanti
  cat_list    <- desc_var$category
  
  if (is.null(quanti_list) && is.null(cat_list)) {
    stop("No se encontraron caracterizaciones de variables en 'res.hcpc$desc.var'.")
  }
  
  # Obtener la lista de clústeres presentes
  clusters_nombres <- unique(c(names(quanti_list), names(cat_list)))
  
  # 2. Procesar por clúster
  filas_list <- lapply(clusters_nombres, function(cl) {
    df_quanti <- NULL
    df_cat    <- NULL
    
    # A. Procesar variables cuantitativas (si existen para el clúster)
    if (!is.null(quanti_list[[cl]])) {
      mat_q <- quanti_list[[cl]]
      df_q  <- as.data.frame(mat_q)
      
      df_quanti <- data.frame(
        Cluster         = paste0("Clúster ", cl),
        Tipo            = "Cuantitativa",
        Variable        = rownames(mat_q),
        v_test          = round(df_q$`v.test`, 2),
        Efecto          = ifelse(df_q$`v.test` > 0, "Alto (+)", "Bajo (-)"),
        Detalle_Cluster = paste0("Media: ", round(df_q$`Mean in category`, 2)),
        Detalle_Global  = paste0("Media: ", round(df_q$`Overall mean`, 2)),
        p_valor         = round(df_q$`p.value`, 4),
        stringsAsFactors = FALSE
      )
    }
    
    # B. Procesar variables cualitativas / categorías (si existen para el clúster)
    if (!is.null(cat_list[[cl]])) {
      mat_c <- cat_list[[cl]]
      df_c  <- as.data.frame(mat_c)
      
      df_cat <- data.frame(
        Cluster         = paste0("Clúster ", cl),
        Tipo            = "Cualitativa",
        Variable        = rownames(mat_c), # Suele ser "Variable=Categoría"
        v_test          = round(df_c$`v.test`, 2),
        Efecto          = ifelse(df_c$`v.test` > 0, "Sobrerrepresentado (+)", "Subrepresentado (-)"),
        Detalle_Cluster = paste0(round(df_c$`Mod/Cla`, 1), "% del grupo"),
        Detalle_Global  = paste0(round(df_c$`Global`, 1), "% global"),
        p_valor         = round(df_c$`p.value`, 4),
        stringsAsFactors = FALSE
      )
    }
    
    # C. Combinar cuantitativas y cualitativas
    df_unificado <- rbind(df_quanti, df_cat)
    
    if (is.null(df_unificado) || nrow(df_unificado) == 0) return(NULL)
    
    # D. Filtrar por significancia (|v.test| > 1.96) y ordenar por importancia (|v.test|)
    df_unificado <- df_unificado[abs(df_unificado$v_test) > 1.96, ]
    df_unificado <- df_unificado[order(-abs(df_unificado$v_test)), ]
    
    # E. Seleccionar el Top N
    head(df_unificado, top_n)
  })
  
  # 3. Consolidar tabla general
  tabla_larga <- do.call(rbind, filas_list)
  rownames(tabla_larga) <- NULL
  
  if (is.null(tabla_larga) || nrow(tabla_larga) == 0) {
    warning("Ninguna variable cuantitativa o cualitativa superó el umbral de |v.test| > 1.96.")
    return(NULL)
  }
  
  # 4. Formatear salida según la opción seleccionada
  if (formato == "resumen") {
    clusters <- unique(tabla_larga$Cluster)
    resumen_list <- lapply(clusters, function(c) {
      sub <- tabla_larga[tabla_larga$Cluster == c, ]
      vars_str <- paste0(sub$Variable, " [", sub$Tipo, ", ", sub$Efecto, " v.test=", sub$v_test, "]", collapse = "; ")
      data.frame(Cluster = c, Top_Variables = vars_str, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, resumen_list))
  }
  
  return(tabla_larga)
}