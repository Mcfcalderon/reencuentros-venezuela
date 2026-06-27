# ============ AI FUNCTIONS (Google Gemini via ellmer) ============
library(ellmer)

# ============ API KEY ROTATION ============
.gemini_keys <- {
  raw <- Sys.getenv("GEMINI_KEYS")
  if (nchar(raw) > 0) trimws(strsplit(raw, ",")[[1]])
  else {
    single <- Sys.getenv("GEMINI_API_KEY")
    if (nchar(single) > 0) single
    else {
      warning("[Reencuentros] GEMINI_KEYS not set. IA match will fail.")
      "PLACEHOLDER_KEY"
    }
  }
}
assign(".gemini_key_idx_re", 1L, envir = globalenv())

get_gemini <- function() {
  # Intentar key de GEMINI_API_KEY
  key <- trimws(Sys.getenv("GEMINI_API_KEY"))
  
  # Fallback: leer de archivo
  if (nchar(key) == 0 || !startsWith(key, "AIza")) {
    for (ef in c("app_env", ".Renviron")) {
      if (file.exists(ef)) {
        for (ln in readLines(ef, warn = FALSE)) {
          if (startsWith(trimws(ln), "GEMINI_API_KEY=")) {
            key <- trimws(sub("^[^=]+=", "", ln))
            Sys.setenv(GEMINI_API_KEY = key)
            break
          }
        }
        if (nchar(key) > 0 && startsWith(key, "AIza")) break
      }
    }
  }
  
  # Last resort: rotación de keys
  if (nchar(key) == 0 || !startsWith(key, "AIza")) {
    idx <- get0(".gemini_key_idx_re", envir = globalenv(), ifnotfound = 1L)
    key <- .gemini_keys[idx]
  }
  
  message("[Reencuentros] Gemini key: ", substr(key, 1, 8), "*** (", nchar(key), " chars)")
  Sys.setenv(GOOGLE_API_KEY = key)
  chat_google_gemini(model = "gemini-2.5-flash")
}

rotate_key <- function() {
  idx <- get0(".gemini_key_idx_re", envir = globalenv(), ifnotfound = 1L)
  new_idx <- (idx %% length(.gemini_keys)) + 1L
  assign(".gemini_key_idx_re", new_idx, envir = globalenv())
  message("[Reencuentros] Rotated API key to #", new_idx)
  new_idx
}

# ============ MATCH IA: Búsqueda semántica con Gemini ============
match_ia_gemini <- function(descripcion_usuario, reportes_df) {
  if (nrow(reportes_df) == 0) return(data.frame())
  
  # Construir resumen de reportes para el prompt
  resumen_reportes <- paste(sapply(seq_len(nrow(reportes_df)), function(i) {
    r <- reportes_df[i, ]
    paste0(
      "ID:", i,
      " | Tipo:", r$tipo,
      " | Nombre:", ifelse(nzchar(r$nombre %||% ""), r$nombre, "No indicado"),
      " | Ubicación:", ifelse(nzchar(r$ubicacion %||% ""), r$ubicacion, "No indicada"),
      " | Descripción:", ifelse(nzchar(r$descripcion %||% ""), r$descripcion, "Sin descripción"),
      " | Estado:", ifelse(nzchar(r$estado_salud %||% ""), r$estado_salud, "No indicado")
    )
  }), collapse = "\n")
  
  prompt <- paste0(
    "Eres un asistente humanitario de búsqueda de personas desaparecidas tras un terremoto en Venezuela. ",
    "Un familiar está buscando a alguien y ha proporcionado esta descripción:\n\n",
    "DESCRIPCIÓN DEL FAMILIAR: \"", descripcion_usuario, "\"\n\n",
    "REPORTES DISPONIBLES:\n", resumen_reportes, "\n\n",
    "INSTRUCCIONES:\n",
    "- Analiza la descripción del familiar y compárala con cada reporte.\n",
    "- Considera sinónimos (ej: 'suéter' = 'chompa' = 'sweater'), ubicaciones cercanas, ",
    "edades aproximadas, y cualquier detalle que pueda coincidir.\n",
    "- Devuelve SOLO un JSON array con los IDs de reportes que coincidan, ",
    "ordenados de mayor a menor coincidencia, con un score de 1-100.\n",
    "- Formato exacto: [{\"id\":1,\"score\":85,\"razon\":\"breve explicación\"}]\n",
    "- Si no hay coincidencias, devuelve: []\n",
    "- NO incluyas texto adicional, SOLO el JSON array."
  )
  
  tryCatch({
    chat <- get_gemini()
    respuesta <- chat$chat(prompt)
    
    # Extraer JSON de la respuesta
    json_match <- regmatches(respuesta, regexpr("\\[.*\\]", respuesta))
    
    if (length(json_match) == 0 || json_match == "[]") {
      return(data.frame())
    }
    
    coincidencias <- fromJSON(json_match)
    
    if (nrow(coincidencias) == 0) return(data.frame())
    
    # Unir con los reportes originales
    resultado <- reportes_df[coincidencias$id, ]
    resultado$score <- coincidencias$score
    resultado$razon_ia <- coincidencias$razon
    
    resultado[order(-resultado$score), ]
    
  }, error = function(e) {
    message("[Reencuentros] Match IA error: ", e$message)
    # Intentar rotar key si es error de rate limit
    if (grepl("429|503|quota", e$message, ignore.case = TRUE)) {
      rotate_key()
    }
    data.frame()
  })
}
