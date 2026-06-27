# ============ MONGODB FUNCTIONS — Reencuentros Venezuela ============
# Sin autenticación de usuarios (app pública humanitaria)
library(mongolite)

# URI: usa MONGODB_URI_REENCUENTROS o fallback a MONGODB_URI
.mongo_cache_re <- new.env(parent = emptyenv())

get_mongo_uri <- function() {
  uri <- Sys.getenv("MONGODB_URI_REENCUENTROS")
  if (nchar(uri) == 0) uri <- Sys.getenv("MONGODB_URI")
  uri
}

mongo_col <- function(collection) {
  if (!isTRUE(get0(".app_initialized", envir = globalenv()))) {
    # Intentar inicializar
    ensure_init()
    if (!isTRUE(get0(".app_initialized", envir = globalenv()))) return(NULL)
  }
  
  if (exists(collection, envir = .mongo_cache_re)) {
    conn <- get(collection, envir = .mongo_cache_re)
    alive <- tryCatch({ conn$count(); TRUE }, error = function(e) FALSE)
    if (alive) return(conn)
  }
  
  conn <- mongolite::mongo(collection = collection, url = get_mongo_uri())
  assign(collection, conn, envir = .mongo_cache_re)
  conn
}

# ============ GENERAR CÓDIGO ÚNICO DE 4 DÍGITOS ============
generar_codigo <- function() {
  col <- mongo_col("reportes")
  if (is.null(col)) return(sprintf("%04d", sample(0:9999, 1)))
  
  for (i in 1:100) {
    codigo <- sprintf("%04d", sample(0:9999, 1))
    n <- col$count(paste0('{"codigo":"', codigo, '"}'))
    if (n == 0) return(codigo)
  }
  # Fallback: código con más dígitos si todo falla
  sprintf("%06d", sample(0:999999, 1))
}

# ============ INSERTAR REPORTE ============
mg_insertar_reporte <- function(tipo, nombre, descripcion, ubicacion,
                                 estado_salud, contacto, fotos_b64, video_b64) {
  col <- mongo_col("reportes")
  if (is.null(col)) {
    message("[Reencuentros] MongoDB no disponible para insertar")
    return(NULL)
  }
  
  codigo <- generar_codigo()
  
  doc <- list(
    codigo = codigo,
    tipo = tipo,
    nombre = ifelse(is.null(nombre) || !nzchar(nombre), "", nombre),
    descripcion = ifelse(is.null(descripcion) || !nzchar(descripcion), "", descripcion),
    ubicacion = ifelse(is.null(ubicacion) || !nzchar(ubicacion), "", ubicacion),
    estado_salud = ifelse(is.null(estado_salud) || !nzchar(estado_salud), "", estado_salud),
    contacto = ifelse(is.null(contacto) || !nzchar(contacto), "", contacto),
    fotos = ifelse(is.null(fotos_b64), "", fotos_b64),
    video = ifelse(is.null(video_b64), "", video_b64),
    fecha_reporte = as.character(Sys.time()),
    reunificado = FALSE
  )
  
  col$insert(toJSON(doc, auto_unbox = TRUE))
  message("[Reencuentros] Reporte insertado con código: ", codigo)
  codigo
}

# ============ BUSCAR REPORTES ============
mg_buscar <- function(texto = NULL, tipo = NULL) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(data.frame())
  
  filtros <- list(reunificado = FALSE)
  
  if (!is.null(tipo) && tipo != "todos") {
    filtros$tipo <- tipo
  }
  
  query <- toJSON(filtros, auto_unbox = TRUE)
  
  result <- col$find(query, sort = '{"fecha_reporte":-1}')
  
  if (nrow(result) == 0) return(data.frame())
  
  # Filtrar por texto si se proporcionó
  if (!is.null(texto) && nzchar(texto)) {
    texto_lower <- tolower(texto)
    matches <- sapply(seq_len(nrow(result)), function(i) {
      campos <- tolower(paste(
        result$nombre[i], result$descripcion[i],
        result$ubicacion[i], result$estado_salud[i]
      ))
      grepl(texto_lower, campos, fixed = TRUE)
    })
    result <- result[matches, ]
  }
  
  result
}

# ============ OBTENER TODOS LOS REPORTES ACTIVOS ============
mg_obtener_todos <- function() {
  col <- mongo_col("reportes")
  if (is.null(col)) return(data.frame())
  
  result <- col$find('{"reunificado":false}', sort = '{"fecha_reporte":-1}')
  if (nrow(result) == 0) return(data.frame())
  result
}

# ============ REUNIFICAR (marcar como encontrado) ============
mg_reunificar <- function(codigo) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(FALSE)
  
  n <- col$update(
    paste0('{"codigo":"', codigo, '","reunificado":false}'),
    '{"$set":{"reunificado":true}}',
    upsert = FALSE
  )
  
  # n$modifiedCount > 0
  isTRUE(n$modifiedCount > 0)
}

# ============ OBTENER POR CÓDIGO ============
mg_obtener_por_codigo <- function(codigo) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(data.frame())
  col$find(paste0('{"codigo":"', codigo, '"}'))
}

# ============ CONTAR REPORTES ============
mg_contar_reportes <- function() {
  col <- mongo_col("reportes")
  if (is.null(col)) return(0)
  col$count('{"reunificado":false}')
}
