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
  
  # Construir query manualmente para control exacto del JSON
  tipo_filter <- if (!is.null(tipo) && tipo != "todos") {
    paste0(',"tipo":"', tipo, '"')
  } else ""
  
  query <- paste0('{"$or":[{"reunificado":false},{"reunificado":{"$exists":false}}]', tipo_filter, '}')
  
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
  
  # Buscar reportes no reunificados (false o que no tengan el campo)
  result <- col$find(
    '{"$or":[{"reunificado":false},{"reunificado":{"$exists":false}}]}',
    sort = '{"fecha_reporte":-1}'
  )
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

# ============ EDITAR REPORTE (requiere código) ============
mg_editar_reporte <- function(codigo, campos) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(FALSE)
  
  # Solo permitir editar campos seguros
  campos_permitidos <- c("nombre", "descripcion", "ubicacion",
                         "estado_salud", "contacto", "tipo")
  campos <- campos[names(campos) %in% campos_permitidos]
  
  if (length(campos) == 0) return(FALSE)
  
  update_json <- toJSON(list("$set" = campos), auto_unbox = TRUE)
  query_json <- paste0('{"codigo":"', codigo, '"}')
  
  n <- col$update(query_json, update_json, upsert = FALSE)
  isTRUE(n$modifiedCount > 0)
}

# ============ ELIMINAR REPORTE (requiere código) ============
mg_eliminar_reporte <- function(codigo) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(FALSE)
  
  n <- col$remove(paste0('{"codigo":"', codigo, '"}'))
  isTRUE(n > 0)
}

# ============ GRIDFS: GUARDAR VIDEO ============
mg_guardar_video <- function(file_path, filename, codigo) {
  tryCatch({
    fs <- mongolite::gridfs(url = get_mongo_uri(), prefix = "videos")
    # Nombre sin espacios para evitar problemas con mongolite
    fs_name <- paste0(codigo, "_", gsub("[^a-zA-Z0-9._-]", "_", filename))
    fs$write(file(file_path, "rb"), name = fs_name)
    
    # Obtener el ID del archivo guardado
    archivos <- fs$find(paste0('{"filename":"', fs_name, '"}'))
    fs_id <- if (nrow(archivos) > 0) archivos$id[1] else NULL
    
    message("[Reencuentros] Video guardado en GridFS. Name: ", fs_name, " ID: ", fs_id)
    # Devolver el ID (más confiable para lectura)
    list(id = fs_id, name = fs_name)
  }, error = function(e) {
    message("[Reencuentros] Error guardando video en GridFS: ", e$message)
    NULL
  })
}

# ============ GRIDFS: OBTENER VIDEO ============
mg_obtener_video <- function(video_ref) {
  tryCatch({
    fs <- mongolite::gridfs(url = get_mongo_uri(), prefix = "videos")
    tmp <- tempfile(fileext = ".mp4")
    
    # Leer por ObjectID (formato: "id:abc123") o por nombre
    if (startsWith(video_ref, "id:")) {
      oid <- sub("^id:", "", video_ref)
      query <- paste0('{"_id":{"$oid":"', oid, '"}}')
      fs$read(name = query, con = tmp)
    } else {
      # Legacy: intentar buscar por nombre y leer por ID
      archivos <- fs$find()
      match_idx <- grep(video_ref, archivos$name, fixed = TRUE)
      if (length(match_idx) > 0) {
        oid <- archivos$id[match_idx[1]]
        query <- paste0('{"_id":{"$oid":"', oid, '"}}')
        fs$read(name = query, con = tmp)
      } else {
        stop("Archivo no encontrado en GridFS")
      }
    }
    
    raw <- readBin(tmp, "raw", file.info(tmp)$size)
    unlink(tmp)
    paste0("data:video/mp4;base64,", base64enc::base64encode(raw))
  }, error = function(e) {
    message("[Reencuentros] Error leyendo video de GridFS: ", e$message)
    NULL
  })
}

# ============ GRIDFS: ELIMINAR VIDEO ============
mg_eliminar_video <- function(video_ref) {
  tryCatch({
    fs <- mongolite::gridfs(url = get_mongo_uri(), prefix = "videos")
    if (startsWith(video_ref, "id:")) {
      oid <- sub("^id:", "", video_ref)
      query <- paste0('{"_id":{"$oid":"', oid, '"}}')
      fs$remove(query)
    } else {
      # Legacy: buscar por nombre parcial
      archivos <- fs$find()
      match_idx <- grep(video_ref, archivos$name, fixed = TRUE)
      if (length(match_idx) > 0) {
        oid <- archivos$id[match_idx[1]]
        query <- paste0('{"_id":{"$oid":"', oid, '"}}')
        fs$remove(query)
      }
    }
    message("[Reencuentros] Video eliminado de GridFS")
    TRUE
  }, error = function(e) {
    message("[Reencuentros] Error eliminando video: ", e$message)
    FALSE
  })
}

# ============ RECUPERAR CÓDIGO POR CONTACTO ============
mg_recuperar_por_contacto <- function(contacto) {
  col <- mongo_col("reportes")
  if (is.null(col)) return(data.frame())
  if (!nzchar(contacto)) return(data.frame())
  
  # Búsqueda exacta por contacto
  result <- col$find(
    paste0('{"contacto":"', gsub('"', '\\\\"', contacto), '"}'),
    fields = '{"codigo":1, "tipo":1, "nombre":1, "ubicacion":1, "fecha_reporte":1, "reunificado":1}'
  )
  result
}

# ============ CONTAR REPORTES ============
mg_contar_reportes <- function() {
  col <- mongo_col("reportes")
  if (is.null(col)) return(0)
  col$count('{"reunificado":false}')
}
