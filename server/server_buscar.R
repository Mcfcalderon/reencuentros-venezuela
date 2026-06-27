# ============ Server: Buscar ============

tab_activa <- reactiveVal("tradicional")
resultados_busqueda <- reactiveVal(data.frame())
rv_buscar <- reactiveValues(refresh = 0)

# Cargar todos los reportes al iniciar o al cambiar a pestaña Buscar
observe({
  rv_buscar$refresh
  req(nav_actual() == "buscar")
  resultado <- tryCatch(mg_obtener_todos(), error = function(e) data.frame())
  resultados_busqueda(resultado)
})

# Control de tabs
observeEvent(input$tab_tradicional, {
  tab_activa("tradicional")
  shinyjs::addClass(id = "tab_tradicional", class = "btn-tab-activo")
  shinyjs::removeClass(id = "tab_ia", class = "btn-tab-activo")
  shinyjs::show("panel_tradicional")
})

observeEvent(input$tab_ia, {
  tab_activa("ia")
  shinyjs::addClass(id = "tab_ia", class = "btn-tab-activo")
  shinyjs::removeClass(id = "tab_tradicional", class = "btn-tab-activo")
  shinyjs::hide("panel_tradicional")
})

# Panel IA
output$panel_ia_ui <- renderUI({
  req(tab_activa() == "ia")
  div(
    class = "paso-card ia-card",
    tags$h5("\u2728 CRUZAR DATOS CON INTELIGENCIA ARTIFICIAL"),
    tags$p(
      "Describe libremente lo que recuerdas de tu familiar. ",
      "La IA procesar\u00e1 vestimentas, sin\u00f3nimos, nombres y lugares ",
      "aproximados de avistamiento para encontrar coincidencias."
    ),
    textAreaInput("descripcion_ia", label = NULL,
                  placeholder = "Ej: Busco a mi abuela Carmen, se perdi\u00f3 por Altamira, vest\u00eda un su\u00e9ter color gris de lana...",
                  rows = 4),
    actionButton("btn_buscar_ia", "\u2728 COMENZAR B\u00daSQUEDA INTELIGENTE",
                 class = "btn-buscar-ia", width = "100%")
  )
})

# Búsqueda tradicional
observeEvent(input$btn_buscar, {
  showNotification("Buscando...", type = "message", duration = 2)
  resultado <- tryCatch(
    mg_buscar(texto = input$txt_busqueda, tipo = input$filtro_tipo),
    error = function(e) {
      showNotification(paste("Error en b\u00fasqueda:", e$message), type = "error")
      data.frame()
    }
  )
  resultados_busqueda(resultado)
})

# Búsqueda IA con Gemini
observeEvent(input$btn_buscar_ia, {
  req(nzchar(input$descripcion_ia))
  showNotification("Buscando coincidencias con IA...", type = "message",
                   id = "ia_notif", duration = NULL)
  todos <- tryCatch(mg_obtener_todos(), error = function(e) data.frame())
  if (nrow(todos) == 0) {
    removeNotification("ia_notif")
    showNotification("No hay reportes registrados a\u00fan.", type = "warning")
    resultados_busqueda(data.frame())
    return()
  }
  resultado <- tryCatch(
    match_ia_gemini(input$descripcion_ia, todos),
    error = function(e) {
      showNotification(paste("Error en Match IA:", e$message), type = "error")
      data.frame()
    }
  )
  removeNotification("ia_notif")
  if (nrow(resultado) == 0) {
    showNotification("No se encontraron coincidencias. Intenta con m\u00e1s detalles.", type = "warning")
  } else {
    showNotification(paste0("\u2728 ", nrow(resultado), " coincidencia(s) encontrada(s)"), type = "message")
  }
  resultados_busqueda(resultado)
})

# Contador de reportes activos
output$reportes_counter_ui <- renderUI({
  rv_buscar$refresh
  total <- tryCatch(mg_contar_reportes(), error = function(e) 0)
  if (total > 0) {
    div(class = "reportes-counter",
        paste0("\U0001F4CB ", total, " reporte", ifelse(total != 1, "s", ""),
               " activo", ifelse(total != 1, "s", "")))
  }
})

output$resultados_conteo <- renderText({
  datos <- resultados_busqueda()
  if (nrow(datos) == 0) "Resultados" else paste0("Resultados (", nrow(datos), ")")
})

# ============ HELPER: Formatear fecha legible ============
formatear_fecha <- function(fecha_str) {
  tryCatch({
    fecha <- as.POSIXct(fecha_str, tz = "UTC")
    ahora <- Sys.time()
    diff_secs <- as.numeric(difftime(ahora, fecha, units = "secs"))
    
    if (diff_secs < 60) return("Hace un momento")
    if (diff_secs < 3600) return(paste0("Hace ", floor(diff_secs / 60), " min"))
    if (diff_secs < 86400) return(paste0("Hace ", floor(diff_secs / 3600), " h"))
    if (diff_secs < 172800) return("Ayer")
    if (diff_secs < 604800) return(paste0("Hace ", floor(diff_secs / 86400), " d\u00edas"))
    format(fecha, "%d %b, %I:%M %p")
  }, error = function(e) fecha_str %||% "")
}

# ============ HELPER: Icono de estado de salud ============
salud_icon_html <- function(estado) {
  info <- switch(estado,
    bien            = list(icon = "\u2705", color = "#2E7D32", label = "Aparentemente bien"),
    herido_leve     = list(icon = "\U0001FA79", color = "#E65100", label = "Herido/a leve"),
    herido_grave    = list(icon = "\U0001F6A8", color = "#C62828", label = "Herido/a grave"),
    necesita_atencion = list(icon = "\U0001F3E5", color = "#AD1457", label = "Necesita atenci\u00f3n m\u00e9dica"),
    desconocido     = list(icon = "\u2753", color = "#757575", label = "Desconocido"),
    list(icon = "\U0001FA7A", color = "#757575", label = estado)
  )
  tags$span(class = "salud-badge",
            style = paste0("background:", info$color, "20; color:", info$color, ";"),
            paste(info$icon, info$label))
}

# ============ HELPER: Galería de fotos con lightbox ============
render_galeria_fotos <- function(fotos_str, reporte_id) {
  if (is.null(fotos_str) || !nzchar(fotos_str %||% "")) return(NULL)
  
  fotos_list <- strsplit(fotos_str, "\\|\\|\\|")[[1]]
  fotos_list <- fotos_list[nzchar(fotos_list)]
  if (length(fotos_list) == 0) return(NULL)
  
  div(
    class = "galeria-fotos",
    lapply(seq_along(fotos_list), function(j) {
      foto_id <- paste0("foto_", reporte_id, "_", j)
      div(
        class = "galeria-thumb-wrapper",
        tags$img(
          src = fotos_list[j],
          class = "galeria-thumb",
          alt = paste("Foto", j, "del reporte"),
          onclick = paste0(
            "var m=document.getElementById('lightbox-overlay');",
            "var img=document.getElementById('lightbox-img');",
            "img.src=this.src;",
            "m.style.display='flex';"
          )
        ),
        # Ícono de lupa superpuesto
        tags$span(class = "galeria-zoom-icon", "\U0001F50D")
      )
    })
  )
}

# ============ RENDERIZAR TARJETAS DE RESULTADOS ============
output$resultados_ui <- renderUI({
  datos <- resultados_busqueda()
  
  if (nrow(datos) == 0) {
    return(div(
      class = "sin-resultados",
      tags$p("\U0001F50D No hay reportes activos. Los reportes aparecen aqu\u00ed autom\u00e1ticamente.")
    ))
  }
  
  etiquetas <- c(nino = "\U0001F9D2 Ni\u00f1o/a", adulto = "\U0001F9D1 Adulto",
                 adulto_mayor = "\U0001F9D3 Adulto Mayor", mascota = "\U0001F43E Mascota")
  
  tarjetas <- lapply(seq_len(nrow(datos)), function(i) {
    r <- datos[i, ]
    rid <- r$codigo %||% i
    
    # Galería de fotos con lightbox
    fotos_html <- render_galeria_fotos(r$fotos, rid)
    
    # Video: reproducible si está en GridFS
    video_html <- NULL
    if ("video" %in% names(r) && nzchar(r$video %||% "")) {
      if (startsWith(r$video, "gridfs:")) {
        # Video almacenado en GridFS — cargar y mostrar con <video>
        fs_name <- sub("^gridfs:", "", r$video)
        video_src <- tryCatch(mg_obtener_video(fs_name), error = function(e) NULL)
        if (!is.null(video_src)) {
          video_name <- if ("video_name" %in% names(r)) r$video_name else "Video"
          video_html <- div(
            class = "video-container",
            tags$p(class = "video-label", paste0("\U0001F4F9 ", video_name)),
            tags$video(
              src = video_src,
              type = "video/mp4",
              controls = NA,
              preload = "metadata",
              style = "width:100%; border-radius:8px; max-height:400px;",
              `aria-label` = paste("Video del reporte", rid)
            )
          )
        } else {
          video_html <- div(class = "video-meta-badge",
                            tags$span("\U0001F4F9"), tags$span("Video no disponible"))
        }
      } else {
        # Legacy: solo metadata de texto
        video_html <- div(class = "video-meta-badge",
                          tags$span("\U0001F4F9"), tags$span(r$video))
      }
    }
    
    div(
      class = "resultado-card",
      # Header: tipo + fecha
      div(
        class = "resultado-header",
        tags$span(class = "resultado-tipo", etiquetas[r$tipo] %||% r$tipo),
        tags$span(class = "resultado-fecha", formatear_fecha(r$fecha_reporte))
      ),
      # Nombre
      if (nzchar(r$nombre %||% ""))
        tags$h5(class = "resultado-nombre", r$nombre),
      # Ubicación
      if (nzchar(r$ubicacion %||% ""))
        tags$p(class = "resultado-ubicacion", paste("\U0001F4CD", r$ubicacion)),
      # Estado de salud con ícono y color
      if (nzchar(r$estado_salud %||% ""))
        salud_icon_html(r$estado_salud),
      # Descripción
      if (nzchar(r$descripcion %||% ""))
        tags$p(class = "resultado-desc", r$descripcion),
      # Contacto
      if (nzchar(r$contacto %||% ""))
        tags$p(class = "resultado-contacto", paste("\U0001F4DE", r$contacto)),
      # Galería de fotos
      fotos_html,
      # Video info
      video_html,
      # Score IA
      if ("score" %in% names(r) && !is.na(r$score))
        tags$span(class = "match-score", paste0("\u2728 Match IA: ", r$score, "%")),
      if ("razon_ia" %in% names(r) && nzchar(r$razon_ia %||% ""))
        tags$p(class = "texto-gris", style = "font-size:0.8rem; margin-top:5px;",
               paste0("\U0001F4AC ", r$razon_ia))
    )
  })
  
  tagList(
    # Lightbox overlay (global, reutilizable)
    div(
      id = "lightbox-overlay",
      class = "lightbox-overlay",
      style = "display:none;",
      onclick = "this.style.display='none';",
      tags$img(id = "lightbox-img", class = "lightbox-img", src = "", alt = "Foto ampliada"),
      tags$span(class = "lightbox-close", onclick = "document.getElementById('lightbox-overlay').style.display='none';", "\u2715")
    ),
    # Grid de resultados
    div(class = "resultados-grid", tarjetas)
  )
})
