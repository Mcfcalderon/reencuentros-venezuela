# ============ Server: Reportar SOS ============
# Se carga con local=TRUE, tiene acceso a nav_actual y otros reactivos de app.R

tipo_sel <- reactiveVal(NULL)

observeEvent(input$tipo_nino, tipo_sel("nino"))
observeEvent(input$tipo_adulto, tipo_sel("adulto"))
observeEvent(input$tipo_mayor, tipo_sel("adulto_mayor"))
observeEvent(input$tipo_mascota, tipo_sel("mascota"))

# Resaltar botón seleccionado
observe({
  tipos <- c("nino", "adulto", "adulto_mayor", "mascota")
  ids <- c("tipo_nino", "tipo_adulto", "tipo_mayor", "tipo_mascota")
  sel <- tipo_sel()
  
  for (i in seq_along(tipos)) {
    if (!is.null(sel) && tipos[i] == sel) {
      shinyjs::addClass(id = ids[i], class = "btn-tipo-activo")
    } else {
      shinyjs::removeClass(id = ids[i], class = "btn-tipo-activo")
    }
  }
})

output$tipo_seleccionado_ui <- renderUI({
  req(tipo_sel())
  etiquetas <- c(nino = "Ni\u00f1o/Ni\u00f1a", adulto = "Adulto",
                 adulto_mayor = "Adulto Mayor", mascota = "Mascota/Animal")
  div(class = "tipo-badge", paste("\u2705 Seleccionado:", etiquetas[tipo_sel()]))
})

# ── Indicador de progreso (1-4) ──
output$progress_steps_ui <- renderUI({
  paso1 <- !is.null(tipo_sel())
  paso2 <- !is.null(input$video_file)
  paso3 <- !is.null(input$fotos_file)
  paso4 <- nzchar(input$rep_ubicacion %||% "") || nzchar(input$rep_descripcion %||% "")
  
  pasos <- c(paso1, paso2, paso3, paso4)
  
  make_step <- function(n, done) {
    cls <- if (done) "progress-step progress-step-done" else "progress-step"
    tags$span(class = cls, n)
  }
  make_line <- function(done) {
    cls <- if (done) "progress-line progress-line-done" else "progress-line"
    tags$span(class = cls)
  }
  
  div(class = "progress-steps",
      make_step(1, pasos[1]), make_line(pasos[1]),
      make_step(2, pasos[2]), make_line(pasos[2]),
      make_step(3, pasos[3]), make_line(pasos[3]),
      make_step(4, pasos[4]))
})

# ── Vista previa de video ──
output$video_preview_ui <- renderUI({
  req(input$video_file)
  size_mb <- round(input$video_file$size / (1024^2), 1)
  over <- size_mb > MAX_VIDEO_MB
  div(class = "file-preview",
      tags$span(
        style = paste0("color:", if(over) "var(--color-danger)" else "var(--color-success)", ";font-weight:600;"),
        paste0("\U0001F4F9 ", input$video_file$name, " (", size_mb, " MB)",
               if(over) paste0(" \u2014 \u26A0\uFE0F Excede l\u00edmite de ", MAX_VIDEO_MB, "MB") else " \u2705")
      ))
})

# ── Vista previa de fotos ──
output$fotos_preview_ui <- renderUI({
  req(input$fotos_file)
  imgs <- lapply(seq_len(nrow(input$fotos_file)), function(i) {
    size_mb <- round(input$fotos_file$size[i] / (1024^2), 1)
    over <- size_mb > MAX_FOTO_MB
    # Generar data URI para la preview
    raw <- readBin(input$fotos_file$datapath[i], "raw",
                   min(input$fotos_file$size[i], 300000))
    src <- paste0("data:image/jpeg;base64,", base64enc::base64encode(raw))
    div(style = "display:inline-block; margin:4px; text-align:center;",
        tags$img(src = src, style = "max-width:100px; max-height:100px; object-fit:cover; border-radius:8px; border:1px solid #ddd;"),
        tags$div(style = paste0("font-size:0.7rem; color:", if(over) "var(--color-danger)" else "var(--color-text-secondary)", ";"),
                 paste0(size_mb, "MB", if(over) " \u26A0\uFE0F" else "")))
  })
  div(class = "file-preview", imgs)
})

# Comprimir foto a thumbnail (max ~200KB por foto → safe para MongoDB)
foto_a_thumbnail <- function(file_info, max_width = 800, quality = 70) {
  if (is.null(file_info)) return(NULL)
  
  rutas_b64 <- sapply(seq_len(nrow(file_info)), function(i) {
    tryCatch({
      ext <- tolower(tools::file_ext(file_info$name[i]))
      
      # Leer imagen según formato
      img <- if (ext %in% c("jpg", "jpeg")) {
        jpeg::readJPEG(file_info$datapath[i])
      } else if (ext == "png") {
        png::readPNG(file_info$datapath[i])
      } else {
        # Para otros formatos, leer raw pero limitar tamaño
        raw <- readBin(file_info$datapath[i], "raw", 
                       min(file.info(file_info$datapath[i])$size, 500000))
        return(paste0("data:image/", ext, ";base64,", base64enc::base64encode(raw)))
      }
      
      # Redimensionar si es muy grande
      dims <- dim(img)
      h <- dims[1]; w <- dims[2]
      if (w > max_width) {
        ratio <- max_width / w
        new_w <- max_width
        new_h <- round(h * ratio)
        # Resize simple por interpolación
        row_idx <- round(seq(1, h, length.out = new_h))
        col_idx <- round(seq(1, w, length.out = new_w))
        if (length(dims) == 3) {
          img <- img[row_idx, col_idx, ]
        } else {
          img <- img[row_idx, col_idx]
        }
      }
      
      # Guardar como JPEG comprimido en archivo temporal
      tmp <- tempfile(fileext = ".jpg")
      jpeg::writeJPEG(img, tmp, quality = quality / 100)
      raw <- readBin(tmp, "raw", file.info(tmp)$size)
      unlink(tmp)
      
      paste0("data:image/jpeg;base64,", base64enc::base64encode(raw))
    }, error = function(e) {
      # Fallback: leer raw pero limitar a 500KB
      message("[Reencuentros] Error procesando foto: ", e$message)
      raw <- readBin(file_info$datapath[i], "raw", 
                     min(file.info(file_info$datapath[i])$size, 500000))
      paste0("data:image/jpeg;base64,", base64enc::base64encode(raw))
    })
  })
  paste(rutas_b64, collapse = "|||")
}

# Publicar reporte
observeEvent(input$btn_publicar, {
  # Validaciones de campos requeridos
  errores <- c()
  if (is.null(tipo_sel())) errores <- c(errores, "Selecciona a qui\u00e9n vas a reportar")
  if (!nzchar(input$rep_ubicacion)) errores <- c(errores, "La ubicaci\u00f3n es obligatoria")
  if (!nzchar(input$rep_descripcion) && is.null(input$video_file) && is.null(input$fotos_file))
    errores <- c(errores, "Agrega al menos una descripci\u00f3n, foto o video")
  
  if (length(errores) > 0) {
    showNotification(
      paste("\u26A0\uFE0F", paste(errores, collapse = "\n")),
      type = "error", duration = 5
    )
    return()
  }
  
  # Validar tamaño de video
  if (!is.null(input$video_file)) {
    size_mb <- input$video_file$size / (1024^2)
    if (size_mb > MAX_VIDEO_MB) {
      showNotification(
        paste0("El video excede el l\u00edmite de ", MAX_VIDEO_MB, "MB (", round(size_mb, 1), "MB)"),
        type = "error"
      )
      return()
    }
  }
  
  # Validar tamaño de fotos
  if (!is.null(input$fotos_file)) {
    for (i in seq_len(nrow(input$fotos_file))) {
      size_mb <- input$fotos_file$size[i] / (1024^2)
      if (size_mb > MAX_FOTO_MB) {
        showNotification(
          paste0("La foto '", input$fotos_file$name[i], "' excede ", MAX_FOTO_MB, "MB"),
          type = "error"
        )
        return()
      }
    }
  }
  
  showNotification("Publicando reporte...", type = "message", id = "pub_notif", duration = NULL)
  
  # Fotos: comprimir a thumbnails y guardar como base64
  fotos_b64 <- foto_a_thumbnail(input$fotos_file)
  
  # Video: solo guardar metadata (nombre y tamaño), NO el archivo
  video_meta <- NULL
  if (!is.null(input$video_file)) {
    video_meta <- paste0(
      input$video_file$name, " (",
      round(input$video_file$size / (1024^2), 1), " MB)"
    )
  }
  
  # Insertar en MongoDB
  codigo <- tryCatch({
    mg_insertar_reporte(
      tipo = tipo_sel(),
      nombre = input$rep_nombre,
      descripcion = input$rep_descripcion,
      ubicacion = input$rep_ubicacion,
      estado_salud = input$rep_estado_salud,
      contacto = input$rep_contacto,
      fotos_b64 = fotos_b64,
      video_b64 = video_meta
    )
  }, error = function(e) {
    showNotification(paste("Error al publicar:", e$message), type = "error")
    removeNotification("pub_notif")
    return(NULL)
  })
  
  removeNotification("pub_notif")
  
  if (!is.null(codigo)) {
    showModal(modalDialog(
      title = div(class = "text-center", tags$h3("\u2705 Reporte publicado exitosamente")),
      div(
        class = "text-center",
        tags$p("Tu c\u00f3digo de verificaci\u00f3n es:"),
        tags$h1(class = "codigo-display", id = "codigo-valor", codigo),
        tags$button(
          class = "btn-copiar-codigo",
          onclick = paste0(
            "navigator.clipboard.writeText('", codigo, "');",
            "this.textContent='\u2705 Copiado!';",
            "setTimeout(()=>this.textContent='\U0001F4CB Copiar c\u00f3digo',2000);"
          ),
          "\U0001F4CB Copiar c\u00f3digo"
        ),
        tags$p(class = "texto-gris", style = "margin-top:15px;",
               "\u26A0\uFE0F Guarda este c\u00f3digo. Es la \u00fanica forma de ",
               "editar, eliminar o marcar tu reporte como reunificado."),
        if (!is.null(video_meta))
          tags$p(class = "texto-gris",
                 "\U0001F4F9 Video registrado: ", video_meta)
      ),
      footer = modalButton("Cerrar"),
      easyClose = FALSE
    ))
    
    # Refrescar resultados de búsqueda
    rv_buscar$refresh <- rv_buscar$refresh + 1
    
    # Limpiar formulario
    tipo_sel(NULL)
    updateTextInput(session, "rep_nombre", value = "")
    updateTextInput(session, "rep_ubicacion", value = "")
    updateTextInput(session, "rep_contacto", value = "")
    updateTextAreaInput(session, "rep_descripcion", value = "")
    updateSelectInput(session, "rep_estado_salud", selected = "")
  }
})
