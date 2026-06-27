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

# Convertir archivo a base64 para guardar en MongoDB
archivo_a_base64 <- function(file_info) {
  if (is.null(file_info)) return(NULL)
  
  rutas_b64 <- sapply(seq_len(nrow(file_info)), function(i) {
    raw <- readBin(file_info$datapath[i], "raw", file.info(file_info$datapath[i])$size)
    ext <- tolower(tools::file_ext(file_info$name[i]))
    mime <- switch(ext,
      jpg = "image/jpeg", jpeg = "image/jpeg",
      png = "image/png", webp = "image/webp",
      mp4 = "video/mp4", mov = "video/quicktime",
      webm = "video/webm",
      "application/octet-stream"
    )
    paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
  })
  paste(rutas_b64, collapse = "|||")
}

# Publicar reporte
observeEvent(input$btn_publicar, {
  # Validaciones
  if (is.null(tipo_sel())) {
    showNotification("Selecciona a qui\u00e9n vas a reportar", type = "error")
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
  
  # Convertir media a base64
  video_b64 <- archivo_a_base64(input$video_file)
  fotos_b64 <- archivo_a_base64(input$fotos_file)
  
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
      video_b64 = video_b64
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
        tags$h1(class = "codigo-display", codigo),
        tags$p(class = "texto-gris",
               "Guarda este c\u00f3digo. Solo quien lo posee puede ",
               "marcar el caso como reunificado.")
      ),
      footer = modalButton("Cerrar"),
      easyClose = TRUE
    ))
    
    # Limpiar formulario
    tipo_sel(NULL)
    updateTextInput(session, "rep_nombre", value = "")
    updateTextInput(session, "rep_ubicacion", value = "")
    updateTextInput(session, "rep_contacto", value = "")
    updateTextAreaInput(session, "rep_descripcion", value = "")
    updateSelectInput(session, "rep_estado_salud", selected = "")
  }
})
