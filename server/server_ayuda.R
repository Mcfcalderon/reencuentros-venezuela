# ============ Server: Cómo Ayudar + Gestión de Reportes ============

reporte_gestion <- reactiveVal(NULL)

# Buscar reporte por código
observeEvent(input$btn_buscar_codigo, {
  req(nzchar(input$codigo_gestionar))
  
  resultado <- tryCatch(
    mg_obtener_por_codigo(input$codigo_gestionar),
    error = function(e) data.frame()
  )
  
  if (nrow(resultado) == 0) {
    showNotification("No se encontr\u00f3 ning\u00fan reporte con ese c\u00f3digo.", type = "error")
    reporte_gestion(NULL)
  } else {
    reporte_gestion(resultado)
  }
})

# Panel de gestión del reporte
output$panel_gestion_ui <- renderUI({
  r <- reporte_gestion()
  if (is.null(r) || nrow(r) == 0) return(NULL)
  
  r <- r[1, ]
  
  etiquetas <- c(nino = "Ni\u00f1o/Ni\u00f1a", adulto = "Adulto",
                 adulto_mayor = "Adulto Mayor", mascota = "Mascota/Animal")
  salud_labels <- c(bien = "Aparentemente bien", herido_leve = "Herido/a leve",
                    herido_grave = "Herido/a grave", necesita_atencion = "Necesita atenci\u00f3n m\u00e9dica",
                    desconocido = "Desconocido")
  
  es_reunificado <- isTRUE(r$reunificado)
  
  div(
    class = "gestion-panel",
    
    # Vista previa del reporte
    div(
      class = "resultado-card",
      style = "border-left: 4px solid #D32F2F;",
      div(class = "resultado-header",
          tags$span(class = "resultado-tipo", etiquetas[r$tipo] %||% r$tipo),
          tags$span(class = "resultado-fecha", r$fecha_reporte %||% "")),
      if (nzchar(r$nombre %||% "")) tags$h5(r$nombre),
      if (nzchar(r$ubicacion %||% ""))
        tags$p(class = "resultado-ubicacion", paste("\U0001F4CD", r$ubicacion)),
      if (nzchar(r$descripcion %||% ""))
        tags$p(class = "resultado-desc", r$descripcion),
      if (nzchar(r$estado_salud %||% ""))
        tags$p(class = "resultado-salud",
               paste("\U0001FA7A Estado:", salud_labels[r$estado_salud] %||% r$estado_salud)),
      if (es_reunificado)
        tags$span(class = "badge-reunificado", "\u2705 REUNIFICADO")
    ),
    
    tags$hr(),
    
    # Formulario de edición
    if (!es_reunificado) {
      div(
        tags$h6("\u270F\uFE0F Editar reporte"),
        selectInput("edit_tipo", "Tipo",
                    choices = c("Ni\u00f1o / Ni\u00f1a" = "nino",
                                "Adulto" = "adulto",
                                "Adulto Mayor" = "adulto_mayor",
                                "Mascota / Animal" = "mascota"),
                    selected = r$tipo),
        textInput("edit_nombre", "Nombre", value = r$nombre %||% ""),
        textInput("edit_ubicacion", "Ubicaci\u00f3n", value = r$ubicacion %||% ""),
        selectInput("edit_estado_salud", "Estado de salud",
                    choices = c("Seleccionar..." = "",
                                "Aparentemente bien" = "bien",
                                "Herido/a leve" = "herido_leve",
                                "Herido/a grave" = "herido_grave",
                                "Necesita atenci\u00f3n m\u00e9dica" = "necesita_atencion",
                                "Desconocido" = "desconocido"),
                    selected = r$estado_salud %||% ""),
        textInput("edit_contacto", "Contacto", value = r$contacto %||% ""),
        textAreaInput("edit_descripcion", "Descripci\u00f3n",
                      value = r$descripcion %||% "", rows = 3),
        
        # Botones de acción
        div(
          class = "gestion-botones",
          actionButton("btn_guardar_edicion", "\U0001F4BE Guardar cambios",
                       class = "btn-guardar"),
          actionButton("btn_reunificar", "\u2705 Marcar reunificado",
                       class = "btn-reunificar"),
          actionButton("btn_eliminar", "\U0001F5D1\uFE0F Eliminar reporte",
                       class = "btn-eliminar")
        )
      )
    } else {
      div(
        tags$p(class = "texto-gris",
               "Este reporte ya fue marcado como reunificado. No se puede editar."),
        actionButton("btn_eliminar", "\U0001F5D1\uFE0F Eliminar reporte",
                     class = "btn-eliminar")
      )
    }
  )
})

# Guardar edición
observeEvent(input$btn_guardar_edicion, {
  req(nzchar(input$codigo_gestionar))
  
  campos <- list(
    tipo = input$edit_tipo,
    nombre = input$edit_nombre,
    ubicacion = input$edit_ubicacion,
    estado_salud = input$edit_estado_salud,
    contacto = input$edit_contacto,
    descripcion = input$edit_descripcion
  )
  
  exito <- tryCatch(
    mg_editar_reporte(input$codigo_gestionar, campos),
    error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      FALSE
    }
  )
  
  if (exito) {
    showNotification("\u2705 Reporte actualizado correctamente.", type = "message")
    # Refrescar vista
    reporte_gestion(mg_obtener_por_codigo(input$codigo_gestionar))
    rv_buscar$refresh <- rv_buscar$refresh + 1
  } else {
    showNotification("No se pudo actualizar el reporte.", type = "warning")
  }
})

# Reunificar
observeEvent(input$btn_reunificar, {
  req(nzchar(input$codigo_gestionar))
  
  showModal(modalDialog(
    title = "\u00bfMarcar como reunificado?",
    tags$p("Esta acci\u00f3n indica que la persona o mascota fue encontrada por su familia."),
    tags$p(tags$strong("Esta acci\u00f3n no se puede deshacer.")),
    footer = tagList(
      modalButton("Cancelar"),
      actionButton("btn_confirmar_reunificar", "\u2705 S\u00ed, confirmar",
                   class = "btn-reunificar")
    ),
    easyClose = TRUE
  ))
})

observeEvent(input$btn_confirmar_reunificar, {
  removeModal()
  
  exito <- tryCatch(
    mg_reunificar(input$codigo_gestionar),
    error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      FALSE
    }
  )
  
  if (exito) {
    showModal(modalDialog(
      title = "\u2705 Caso reunificado",
      div(class = "text-center",
          tags$p("El caso ha sido marcado como reunificado exitosamente."),
          tags$p(class = "texto-gris", "\u00a1Nos alegra que se hayan reencontrado!")),
      footer = modalButton("Cerrar"),
      easyClose = TRUE
    ))
    reporte_gestion(mg_obtener_por_codigo(input$codigo_gestionar))
    rv_buscar$refresh <- rv_buscar$refresh + 1
  } else {
    showNotification("No se pudo reunificar. Verifica el c\u00f3digo.", type = "error")
  }
})

# Eliminar
observeEvent(input$btn_eliminar, {
  req(nzchar(input$codigo_gestionar))
  
  showModal(modalDialog(
    title = "\u26A0\uFE0F \u00bfEliminar reporte?",
    div(
      tags$p("Est\u00e1s a punto de eliminar permanentemente este reporte."),
      tags$p(tags$strong(class = "texto-rojo",
                         "Esta acci\u00f3n NO se puede deshacer."))
    ),
    footer = tagList(
      modalButton("Cancelar"),
      actionButton("btn_confirmar_eliminar", "\U0001F5D1\uFE0F S\u00ed, eliminar",
                   class = "btn-eliminar")
    ),
    easyClose = TRUE
  ))
})

observeEvent(input$btn_confirmar_eliminar, {
  removeModal()
  
  exito <- tryCatch(
    mg_eliminar_reporte(input$codigo_gestionar),
    error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      FALSE
    }
  )
  
  if (exito) {
    showNotification("\u2705 Reporte eliminado correctamente.", type = "message")
    reporte_gestion(NULL)
    updateTextInput(session, "codigo_gestionar", value = "")
    rv_buscar$refresh <- rv_buscar$refresh + 1
  } else {
    showNotification("No se pudo eliminar el reporte.", type = "error")
  }
})

# Estadísticas
output$stats_ui <- renderUI({
  rv_buscar$refresh
  total <- tryCatch(mg_contar_reportes(), error = function(e) 0)
  
  div(
    class = "stats-grid",
    div(class = "stat-card",
        tags$span(class = "stat-numero", total),
        tags$span(class = "stat-label", "Reportes activos")),
    div(class = "stat-card",
        tags$span(class = "stat-numero", "\U0001F7E2"),
        tags$span(class = "stat-label", "Plataforma en l\u00ednea"))
  )
})
