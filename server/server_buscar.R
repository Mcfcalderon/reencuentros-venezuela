# ============ Server: Buscar ============

tab_activa <- reactiveVal("tradicional")
resultados_busqueda <- reactiveVal(data.frame())

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

# Panel IA (renderizado dinámico)
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
    showNotification("No se encontraron coincidencias. Intenta con m\u00e1s detalles.",
                     type = "warning")
  } else {
    showNotification(
      paste0("\u2728 ", nrow(resultado), " coincidencia(s) encontrada(s)"),
      type = "message"
    )
  }
  
  resultados_busqueda(resultado)
})

# Conteo de resultados
output$resultados_conteo <- renderText({
  datos <- resultados_busqueda()
  if (nrow(datos) == 0) "Resultados"
  else paste0("Resultados (", nrow(datos), ")")
})

# Renderizar tarjetas de resultados
output$resultados_ui <- renderUI({
  datos <- resultados_busqueda()
  
  if (nrow(datos) == 0) {
    return(div(
      class = "sin-resultados",
      tags$p("\U0001F50D No hay resultados. Realiza una b\u00fasqueda o a\u00fan no hay reportes registrados.")
    ))
  }
  
  etiquetas <- c(nino = "\U0001F9D2 Ni\u00f1o/a", adulto = "\U0001F9D1 Adulto",
                 adulto_mayor = "\U0001F9D3 Adulto Mayor", mascota = "\U0001F43E Mascota")
  
  salud_labels <- c(bien = "Aparentemente bien", herido_leve = "Herido/a leve",
                    herido_grave = "Herido/a grave", necesita_atencion = "Necesita atenci\u00f3n m\u00e9dica",
                    desconocido = "Desconocido")
  
  tarjetas <- lapply(seq_len(nrow(datos)), function(i) {
    r <- datos[i, ]
    
    # Fotos en miniatura (si hay)
    fotos_html <- NULL
    if ("fotos" %in% names(r) && nzchar(r$fotos %||% "")) {
      fotos_list <- strsplit(r$fotos, "\\|\\|\\|")[[1]]
      fotos_html <- div(
        class = "resultado-fotos",
        lapply(fotos_list[1:min(3, length(fotos_list))], function(src) {
          tags$img(src = src, class = "resultado-foto-thumb")
        })
      )
    }
    
    div(
      class = "resultado-card",
      div(
        class = "resultado-header",
        tags$span(class = "resultado-tipo", etiquetas[r$tipo] %||% r$tipo),
        tags$span(class = "resultado-fecha", r$fecha_reporte %||% "")
      ),
      if (nzchar(r$nombre %||% "")) tags$h5(r$nombre),
      if (nzchar(r$ubicacion %||% ""))
        tags$p(class = "resultado-ubicacion", paste("\U0001F4CD", r$ubicacion)),
      if (nzchar(r$descripcion %||% ""))
        tags$p(class = "resultado-desc", r$descripcion),
      if (nzchar(r$estado_salud %||% ""))
        tags$p(class = "resultado-salud",
               paste("\U0001FA7A Estado:", salud_labels[r$estado_salud] %||% r$estado_salud)),
      if (nzchar(r$contacto %||% ""))
        tags$p(class = "resultado-contacto", paste("\U0001F4DE", r$contacto)),
      fotos_html,
      # Score de IA si existe
      if ("score" %in% names(r) && !is.na(r$score))
        tags$span(class = "match-score",
                  paste0("\u2728 Match IA: ", r$score, "%")),
      if ("razon_ia" %in% names(r) && nzchar(r$razon_ia %||% ""))
        tags$p(class = "texto-gris", style = "font-size:0.8rem; margin-top:5px;",
               paste0("\U0001F4AC ", r$razon_ia))
    )
  })
  
  div(class = "resultados-lista", tarjetas)
})
