# ============ Server: Cómo Ayudar ============

# Reunificar caso por código
observeEvent(input$btn_reunificar, {
  req(nzchar(input$codigo_reunificar))
  
  exito <- tryCatch(
    mg_reunificar(input$codigo_reunificar),
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
          tags$p(class = "texto-gris",
                 "\u00a1Nos alegra que se hayan reencontrado!")),
      footer = modalButton("Cerrar"),
      easyClose = TRUE
    ))
    updateTextInput(session, "codigo_reunificar", value = "")
  } else {
    showNotification("C\u00f3digo no encontrado o el caso ya fue reunificado.",
                     type = "error")
  }
})

# Estadísticas
output$stats_ui <- renderUI({
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
