# ============ UI: Buscar ============

ui_buscar <- function() {
  tagList(
    # Tabs de búsqueda
    div(
      class = "busqueda-tabs",
      actionButton("tab_tradicional", "\U0001F50D B\u00fasqueda Tradicional",
                   class = "btn-tab btn-tab-activo"),
      actionButton("tab_ia", "\u2728 Asistente de Match IA",
                   class = "btn-tab")
    ),
    
    # Panel de búsqueda tradicional
    div(
      id = "panel_tradicional",
      class = "paso-card",
      tags$h5("\U0001F50D B\u00daSQUEDA TRADICIONAL"),
      textInput("txt_busqueda", label = NULL,
                placeholder = "Buscar por nombre, ubicaci\u00f3n o descripci\u00f3n..."),
      selectInput("filtro_tipo", "Filtrar por tipo",
                  choices = c("Todos" = "todos",
                              "Ni\u00f1o / Ni\u00f1a" = "nino",
                              "Adulto" = "adulto",
                              "Adulto Mayor" = "adulto_mayor",
                              "Mascota / Animal" = "mascota")),
      actionButton("btn_buscar", "Buscar", class = "btn-buscar", width = "100%")
    ),
    
    # Panel Match IA
    uiOutput("panel_ia_ui"),
    
    # Resultados
    tags$h5(class = "resultados-titulo",
            textOutput("resultados_conteo", inline = TRUE)),
    uiOutput("resultados_ui")
  )
}
