# ============ UI Principal — Reencuentros Venezuela ============

ui_main <- function() {
  page_fillable(
    theme = bs_theme(
      version = 5,
      bg = "#F5F5F5",
      fg = "#333333",
      primary = "#D32F2F",
      font_scale = 0.95,
      "enable-rounded" = TRUE
    ),
    
    useShinyjs(),
    # Skip link para accesibilidad
    tags$a(class = "skip-link", href = "#contenido-main", "Saltar al contenido"),
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("Reencuentro Tras Terremotos - Venezuela")
    ),
    
    # Header
    div(
      class = "header-app",
      div(class = "header-logo", "VE"),
      div(
        tags$h3("REENCUENTRO TRAS TERREMOTOS"),
        div(class = "subtitulo", "VENEZUELA")
      ),
      div(class = "canal-sos", "\U0001F7E2 CANAL SOS")
    ),
    
    # Contenido principal (paneles controlados por nav inferior)
    div(
      class = "contenido-principal",
      id = "contenido-main",
      
      conditionalPanel(
        "output.nav_actual_out == 'reportar'",
        ui_reportar()
      ),
      
      conditionalPanel(
        "output.nav_actual_out == 'buscar'",
        ui_buscar()
      ),
      
      conditionalPanel(
        "output.nav_actual_out == 'ayuda'",
        ui_ayuda()
      )
    ),
    
    # Navbar inferior
    div(
      class = "navbar-inferior",
      actionButton("nav_reportar",
        label = tagList(tags$span(class = "nav-icono", "\u26A0\uFE0F"), "Reportar SOS"),
        class = "nav-btn nav-btn-activo",
        `aria-label` = "Ir a Reportar SOS"
      ),
      actionButton("nav_buscar",
        label = tagList(tags$span(class = "nav-icono", "\U0001F465"), "Buscar"),
        class = "nav-btn",
        `aria-label` = "Ir a Buscar personas"
      ),
      actionButton("nav_ayuda",
        label = tagList(tags$span(class = "nav-icono", "\u2753"), "C\u00f3mo Ayudar"),
        class = "nav-btn",
        `aria-label` = "Ir a C\u00f3mo Ayudar"
      )
    )
  )
}
